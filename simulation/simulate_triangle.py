"""
Simulate quarterly claims development triangles for general insurance.

Data is always simulated on an accident-quarter x development-quarter grid, so it
can be aggregated to annual (or annual accident year x quarterly development)
afterwards. Each cell is generated under an over-dispersed Poisson (ODP) noise
model, matching the assumptions the reserving scripts in ../Reserve_Risk
bootstrap.

Three usage modes:
  1. CLI:        python simulate_triangle.py --config configs/gl_canada.yaml
  2. Python API: from simulate_triangle import simulate
                 triangles = simulate(lob="gl", region="canada", n_simulations=5)
  3. Wild mode:  simulate(lob="wild")   # LOB-agnostic, realistic but unknown line

Earned premium is computed internally to anchor the loss scale and written to a
side file. Full earned-premium treatment (on-levelling, its own noise/trend) is
tracked in GitHub issue #4. Seasonality is deferred to issue #3.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd

try:
    from scrape_benchmarks import load_benchmarks
except ImportError:  # when imported as part of a package
    from .scrape_benchmarks import load_benchmarks

OUTPUTS_DIR = Path(__file__).parent / "outputs"

# ---------------------------------------------------------------------------
# Defaults. Anything the user does not specify falls back to these.
# Trends are ANNUAL rates; they are applied with fractional-year exponents
# across accident quarters.
# ---------------------------------------------------------------------------

DEFAULTS = {
    "lob": "gl",                  # "property" | "commercial_auto" | "gl" | "wild"
    "region": "canada",
    "seed": 42,                   # set to None for non-reproducible runs
    "accident_years": 20,         # -> accident_years * 4 accident quarters
    "development_quarters": None,  # None -> equal to number of accident quarters (square)
    "base_earned_premium": 100_000_000.0,  # annual EP for the MOST RECENT accident year
    "base_loss_ratio": None,      # None -> use benchmark loss_ratio
    "noise_dist": "lognormal",    # cell process noise: "lognormal" (default) or "gamma"
    "severity_trend": 0.04,       # annual loss severity trend
    "frequency_trend": -0.01,     # annual claim frequency trend
    "premium_trend": 0.03,        # annual earned-premium (rate x exposure) trend
    "elr_overrides": {},          # {accident_year_index: loss_ratio}, year 0 = oldest
    "phi_factor": None,           # None -> use benchmark phi_factor
    "n_simulations": 1,
    "include_future": False,      # True -> also write the full (un-masked) "truth" triangle
}


# ---------------------------------------------------------------------------
# Benchmark resolution
# ---------------------------------------------------------------------------

def _wild_benchmark(rng: np.random.Generator) -> dict:
    """Generate a plausible but LOB-agnostic benchmark for an unknown line."""
    n_factors = int(rng.integers(4, 13))           # tail length: 4-12 annual factors
    first_excess = rng.uniform(0.25, 0.90)          # first age-to-age factor 1.25-1.90
    decay = rng.uniform(0.35, 0.65)                 # geometric decay of development
    excess = first_excess
    ldfs = []
    for _ in range(n_factors):
        ldfs.append(round(1.0 + excess, 4))
        excess *= decay
    tail = round(1.0 + max(excess * decay, 0.0005), 4)
    return {
        "loss_ratio": float(rng.uniform(0.45, 0.85)),
        "annual_ldfs": ldfs,
        "tail_factor": tail,
        "phi_factor": float(rng.uniform(0.10, 0.45)),
        "description": "Wild mode: randomly generated pattern for an unknown LOB.",
    }


def resolve_benchmark(cfg: dict, rng: np.random.Generator) -> dict:
    """Return the benchmark dict for the configured LOB, honouring user overrides."""
    if cfg["lob"] == "wild":
        bench = _wild_benchmark(rng)
    else:
        all_benchmarks = load_benchmarks(cfg["region"])
        if cfg["lob"] not in all_benchmarks:
            available = [k for k in all_benchmarks if not k.startswith("_")]
            raise ValueError(
                f"LOB '{cfg['lob']}' not in benchmarks for region "
                f"'{cfg['region']}'. Available: {available + ['wild']}. "
                "See GitHub issue #1 for the LOB expansion roadmap."
            )
        bench = dict(all_benchmarks[cfg["lob"]])

    # User overrides of scale/dispersion
    if cfg["base_loss_ratio"] is not None:
        bench["loss_ratio"] = float(cfg["base_loss_ratio"])
    if cfg["phi_factor"] is not None:
        bench["phi_factor"] = float(cfg["phi_factor"])
    return bench


# ---------------------------------------------------------------------------
# Development pattern: annual LDFs -> quarterly incremental proportions
# ---------------------------------------------------------------------------

def quarterly_incremental_pattern(annual_ldfs, tail_factor, n_dev_quarters) -> np.ndarray:
    """
    Convert annual age-to-age factors into a quarterly incremental development
    pattern that sums to 1.0 over n_dev_quarters.

    Method: build the cumulative %-developed curve at the known annual ages
    (12m, 24m, ...), anchor it at (age 0 -> 0%) and (one year past the last
    factor -> 100%), then linearly interpolate the cumulative curve to every
    quarter and difference it. Linear interpolation gives flat quarterly
    increments within each development year -- a known v1 simplification
    (smoother sub-annual interpolation is a future improvement).
    """
    annual_ldfs = list(annual_ldfs)
    n = len(annual_ldfs)

    # cumulative-to-ultimate factor at each annual age 12m, 24m, ..., 12*n m
    cum_to_ult = []
    for i in range(n):
        f = tail_factor
        for j in range(i, n):
            f *= annual_ldfs[j]
        cum_to_ult.append(f)

    anchor_ages = [0] + [4 * (i + 1) for i in range(n)] + [4 * (n + 1)]
    anchor_pct = [0.0] + [1.0 / c for c in cum_to_ult] + [1.0]

    quarters = np.arange(1, n_dev_quarters + 1)
    cum_pct = np.interp(quarters, anchor_ages, anchor_pct, left=0.0, right=1.0)
    cum_pct = np.maximum.accumulate(np.clip(cum_pct, 0.0, 1.0))  # enforce monotonic

    incr = np.diff(np.concatenate([[0.0], cum_pct]))
    total = incr.sum()
    if total > 0:
        incr = incr / total  # normalise so the pattern fully develops in-window
    return incr


# ---------------------------------------------------------------------------
# Loss-ratio / earned-premium term structure by accident quarter
# ---------------------------------------------------------------------------

def accident_quarter_terms(cfg: dict, base_loss_ratio: float):
    """
    Compute earned premium and expected loss ratio for each accident quarter.

    The most recent accident year sits at the benchmark (current) cost level.
    Older accident quarters are de-trended: premium by `premium_trend`, losses by
    the combined severity x frequency trend, so the loss ratio trends at the ratio
    of the two. Per-accident-year overrides replace the loss ratio outright.
    """
    Y = int(cfg["accident_years"])
    Q = Y * 4
    quarterly_ep_base = cfg["base_earned_premium"] / 4.0

    loss_trend = (1 + cfg["severity_trend"]) * (1 + cfg["frequency_trend"]) - 1
    lr_trend = (1 + loss_trend) / (1 + cfg["premium_trend"]) - 1

    overrides = {int(k): float(v) for k, v in (cfg["elr_overrides"] or {}).items()}

    earned_premium = np.empty(Q)
    loss_ratio = np.empty(Q)
    for aq in range(Q):
        years_before_present = (Q - 1 - aq) / 4.0
        earned_premium[aq] = quarterly_ep_base * (1 + cfg["premium_trend"]) ** (-years_before_present)
        accident_year = aq // 4  # 0 = oldest year
        if accident_year in overrides:
            loss_ratio[aq] = overrides[accident_year]
        else:
            loss_ratio[aq] = base_loss_ratio * (1 + lr_trend) ** (-years_before_present)

    return earned_premium, loss_ratio


# ---------------------------------------------------------------------------
# Core single-triangle simulation
# ---------------------------------------------------------------------------

def simulate_one(cfg: dict, bench: dict, rng: np.random.Generator):
    """
    Return (incremental_observed, incremental_full, earned_premium).

    incremental_observed is the masked upper-left triangle (future cells NaN).
    incremental_full is the complete rectangle (the "truth", for validation).
    Rows are accident quarters (0 = oldest), columns development quarters (0-based).
    """
    Y = int(cfg["accident_years"])
    Q = Y * 4
    D = int(cfg["development_quarters"]) if cfg["development_quarters"] else Q

    pattern = quarterly_incremental_pattern(bench["annual_ldfs"], bench["tail_factor"], D)
    earned_premium, loss_ratio = accident_quarter_terms(cfg, bench["loss_ratio"])
    expected_ultimate = earned_premium * loss_ratio  # per accident quarter

    # Expected incremental losses: outer product of ultimate and development pattern
    means = np.outer(expected_ultimate, pattern)  # shape (Q, D)

    # Process noise. Variance is set proportional to the mean (ODP-style); to keep
    # phi_factor scale-free, anchor the absolute dispersion to the average cell size,
    # so phi_factor behaves like a relative-dispersion knob (CoV of the average cell
    # ~ sqrt(phi_factor)). The cell distribution is lognormal by default (heavier
    # right tail, always positive) or gamma; both are matched to the same mean and
    # variance. This is the data-generating choice and is independent of the
    # bootstrap used later by the reserving code.
    noise = cfg["noise_dist"]
    if noise not in ("lognormal", "gamma"):
        raise ValueError(f"noise_dist must be 'lognormal' or 'gamma', got '{noise}'.")
    avg_mean = means[means > 0].mean()
    phi_abs = bench["phi_factor"] * avg_mean

    incremental_full = np.zeros((Q, D))
    for aq in range(Q):
        for dq in range(D):
            m = means[aq, dq]
            if m <= 0:
                continue
            if noise == "gamma":
                # Gamma(shape=m/phi, scale=phi) -> mean m, var phi*m
                incremental_full[aq, dq] = rng.gamma(m / phi_abs, phi_abs)
            else:
                # Lognormal matched to mean m and variance phi_abs*m
                var = phi_abs * m
                sigma2 = np.log1p(var / m**2)
                mu = np.log(m) - 0.5 * sigma2
                incremental_full[aq, dq] = rng.lognormal(mu, np.sqrt(sigma2))

    # Mask future cells: cell observed if on/above the latest diagonal and within D
    incremental_observed = incremental_full.copy()
    for aq in range(Q):
        for dq in range(D):
            if aq + dq > Q - 1:
                incremental_observed[aq, dq] = np.nan

    return incremental_observed, incremental_full, earned_premium


# ---------------------------------------------------------------------------
# Formatting / output
# ---------------------------------------------------------------------------

def _triangle_to_frame(arr: np.ndarray) -> pd.DataFrame:
    """Match the CSV style of ../Reserve_Risk/claims_triangle.csv."""
    Q, D = arr.shape
    df = pd.DataFrame(
        arr,
        index=range(1, Q + 1),
        columns=range(1, D + 1),
    )
    df.index.name = "Origin/Development"
    return df


def simulate(config: dict | None = None, **kwargs):
    """
    Run one or more simulations.

    Pass a config dict and/or keyword overrides (kwargs win). Returns a single
    DataFrame when n_simulations == 1, otherwise a list of DataFrames. Always
    writes outputs to outputs/.
    """
    cfg = dict(DEFAULTS)
    if config:
        cfg.update(config)
    cfg.update(kwargs)

    master_seed = cfg["seed"]
    n = int(cfg["n_simulations"])
    tag = f"{cfg['lob']}_{cfg['region']}"
    OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)

    frames = []
    batch_rows = []
    earned_premium = None
    for sim_id in range(n):
        # Each simulation gets its own child seed for reproducible batches.
        seed = None if master_seed is None else master_seed + sim_id
        rng = np.random.default_rng(seed)
        bench = resolve_benchmark(cfg, rng)

        observed, full, earned_premium = simulate_one(cfg, bench, rng)
        obs_frame = _triangle_to_frame(observed)
        frames.append(obs_frame)

        if n == 1:
            obs_frame.to_csv(OUTPUTS_DIR / f"{tag}_triangle.csv")
            if cfg["include_future"]:
                _triangle_to_frame(full).to_csv(OUTPUTS_DIR / f"{tag}_triangle_truth.csv")
        else:
            tagged = obs_frame.reset_index()
            tagged.insert(0, "sim_id", sim_id)
            batch_rows.append(tagged)

    # Earned premium (same across sims for given config) -> side file (issue #4)
    Q = int(cfg["accident_years"]) * 4
    ep_frame = pd.DataFrame(
        {"accident_quarter": range(1, Q + 1), "earned_premium": earned_premium}
    )
    ep_frame.to_csv(OUTPUTS_DIR / f"{tag}_earned_premium.csv", index=False)

    if n > 1:
        pd.concat(batch_rows, ignore_index=True).to_csv(
            OUTPUTS_DIR / f"{tag}_triangles_batch.csv", index=False
        )
        print(f"Wrote {n} triangles -> outputs/{tag}_triangles_batch.csv")
        return frames

    print(f"Wrote triangle -> outputs/{tag}_triangle.csv")
    return frames[0]


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _load_yaml_config(path: str) -> dict:
    import yaml  # imported lazily so the API works without pyyaml
    with open(path) as f:
        return yaml.safe_load(f) or {}


def main():
    parser = argparse.ArgumentParser(description="Simulate quarterly claims triangles.")
    parser.add_argument("--config", help="Path to a YAML config file.")
    parser.add_argument("--lob", help="property | commercial_auto | gl | wild")
    parser.add_argument("--region", help="Region (v1: canada only).")
    parser.add_argument("--n", type=int, help="Number of simulations.")
    parser.add_argument("--seed", type=int, help="Random seed (omit for config default).")
    parser.add_argument("--include-future", action="store_true",
                        help="Also write the full un-masked 'truth' triangle.")
    args = parser.parse_args()

    cfg = {}
    if args.config:
        cfg.update(_load_yaml_config(args.config))
    if args.lob:
        cfg["lob"] = args.lob
    if args.region:
        cfg["region"] = args.region
    if args.n is not None:
        cfg["n_simulations"] = args.n
    if args.seed is not None:
        cfg["seed"] = args.seed
    if args.include_future:
        cfg["include_future"] = True

    simulate(cfg)


if __name__ == "__main__":
    main()
