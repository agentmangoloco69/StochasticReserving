"""
Reserve-risk emergence for a single line of business - one entry point, two methods.

The RISK EMERGENCE FACTOR is the share of total (ultimate) reserve risk that
emerges over the next calendar year:

    one-year risk        SD( total one-year CDR, calendar year 1 )
    -------------    =   ----------------------------------------
    ultimate risk        SD( total ultimate reserve )

Two ways to get it, behind a single `risk_emergence(...)` call:

  method="analytic"   (default) - closed-form Merz-Wuthrich (2008) one-year CDR
                      vs Mack (1993) ultimate. Fast, exact, validated against R
                      ChainLadder. Standard-deviation based. Engine: merz_wuthrich.py.
  method="simulation" - bootstrap the reserve distribution and run the
                      "Actuary-in-the-Box" CDR. Slower, but yields the full
                      predictive distribution (VaR/tails), and supports ODP /
                      Negative Binomial models. Engine: StochResFunctions.py.

Both return the same headline keys, so callers can switch methods freely.

Calculations are on an ANNUAL x ANNUAL triangle. For quarterly data pass
--periodicity quarterly (CLI) or call aggregate_to_annual() first; a partially
developed most-recent year is dropped so the result is a proper triangle.

Usage:
    python risk_emergence.py --triangle claims_triangle.csv                 # analytic (default)
    python risk_emergence.py --triangle claims_triangle.csv --sensitivity   # + outlier scan
    python risk_emergence.py --triangle paid.csv --method simulation --model ODPNonConstant
    python risk_emergence.py --triangle quarterly.csv --periodicity quarterly

    from risk_emergence import risk_emergence, to_triangle  # to_triangle re-exported
    res = risk_emergence(tri)                       # analytic
    res = risk_emergence(tri, method="simulation")  # bootstrap + CDR

    # Aggregate several LoBs into one portfolio emergence factor:
    from risk_emergence import portfolio_emergence
    port = portfolio_emergence([res_gl, res_auto, res_property], rho=0.25)
"""

from __future__ import annotations

import argparse
import warnings

import numpy as np
import pandas as pd

import StochResFunctions as srf
import merz_wuthrich as mw
from triangle_io import to_triangle  # re-export for convenience


# ---------------------------------------------------------------------------
# Quarterly -> annual aggregation
# ---------------------------------------------------------------------------

def aggregate_to_annual(incremental_quarterly: np.ndarray) -> np.ndarray:
    """
    Sum 4x4 quarter blocks of an incremental triangle into annual cells.

    An annual cell is only formed where all 16 constituent quarterly cells are
    observed, so the most-recent partially developed accident year naturally
    drops out. Returns the largest proper annual triangle (standard upper-left
    shape). The input must be square with a number of periods divisible by 4.
    """
    q = np.asarray(incremental_quarterly, dtype=float)
    n = q.shape[0]
    if q.shape[1] != n:
        raise ValueError("Quarterly triangle must be square.")
    if n % 4 != 0:
        raise ValueError(f"Quarterly triangle size ({n}) must be divisible by 4.")

    m = n // 4
    annual = np.full((m, m), np.nan)
    for I in range(m):
        for J in range(m):
            block = q[4 * I:4 * I + 4, 4 * J:4 * J + 4]
            if not np.isnan(block).any():
                annual[I, J] = block.sum()

    return _largest_standard_triangle(annual)


def _largest_standard_triangle(tri: np.ndarray) -> np.ndarray:
    """Return the largest top-left n x n block whose observed cells are exactly
    those with i + j <= n - 1 (a standard run-off triangle)."""
    M = tri.shape[0]
    for n in range(M, 0, -1):
        sub = tri[:n, :n]
        observed = ~np.isnan(sub)
        expected = np.fromfunction(lambda i, j: (i + j) <= (n - 1), (n, n))
        if np.array_equal(observed, expected):
            return sub
    raise ValueError(
        "Could not extract a standard annual triangle after aggregation. "
        "Check that the quarterly triangle is a standard square run-off triangle."
    )


def _as_array(x) -> np.ndarray:
    """Accept a labelled triangle DataFrame or a raw array; return a float array."""
    if hasattr(x, "to_numpy"):
        return x.to_numpy(dtype=float)
    return np.asarray(x, dtype=float)


# ---------------------------------------------------------------------------
# The two engines
# ---------------------------------------------------------------------------

def _emergence_analytic(triangle, sigma_method: str = "loglinear",
                        exclude_first_dev: bool = False) -> dict:
    res = mw.merz_wuthrich(triangle, sigma_method=sigma_method,
                           exclude_first_dev=exclude_first_dev)
    return {
        "method": "analytic",
        "emergence_factor": res["emergence_factor"],
        "total_oneyear_se": res["total_oneyear_se"],
        "total_ultimate_se": res["total_ultimate_se"],
        "total_reserve": res["total_reserve"],
        "emergence_pattern": res["emergence_pattern"],
        "detail": res["table"],
    }


def _emergence_simulation(triangle, model="Mack", iterations=10000, seed=101,
                          bootstrap_dist="Gamma", forecast_dist="Gamma",
                          var_p=0.995, exclude_first_dev=False) -> dict:
    incremental = _as_array(triangle)
    if exclude_first_dev:
        incremental = mw.drop_first_dev(incremental)
    cumulative = srf.Cumulatives(incremental)

    bstrap = srf.Run_Bootstrap(
        cumulative, method=model, Mask=None, iterations=iterations, seed=seed,
        BootstrapDist=bootstrap_dist, ForecastDist=forecast_dist, UserSqrtScale=None,
    )
    avg_ult_reserve = bstrap["Avg_TotalReserve"]
    sd_ult_reserve = bstrap["SD_TotalReserve"]

    # The final future year has too few valid CDRs for a mean/variance; numpy
    # warns on those empty slices. The figures are valid, so silence the noise.
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=RuntimeWarning)
        cdr = srf.CDR_Full_Picture(
            cumulative, bstrap["Complete_Cumulatives"], VAR_p=var_p, Mask=None,
        )
    avg_cdr = cdr["Avg_TotalCDR"]
    sd_cdr = cdr["SD_TotalCDR"]
    var_cdr = cdr["TotalCDR_VAR"]
    n_years = len(sd_cdr)
    ef = sd_cdr[0] / sd_ult_reserve if sd_ult_reserve else np.nan

    detail = pd.DataFrame({
        "future_year": np.arange(1, n_years + 1),
        "avg_CDR": avg_cdr,
        "SD_CDR": sd_cdr,
        f"VaR@{var_p:.1%}_CDR": var_cdr,
        "emergence_vs_ultimate": sd_cdr / sd_ult_reserve if sd_ult_reserve else np.nan,
    })
    emergence_pattern = pd.DataFrame({
        "future_year": np.arange(1, n_years + 1),
        "total_CDR_SE": sd_cdr,
        "ratio_to_ultimate": sd_cdr / sd_ult_reserve if sd_ult_reserve else np.nan,
    })
    return {
        "method": "simulation",
        "emergence_factor": ef,
        "total_oneyear_se": float(sd_cdr[0]),
        "total_ultimate_se": float(sd_ult_reserve),
        "total_reserve": float(avg_ult_reserve),
        "cov_ultimate_reserve": bstrap["CoV_TotalReserve"],
        "emergence_pattern": emergence_pattern,
        "detail": detail,
        "model": model,
        "iterations": iterations,
    }


# ---------------------------------------------------------------------------
# Unified entry point
# ---------------------------------------------------------------------------

def risk_emergence(triangle, method: str = "analytic", *,
                   sigma_method: str = "loglinear", exclude_first_dev: bool = False,
                   model: str = "Mack", iterations: int = 10000, seed: int = 101,
                   bootstrap_dist: str = "Gamma", forecast_dist: str = "Gamma",
                   var_p: float = 0.995) -> dict:
    """
    Risk emergence factor + pattern for one triangle.

    triangle : labelled incremental triangle DataFrame (from to_triangle) or array.
    method   : "analytic" (Merz-Wuthrich, default) or "simulation" (bootstrap + CDR).

    Analytic uses `sigma_method` (loglinear | mack). Simulation uses `model`
    (Mack | ODPConstant | ODPNonConstant | NegBinConstant | NegBinNonConstant),
    `iterations`, `seed`, `bootstrap_dist`, `forecast_dist`, `var_p`.

    Returns a dict with common keys: method, emergence_factor, total_oneyear_se,
    total_ultimate_se, total_reserve, emergence_pattern (per future year), detail.
    """
    if method == "analytic":
        return _emergence_analytic(triangle, sigma_method=sigma_method,
                                   exclude_first_dev=exclude_first_dev)
    if method == "simulation":
        return _emergence_simulation(triangle, model=model, iterations=iterations,
                                     seed=seed, bootstrap_dist=bootstrap_dist,
                                     forecast_dist=forecast_dist, var_p=var_p,
                                     exclude_first_dev=exclude_first_dev)
    raise ValueError(f"method must be 'analytic' or 'simulation', got '{method}'.")


# ---------------------------------------------------------------------------
# Portfolio aggregation across lines of business
# ---------------------------------------------------------------------------

def _aggregate_se(x: np.ndarray, rho: float) -> float:
    """SD of a sum under a single pairwise correlation rho:
    Var = rho*(sum x)^2 + (1-rho)*sum(x^2).  rho=0 independent, rho=1 comonotonic."""
    s = x.sum()
    return float(np.sqrt(rho * s * s + (1.0 - rho) * np.sum(x * x)))


def portfolio_emergence(oneyear_se, ultimate_se=None, rho: float = 0.25, names=None) -> dict:
    """
    Combine per-LoB risk emergence into one portfolio number.

    Emergence factors are ratios of standard deviations, so they cannot be
    averaged directly - the dollar SEs must be aggregated WITH a correlation
    assumption, then divided. This applies a single correlation `rho` between
    every pair of LoBs (same matrix for the one-year and ultimate horizons, so
    rho largely cancels in the ratio), and also reports the independence and
    full-correlation bookends.

    Inputs: either two array-likes (oneyear_se, ultimate_se), or a single list of
    risk_emergence() result dicts as the first argument (ultimate_se left None).

    Returns portfolio one-year SE, ultimate SE, emergence factor, the bookends,
    and a per-LoB table.
    """
    if ultimate_se is None:                      # a list of risk_emergence() dicts
        results = list(oneyear_se)
        if names is None:
            names = [r.get("name", f"LoB{i+1}") for i, r in enumerate(results)]
        oneyear_se = [r["total_oneyear_se"] for r in results]
        ultimate_se = [r["total_ultimate_se"] for r in results]

    o = np.asarray(oneyear_se, dtype=float)
    u = np.asarray(ultimate_se, dtype=float)
    if names is None:
        names = [f"LoB{i+1}" for i in range(len(o))]

    O = _aggregate_se(o, rho)
    U = _aggregate_se(u, rho)
    O_ind, U_ind = _aggregate_se(o, 0.0), _aggregate_se(u, 0.0)
    O_full, U_full = _aggregate_se(o, 1.0), _aggregate_se(u, 1.0)

    table = pd.DataFrame({
        "LoB": names,
        "oneyear_SE": o,
        "ultimate_SE": u,
        "emergence_factor": o / u,
    })

    return {
        "rho": rho,
        "portfolio_oneyear_se": O,
        "portfolio_ultimate_se": U,
        "emergence_factor": O / U,
        "emergence_factor_independent": O_ind / U_ind,
        "emergence_factor_fully_correlated": O_full / U_full,
        "diversification_benefit_oneyear": 1.0 - O / o.sum(),   # vs simply adding SEs
        "table": table,
    }


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Reserve-risk emergence from a claims triangle.")
    parser.add_argument("--triangle", required=True, help="Incremental triangle CSV.")
    parser.add_argument("--method", default="analytic", choices=["analytic", "simulation"],
                        help="analytic (Merz-Wuthrich, default) or simulation (bootstrap + CDR).")
    parser.add_argument("--periodicity", default="annual", choices=["annual", "quarterly"],
                        help="If 'quarterly', aggregate 4x4 blocks to annual first.")
    parser.add_argument("--exclude-first-dev", action="store_true",
                        help="Drop the first development column and the immature most-recent "
                             "accident year (useful for long-tail lines like GL).")
    # analytic options
    parser.add_argument("--sigma-method", default="loglinear", choices=["loglinear", "mack"],
                        help="[analytic] tail-sigma extrapolation.")
    parser.add_argument("--sensitivity", action="store_true",
                        help="[analytic] also run leave-one-out outlier sensitivity.")
    parser.add_argument("--top", type=int, default=10, help="[analytic] sensitivity rows to show.")
    # simulation options
    parser.add_argument("--model", default="Mack",
                        help="[simulation] Mack | ODPConstant | ODPNonConstant | NegBin*")
    parser.add_argument("--iterations", type=int, default=10000, help="[simulation]")
    parser.add_argument("--seed", type=int, default=101, help="[simulation]")
    parser.add_argument("--bootstrap-dist", default="Gamma", help="[simulation] estimation-error.")
    parser.add_argument("--forecast-dist", default="Gamma", help="[simulation] process-error.")
    parser.add_argument("--var-p", type=float, default=0.995, help="[simulation] VaR level.")
    parser.add_argument("--out", help="Optional CSV path for the emergence pattern.")
    args = parser.parse_args()

    incremental = pd.read_csv(args.triangle, index_col=0).to_numpy(dtype=float)
    if args.periodicity == "quarterly":
        incremental = aggregate_to_annual(incremental)
        print(f"Aggregated quarterly -> annual {incremental.shape[0]}x{incremental.shape[1]} triangle.\n")

    res = risk_emergence(
        incremental, method=args.method, sigma_method=args.sigma_method,
        exclude_first_dev=args.exclude_first_dev,
        model=args.model, iterations=args.iterations, seed=args.seed,
        bootstrap_dist=args.bootstrap_dist, forecast_dist=args.forecast_dist, var_p=args.var_p,
    )

    if res["method"] == "simulation":
        print(f"Method: simulation ({res['model']}, {res['iterations']:,} iterations)")
    else:
        print(f"Method: analytic (Merz-Wuthrich, sigma={args.sigma_method})")
    print(f"Total reserve      : {res['total_reserve']:,.0f}")
    print(f"Ultimate risk  SE  : {res['total_ultimate_se']:,.0f}")
    print(f"One-year risk  SE  : {res['total_oneyear_se']:,.0f}")
    print(f"\n>>> RISK EMERGENCE FACTOR (one-year SE / ultimate SE): "
          f"{res['emergence_factor']:.1%}\n")
    print("Emergence pattern by future calendar year:")
    with pd.option_context("display.float_format", lambda x: f"{x:,.3f}"):
        print(res["emergence_pattern"].to_string(index=False))

    if args.sensitivity and res["method"] == "analytic":
        print("\n" + "=" * 60)
        print("Leave-one-out sensitivity (largest impact on one-year SE):")
        sens = mw.sensitivity_oneyear(incremental, sigma_method=args.sigma_method,
                                      exclude_first_dev=args.exclude_first_dev)
        money = lambda x: f"{x:,.0f}"
        formatters = {"d_oneyear_SE": money, "d_ultimate_SE": money, "d_reserve": money,
                      "d_emergence_factor": lambda x: f"{x:+.3f}"}
        print(sens.head(args.top).to_string(index=False, formatters=formatters))
    elif args.sensitivity:
        print("\n(--sensitivity is only available for method=analytic)")

    if args.out:
        res["emergence_pattern"].to_csv(args.out, index=False)
        print(f"\nWrote emergence pattern -> {args.out}")


if __name__ == "__main__":
    main()
