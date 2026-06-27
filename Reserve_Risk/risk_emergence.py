"""
Compute the reserve-risk emergence pattern for a single line of business.

Feed in an incremental claims triangle and this:
  1. bootstraps the reserve distribution (lifetime / ultimate view),
  2. runs the "Actuary-in-the-Box" CDR for each future calendar year
     (the sequence of one-year views), and
  3. reports the RISK EMERGENCE FACTOR -- the share of total (ultimate) reserve
     risk that emerges over the next calendar year -- plus the full year-by-year
     emergence pattern.

The risk emergence factor is:

    one-year risk        SD( total one-year CDR, calendar year 1 )
    -------------    =   ----------------------------------------
    ultimate risk        SD( total ultimate reserve )

Calculations are on an ANNUAL x ANNUAL triangle. If you have quarterly data,
pass --periodicity quarterly and it is aggregated to annual first (a partially
developed most-recent year is dropped so the result is a proper triangle).

Usage:
    python risk_emergence.py --triangle claims_triangle.csv
    python risk_emergence.py --triangle my_quarterly.csv --periodicity quarterly
    python risk_emergence.py --triangle paid.csv --method ODPNonConstant --out emergence.csv
"""

from __future__ import annotations

import argparse
import warnings

import numpy as np
import pandas as pd

import StochResFunctions as srf


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
    n = incremental_quarterly.shape[0]
    if incremental_quarterly.shape[1] != n:
        raise ValueError("Quarterly triangle must be square.")
    if n % 4 != 0:
        raise ValueError(f"Quarterly triangle size ({n}) must be divisible by 4.")

    m = n // 4
    annual = np.full((m, m), np.nan)
    for I in range(m):
        for J in range(m):
            block = incremental_quarterly[4 * I:4 * I + 4, 4 * J:4 * J + 4]
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


# ---------------------------------------------------------------------------
# Risk emergence
# ---------------------------------------------------------------------------

def risk_emergence(
    incremental_triangle: np.ndarray,
    method: str = "Mack",
    iterations: int = 10000,
    seed: int = 101,
    bootstrap_dist: str = "Gamma",
    forecast_dist: str = "Gamma",
    var_p: float = 0.995,
) -> dict:
    """
    Bootstrap the triangle, run the full-picture CDR, and compute the emergence
    factor and pattern. Returns a dict with a summary DataFrame and headline figures.

    `incremental_triangle` may be a labelled triangle DataFrame or a raw array.
    """
    if hasattr(incremental_triangle, "to_numpy"):
        incremental_triangle = incremental_triangle.to_numpy(dtype=float)
    cumulative = srf.Cumulatives(incremental_triangle)

    bstrap = srf.Run_Bootstrap(
        cumulative, method=method, Mask=None, iterations=iterations, seed=seed,
        BootstrapDist=bootstrap_dist, ForecastDist=forecast_dist, UserSqrtScale=None,
    )
    avg_ult_reserve = bstrap["Avg_TotalReserve"]
    sd_ult_reserve = bstrap["SD_TotalReserve"]

    # The final future year has too few valid CDRs for a mean/variance; numpy warns
    # on those empty slices. The figures are valid, so silence the noise.
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=RuntimeWarning)
        cdr = srf.CDR_Full_Picture(
            cumulative, bstrap["Complete_Cumulatives"], VAR_p=var_p, Mask=None,
        )
    avg_cdr = cdr["Avg_TotalCDR"]        # by future calendar year (year 1 first)
    sd_cdr = cdr["SD_TotalCDR"]
    var_cdr = cdr["TotalCDR_VAR"]

    n_years = len(sd_cdr)
    emergence_factor = sd_cdr[0] / sd_ult_reserve if sd_ult_reserve else np.nan

    table = pd.DataFrame({
        "future_year": np.arange(1, n_years + 1),
        "avg_CDR": avg_cdr,
        "SD_CDR": sd_cdr,
        f"VaR@{var_p:.1%}_CDR": var_cdr,
        "emergence_vs_ultimate": sd_cdr / sd_ult_reserve if sd_ult_reserve else np.nan,
    })

    return {
        "table": table,
        "avg_ultimate_reserve": avg_ult_reserve,
        "sd_ultimate_reserve": sd_ult_reserve,
        "cov_ultimate_reserve": bstrap["CoV_TotalReserve"],
        "emergence_factor": emergence_factor,
        "method": method,
        "iterations": iterations,
    }


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Reserve-risk emergence from a claims triangle.")
    parser.add_argument("--triangle", required=True, help="Incremental triangle CSV.")
    parser.add_argument("--method", default="Mack",
                        help="Mack | ODPConstant | ODPNonConstant | NegBinConstant | NegBinNonConstant")
    parser.add_argument("--periodicity", default="annual", choices=["annual", "quarterly"],
                        help="If 'quarterly', aggregate 4x4 blocks to annual first.")
    parser.add_argument("--iterations", type=int, default=10000)
    parser.add_argument("--seed", type=int, default=101)
    parser.add_argument("--bootstrap-dist", default="Gamma",
                        help="Estimation-error pseudo-data: NonParametric | Gamma | Lognormal")
    parser.add_argument("--forecast-dist", default="Gamma",
                        help="Process-error forecast: NonParametric | Gamma | Lognormal")
    parser.add_argument("--var-p", type=float, default=0.995)
    parser.add_argument("--out", help="Optional path to write the emergence table CSV.")
    args = parser.parse_args()

    incremental = pd.read_csv(args.triangle, index_col=0).to_numpy(dtype=float)
    if args.periodicity == "quarterly":
        incremental = aggregate_to_annual(incremental)
        print(f"Aggregated quarterly -> annual {incremental.shape[0]}x{incremental.shape[1]} triangle.\n")

    result = risk_emergence(
        incremental, method=args.method, iterations=args.iterations, seed=args.seed,
        bootstrap_dist=args.bootstrap_dist, forecast_dist=args.forecast_dist, var_p=args.var_p,
    )

    print(f"Method: {result['method']}   Iterations: {result['iterations']:,}")
    print(f"Ultimate reserve   : mean {result['avg_ultimate_reserve']:,.0f}   "
          f"SD {result['sd_ultimate_reserve']:,.0f}   "
          f"CoV {result['cov_ultimate_reserve']:.1%}")
    print(f"\n>>> RISK EMERGENCE FACTOR (year 1 one-year risk / ultimate risk): "
          f"{result['emergence_factor']:.1%}\n")
    with pd.option_context("display.float_format", lambda x: f"{x:,.3f}"):
        print(result["table"].to_string(index=False))

    if args.out:
        result["table"].to_csv(args.out, index=False)
        print(f"\nWrote emergence table -> {args.out}")


if __name__ == "__main__":
    main()
