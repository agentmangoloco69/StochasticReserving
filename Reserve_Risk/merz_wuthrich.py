"""
Merz-Wuthrich (2008) one-year claims development result (CDR) - ANALYTIC.

Closed-form (non-simulated) prediction uncertainty of the one-year reserve risk
under the distribution-free chain ladder (Mack) model, from:

    Merz & Wuthrich (2008), "Modelling the Claims Development Result for
    Solvency Purposes", CAS E-Forum / Variance.

This gives, with no Monte Carlo:
  * the ULTIMATE (lifetime) reserve uncertainty  -> Mack (1993) standard error,
  * the ONE-YEAR (next calendar year) CDR uncertainty -> Merz-Wuthrich S.E.,
  * the full year-by-year emergence pattern (one CDR per future calendar year),
  * the RISK EMERGENCE FACTOR = one-year S.E. / ultimate S.E.

The implementation is a faithful port of R's ChainLadder::CDR (function
CL_MSEPs) and is validated to match it on the Taylor-Ashe / GenIns triangle.

Assumptions (these are Mack's model assumptions - read before trusting output):
  1. Chain ladder is the right model: E[C_{i,j+1} | history] = f_j * C_{i,j}.
     Development factors are stable across accident years (one f_j per column).
  2. Accident years are independent.
  3. Var(C_{i,j+1} | C_{i,j}) = sigma_j^2 * C_{i,j}  (variance proportional to
     the cumulative, i.e. volume-weighted / alpha = 1). This is the ONLY case
     the Merz-Wuthrich formulae cover.
  4. No tail beyond the triangle (square triangle; ultimate at the last column).
A consequence of (1) is that a single unusual cell feeds BOTH the factor f_j and
the variance sigma_j^2 for its column, so outliers can move the answer a lot -
use sensitivity_oneyear() to see which ones.

Usage:
    python merz_wuthrich.py --triangle claims_triangle.csv
    python merz_wuthrich.py --triangle claims_triangle.csv --sensitivity
    # Python API:
    from merz_wuthrich import merz_wuthrich
    res = merz_wuthrich(incremental_array)
"""

from __future__ import annotations

import argparse

import numpy as np
import pandas as pd


# ---------------------------------------------------------------------------
# Triangle / chain-ladder building blocks
# ---------------------------------------------------------------------------

def _as_array(x) -> np.ndarray:
    """Accept a labelled triangle DataFrame or a raw array; return a float array."""
    if hasattr(x, "to_numpy"):
        return x.to_numpy(dtype=float)
    return np.asarray(x, dtype=float)


def drop_first_dev(incremental) -> np.ndarray:
    """
    Remove the first development column from an incremental triangle, for lines
    where the first column (and the one-point most-recent accident year) would
    skew the result - e.g. long-tail GL.

    The first development period's DOLLARS are kept (folded into the new first
    column = DP1 + DP2 cumulative); only the first development *step* (factor f1
    and its variance) and the immature most-recent accident year are removed.
    Returns a smaller (n-1 x n-1) incremental triangle.
    """
    a = _as_array(incremental)
    n = a.shape[0]
    if n < 3:
        raise ValueError("exclude_first_dev needs at least a 3x3 triangle.")
    red = a[:-1, 1:].astype(float).copy()      # drop last accident year and DP1 column
    red[:, 0] = red[:, 0] + a[:-1, 0]          # fold DP1 dollars into the new first column
    return red


def _cumulate(incremental: np.ndarray) -> np.ndarray:
    """Row-wise cumulative sum, preserving NaN (unobserved) cells."""
    cum = np.full_like(incremental, np.nan, dtype=float)
    for i in range(incremental.shape[0]):
        running = 0.0
        for j in range(incremental.shape[1]):
            v = incremental[i, j]
            if np.isnan(v):
                break
            running += v
            cum[i, j] = running
    return cum


def _chain_ladder(cum: np.ndarray, weight_mask: np.ndarray | None = None,
                  sigma_method: str = "loglinear") -> dict:
    """
    Volume-weighted chain ladder under Mack's assumptions.

    Returns development factors f[j], variance params sigma2[j] (j = 0..n-2,
    unestimable ones extrapolated), the completed full triangle, the factor
    denominator sums S[j], the newest-diagonal weights alpha[j], and ultimates.

    sigma_method controls how an unestimable (single-point) sigma^2 is filled:
      "loglinear" - log-linear regression of sigma on development period; this is
                    R ChainLadder's default and reproduces its CDR exactly.
      "mack"      - Mack's min rule; reproduces the classic Mack S.E. and matches
                    this repo's own bootstrap/simulation.

    weight_mask (n x n, 1=use / 0=exclude) excludes individual age-to-age ratios
    at position (i, j) = the ratio C[i,j] -> C[i,j+1]; used for leave-one-out
    sensitivity. None means use every observed ratio.
    """
    n = cum.shape[1]
    observed = ~np.isnan(cum)
    if weight_mask is None:
        weight_mask = np.ones((n, n))

    f = np.ones(n - 1)
    sigma2 = np.full(n - 1, np.nan)
    S = np.zeros(n - 1)               # denominator of f_j (sum of C[i,j] used)

    for j in range(n - 1):
        num = den = 0.0
        rows = []
        for i in range(cum.shape[0]):
            if observed[i, j] and observed[i, j + 1] and weight_mask[i, j] == 1:
                num += cum[i, j + 1]
                den += cum[i, j]
                rows.append(i)
        f[j] = num / den if den > 0 else 1.0
        S[j] = den
        m = len(rows)
        if m >= 2:
            ss = sum(cum[i, j] * (cum[i, j + 1] / cum[i, j] - f[j]) ** 2 for i in rows)
            sigma2[j] = ss / (m - 1)

    # Extrapolate any unestimable sigma^2 (typically just the last column).
    est = [j for j in range(n - 1) if not np.isnan(sigma2[j]) and sigma2[j] > 0]
    missing = [j for j in range(n - 1) if np.isnan(sigma2[j])]
    if missing and sigma_method == "loglinear" and len(est) >= 2:
        b, a = np.polyfit(np.array(est) + 1, np.log(np.sqrt([sigma2[j] for j in est])), 1)
        for j in missing:
            sigma2[j] = float(np.exp(a + b * (j + 1)) ** 2)
    else:
        for j in sorted(missing):       # Mack's min rule (and loglinear fallback)
            if j >= 2 and sigma2[j - 1] > 0 and sigma2[j - 2] > 0:
                sigma2[j] = min(sigma2[j - 1] ** 2 / sigma2[j - 2],
                                sigma2[j - 1], sigma2[j - 2])
            elif j >= 1 and not np.isnan(sigma2[j - 1]):
                sigma2[j] = sigma2[j - 1]
            else:
                sigma2[j] = 0.0

    # Complete the triangle to ultimate using f.
    full = cum.copy()
    for i in range(cum.shape[0]):
        last_obs = np.where(observed[i])[0]
        start = last_obs[-1] if len(last_obs) else -1
        for j in range(start + 1, n):
            full[i, j] = full[i, j - 1] * f[j - 1]

    # Newest-diagonal weight alpha[j] = C_{diag,j} / (full observed column sum).
    colsum = np.zeros(n - 1)
    latest = np.zeros(n - 1)
    for j in range(n - 1):
        col_rows = [i for i in range(cum.shape[0]) if observed[i, j]]
        colsum[j] = sum(cum[i, j] for i in col_rows)
        diag_i = cum.shape[0] - 1 - j           # accident year on the latest diagonal in col j
        latest[j] = cum[diag_i, j] if 0 <= diag_i < cum.shape[0] else 0.0
    alpha = np.where(colsum > 0, latest / colsum, 0.0)

    return {"f": f, "sigma2": sigma2, "full": full, "S": S, "alpha": alpha,
            "ratio": sigma2 / f**2, "ultimate": full[:, -1]}


# ---------------------------------------------------------------------------
# Merz-Wuthrich MSEPs (port of ChainLadder::CL_MSEPs)
# ---------------------------------------------------------------------------

def _cl_mseps(clq: dict, I0: int, J0: int):
    """
    Return:
      reserve[i]            IBNR per accident year (i = 0..I0-1)
      cdr_msep[i, s]        MSEP of the one-year CDR for accident year i in
                            future calendar year s+1 (s = 0..J0-2)
      total_cdr_msep[s]     MSEP of the total one-year CDR in future year s+1
    The ultimate (Mack) MSEP is the sum over s of the CDR MSEPs.
    """
    full = clq["full"]
    ratio = clq["ratio"]
    alpha = clq["alpha"]
    S = clq["S"]
    ult = clq["ultimate"]

    n_steps = J0 - 1
    active = range(I0 - J0 + 1, I0)        # accident years still developing (skip fully run-off)

    reserve = np.zeros(I0)
    res3 = np.zeros((I0, n_steps))         # process variance
    res5 = np.zeros((I0, n_steps))         # estimation variance per ultimate^2
    res2 = np.zeros((I0, n_steps))         # total per-AY CDR MSEP

    for ip in active:
        L = I0 - 1 - ip                    # latest observed development column for AY ip
        reserve[ip] = ult[ip] - full[ip, L]
        for s in range(0, J0 - 1 - L):     # future calendar years for this AY
            cs = L + s                     # development column developing in year s+1
            res3[ip, s] = ult[ip] ** 2 * ratio[cs] / full[ip, cs]

            # base term
            y = 1.0
            for m in range(L + 1, L + s + 1):
                y *= (1 - alpha[m])
            est = y * ratio[cs] / S[cs]

            # cross-development terms
            for c2 in range(cs + 1, J0 - 1):
                y2 = 1.0
                for m in range(c2 - s + 1, c2 + 1):
                    y2 *= (1 - alpha[m])
                y2 *= alpha[c2 - s]
                est += y2 * ratio[c2] / S[c2]

            res5[ip, s] = est
            res2[ip, s] = res3[ip, s] + est * ult[ip] ** 2

    # Totals, including covariance between accident years (uses res5 of the
    # more-developed year, which is zero once that year has finished developing).
    total_cdr_msep = np.zeros(n_steps)
    for s in range(n_steps):
        process = sum(res3[ip, s] for ip in active)
        est_total = 0.0
        for ip in active:
            for i1 in active:
                est_total += res5[min(ip, i1), s] * ult[ip] * ult[i1]
        total_cdr_msep[s] = process + est_total

    return reserve, res2, total_cdr_msep


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def merz_wuthrich(incremental: np.ndarray, weight_mask: np.ndarray | None = None,
                  sigma_method: str = "loglinear", exclude_first_dev: bool = False) -> dict:
    """
    Analytic one-year (Merz-Wuthrich) and ultimate (Mack) reserve uncertainty.

    Returns a dict with:
      table             per-accident-year + Total DataFrame (IBNR, one-year CDR
                        S.E., ultimate Mack S.E.)
      emergence_factor  total one-year S.E. / total ultimate S.E.
      emergence_pattern total one-year CDR S.E. by future calendar year, and as a
                        ratio of the ultimate S.E.
      total_oneyear_se, total_ultimate_se, total_reserve
    """
    arr = _as_array(incremental)
    if exclude_first_dev:
        arr = drop_first_dev(arr)
    cum = _cumulate(arr)
    I0, J0 = cum.shape
    if I0 != J0:
        raise ValueError(f"Triangle must be square (got {I0}x{J0}); aggregate first.")

    clq = _chain_ladder(cum, weight_mask, sigma_method)
    reserve, cdr_msep, total_cdr_msep = _cl_mseps(clq, I0, J0)

    oneyear_se = np.sqrt(cdr_msep[:, 0])                 # CDR(1) per accident year
    ult_se = np.sqrt(cdr_msep.sum(axis=1))               # Mack per accident year
    total_oneyear_se = float(np.sqrt(total_cdr_msep[0]))
    total_ultimate_se = float(np.sqrt(total_cdr_msep.sum()))
    total_reserve = float(reserve.sum())

    table = pd.DataFrame({
        "accident_year": list(range(1, I0 + 1)) + ["Total"],
        "IBNR": list(reserve) + [total_reserve],
        "oneyear_CDR_SE": list(oneyear_se) + [total_oneyear_se],
        "ultimate_Mack_SE": list(ult_se) + [total_ultimate_se],
    })

    total_cdr_se_by_year = np.sqrt(total_cdr_msep)
    emergence_pattern = pd.DataFrame({
        "future_year": np.arange(1, J0),
        "total_CDR_SE": total_cdr_se_by_year,
        "ratio_to_ultimate": total_cdr_se_by_year / total_ultimate_se,
    })

    return {
        "table": table,
        "emergence_factor": total_oneyear_se / total_ultimate_se,
        "emergence_pattern": emergence_pattern,
        "total_oneyear_se": total_oneyear_se,
        "total_ultimate_se": total_ultimate_se,
        "total_reserve": total_reserve,
    }


def sensitivity_oneyear(incremental: np.ndarray, sigma_method: str = "loglinear",
                        exclude_first_dev: bool = False) -> pd.DataFrame:
    """
    Leave-one-out sensitivity: exclude each observed age-to-age ratio in turn and
    recompute, to find cells whose removal moves the one-year risk the most.

    Returns a long DataFrame ranked by absolute change in the total one-year CDR
    S.E., with the change in reserve, ultimate S.E., and emergence factor too.
    """
    incremental = _as_array(incremental)
    if exclude_first_dev:
        incremental = drop_first_dev(incremental)
    cum = _cumulate(incremental)
    I0, J0 = cum.shape
    observed = ~np.isnan(cum)

    base = merz_wuthrich(incremental, sigma_method=sigma_method)
    base_oy = base["total_oneyear_se"]
    base_ult = base["total_ultimate_se"]
    base_res = base["total_reserve"]
    base_ef = base["emergence_factor"]

    # number of usable age-to-age ratios in each development column
    col_ratio_count = [
        sum(1 for i in range(I0) if observed[i, j] and observed[i, j + 1])
        for j in range(J0 - 1)
    ]

    rows = []
    for i in range(I0):
        for j in range(J0 - 1):
            # a ratio exists at (i, j) if both C[i,j] and C[i,j+1] are observed
            if not (observed[i, j] and observed[i, j + 1]):
                continue
            # skip if it is the only ratio in its column (removing it would leave
            # the development factor unestimable and the result undefined)
            if col_ratio_count[j] <= 1:
                continue
            mask = np.ones((J0, J0))
            mask[i, j] = 0
            try:
                alt = merz_wuthrich(incremental, weight_mask=mask, sigma_method=sigma_method)
            except Exception:
                continue
            rows.append({
                "accident_year": i + 1,
                "dev_period": j + 1,
                "d_oneyear_SE": alt["total_oneyear_se"] - base_oy,
                "d_ultimate_SE": alt["total_ultimate_se"] - base_ult,
                "d_reserve": alt["total_reserve"] - base_res,
                "d_emergence_factor": alt["emergence_factor"] - base_ef,
            })

    df = pd.DataFrame(rows)
    df["abs_d_oneyear_SE"] = df["d_oneyear_SE"].abs()
    df = df.sort_values("abs_d_oneyear_SE", ascending=False).drop(columns="abs_d_oneyear_SE")
    return df.reset_index(drop=True)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Merz-Wuthrich one-year CDR (analytic).")
    parser.add_argument("--triangle", required=True, help="Incremental triangle CSV.")
    parser.add_argument("--sigma-method", default="loglinear", choices=["loglinear", "mack"],
                        help="Tail-sigma extrapolation: loglinear (matches R ChainLadder, "
                             "default) or mack (classic Mack min rule / this repo's bootstrap).")
    parser.add_argument("--exclude-first-dev", action="store_true",
                        help="Drop the first development column (and the immature most-recent "
                             "accident year); useful for long-tail lines like GL.")
    parser.add_argument("--sensitivity", action="store_true",
                        help="Also run leave-one-out outlier sensitivity.")
    parser.add_argument("--top", type=int, default=10, help="Sensitivity rows to show.")
    parser.add_argument("--out", help="Optional CSV path for the main table.")
    args = parser.parse_args()

    incremental = pd.read_csv(args.triangle, index_col=0).to_numpy(dtype=float)
    res = merz_wuthrich(incremental, sigma_method=args.sigma_method,
                        exclude_first_dev=args.exclude_first_dev)

    print(f"Total reserve      : {res['total_reserve']:,.0f}")
    print(f"Ultimate (Mack) SE : {res['total_ultimate_se']:,.0f}")
    print(f"One-year (MW)  SE  : {res['total_oneyear_se']:,.0f}")
    print(f"\n>>> RISK EMERGENCE FACTOR (one-year SE / ultimate SE): "
          f"{res['emergence_factor']:.1%}\n")
    with pd.option_context("display.float_format", lambda x: f"{x:,.1f}"):
        print(res["table"].to_string(index=False))
    print("\nEmergence pattern by future calendar year:")
    with pd.option_context("display.float_format", lambda x: f"{x:,.3f}"):
        print(res["emergence_pattern"].to_string(index=False))

    if args.sensitivity:
        print("\n" + "=" * 60)
        print("Leave-one-out sensitivity (largest impact on one-year SE):")
        sens = sensitivity_oneyear(incremental, sigma_method=args.sigma_method,
                                   exclude_first_dev=args.exclude_first_dev)
        money = lambda x: f"{x:,.0f}"
        formatters = {
            "d_oneyear_SE": money, "d_ultimate_SE": money, "d_reserve": money,
            "d_emergence_factor": lambda x: f"{x:+.3f}",
        }
        print(sens.head(args.top).to_string(index=False, formatters=formatters))

    if args.out:
        res["table"].to_csv(args.out, index=False)
        print(f"\nWrote table -> {args.out}")


if __name__ == "__main__":
    main()
