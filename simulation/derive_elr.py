"""
Derive expected loss ratios (ELRs) from a claims triangle, the way actuaries do:
first develop to ultimate with a volume-weighted chain ladder, then trend the
older accident years to current cost level, then express on-level loss ratios.

This is a standalone utility. It works on ANY incremental quarterly triangle in
the format produced by simulate_triangle.py -- whether simulated or real reserving
team data -- so the same pipeline serves both training and later validation.

    python derive_elr.py --triangle outputs/gl_canada_triangle.csv \
                         --premium outputs/gl_canada_earned_premium.csv \
                         --severity-trend 0.04 --frequency-trend -0.01 \
                         --premium-trend 0.03

Earned-premium handling here is intentionally simple; full on-levelling is
tracked in GitHub issue #4.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd


def load_incremental_triangle(path: str) -> np.ndarray:
    """Read a triangle CSV (simulate_triangle.py format) into a 2D array with NaNs."""
    df = pd.read_csv(path, index_col=0)
    return df.to_numpy(dtype=float)


def cumulate(incremental: np.ndarray) -> np.ndarray:
    """Row-wise cumulative sum that preserves NaN (unobserved) cells."""
    cum = np.full_like(incremental, np.nan)
    for i in range(incremental.shape[0]):
        running = 0.0
        for j in range(incremental.shape[1]):
            v = incremental[i, j]
            if np.isnan(v):
                break
            running += v
            cum[i, j] = running
    return cum


def volume_weighted_ldfs(cum: np.ndarray) -> np.ndarray:
    """Volume-weighted (chain ladder) age-to-age factors for each development step."""
    n_dev = cum.shape[1]
    ldfs = np.ones(n_dev - 1)
    for j in range(n_dev - 1):
        num = 0.0
        den = 0.0
        for i in range(cum.shape[0]):
            a, b = cum[i, j], cum[i, j + 1]
            if not np.isnan(a) and not np.isnan(b):
                num += b
                den += a
        ldfs[j] = num / den if den > 0 else 1.0
    return ldfs


def chain_ladder_ultimates(cum: np.ndarray, ldfs: np.ndarray) -> np.ndarray:
    """Develop the latest diagonal of each accident period to ultimate."""
    n_acc, n_dev = cum.shape
    cdf_to_ult = np.ones(n_dev)
    for j in range(n_dev - 2, -1, -1):
        cdf_to_ult[j] = cdf_to_ult[j + 1] * ldfs[j]

    ultimates = np.empty(n_acc)
    for i in range(n_acc):
        latest_j = -1
        for j in range(n_dev):
            if not np.isnan(cum[i, j]):
                latest_j = j
        ultimates[i] = cum[i, latest_j] * cdf_to_ult[latest_j] if latest_j >= 0 else np.nan
    return ultimates


def derive_elr(
    triangle_path: str,
    premium_path: str,
    severity_trend: float = 0.04,
    frequency_trend: float = -0.01,
    premium_trend: float = 0.03,
) -> pd.DataFrame:
    """
    Return a per-accident-quarter table with developed ultimates, on-level
    premium and losses, and trended loss ratios, plus summary ELR selections.
    """
    incremental = load_incremental_triangle(triangle_path)
    cum = cumulate(incremental)
    ldfs = volume_weighted_ldfs(cum)
    ultimates = chain_ladder_ultimates(cum, ldfs)

    ep = pd.read_csv(premium_path)["earned_premium"].to_numpy(dtype=float)
    n = len(ultimates)
    if len(ep) != n:
        raise ValueError(
            f"Premium rows ({len(ep)}) != accident quarters in triangle ({n})."
        )

    # Trend losses and premium from each accident quarter to the most recent one.
    loss_trend = (1 + severity_trend) * (1 + frequency_trend) - 1
    aq = np.arange(n)
    years_to_present = (n - 1 - aq) / 4.0
    onlevel_losses = ultimates * (1 + loss_trend) ** years_to_present
    onlevel_premium = ep * (1 + premium_trend) ** years_to_present

    raw_lr = ultimates / ep
    onlevel_lr = onlevel_losses / onlevel_premium

    table = pd.DataFrame({
        "accident_quarter": aq + 1,
        "accident_year": aq // 4 + 1,
        "earned_premium": ep,
        "ultimate_loss": ultimates,
        "raw_loss_ratio": raw_lr,
        "onlevel_premium": onlevel_premium,
        "onlevel_loss": onlevel_losses,
        "onlevel_loss_ratio": onlevel_lr,
    })

    # ELR selections from the on-level loss ratios (judgment still belongs to the user).
    straight = float(np.nanmean(onlevel_lr))
    vol_weighted = float(np.nansum(onlevel_losses) / np.nansum(onlevel_premium))
    table.attrs["selected_elr_straight_average"] = straight
    table.attrs["selected_elr_volume_weighted"] = vol_weighted
    return table


def main():
    parser = argparse.ArgumentParser(description="Derive ELRs via ex-chain-ladder + trending.")
    parser.add_argument("--triangle", required=True, help="Incremental triangle CSV.")
    parser.add_argument("--premium", required=True, help="Earned premium CSV.")
    parser.add_argument("--severity-trend", type=float, default=0.04)
    parser.add_argument("--frequency-trend", type=float, default=-0.01)
    parser.add_argument("--premium-trend", type=float, default=0.03)
    parser.add_argument("--out", help="Optional path to write the ELR table CSV.")
    args = parser.parse_args()

    table = derive_elr(
        args.triangle, args.premium,
        args.severity_trend, args.frequency_trend, args.premium_trend,
    )

    with pd.option_context("display.float_format", lambda x: f"{x:,.3f}"):
        print(table.to_string(index=False))
    print()
    print(f"Selected ELR (straight average) : {table.attrs['selected_elr_straight_average']:.3f}")
    print(f"Selected ELR (volume weighted)  : {table.attrs['selected_elr_volume_weighted']:.3f}")

    if args.out:
        table.to_csv(args.out, index=False)
        print(f"\nWrote ELR table -> {args.out}")


if __name__ == "__main__":
    main()
