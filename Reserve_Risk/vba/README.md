# Merz-Wuthrich one-year reserve risk - VBA version

A self-contained VBA port of the **analytic** Merz-Wuthrich (2008) one-year CDR
(the `merz_wuthrich.py` engine), for running directly in Excel with no Python
install. It needs no add-ins and no matrix/RNG libraries - just arithmetic - so
it is easy to email and run at a workplace.

## Files

| File | What |
|---|---|
| `MerzWuthrich.bas` | The VBA module: UDFs + macros. Import this into any workbook. |
| `MerzWuthrich_demo.xlsx` | Demo workbook with the GenIns triangle and instructions. |

## Setup (one-time, ~30 seconds)

1. Open `MerzWuthrich_demo.xlsx` (or your own workbook).
2. `Alt+F11` -> **File > Import File...** -> choose `MerzWuthrich.bas`.
3. **File > Save As > Excel Macro-Enabled Workbook (`.xlsm`)**.
4. Run `MW_SelfTest` (`Alt+F8`) - it should report **PASS** (reproduces the
   GenIns reference: one-year SE 1,774,013.8, ultimate SE 2,441,364.1, 72.7%).

> A ready-made `.xlsm` is **not** included because it was built on a machine
> without Excel, and a macro-enabled workbook can only be authored by Excel
> itself. The import + save above produces it in two clicks.

## Cell functions (UDFs)

Point them at the **value block only** (no AY/DY labels), e.g. `C3:L12`:

```
=MW_EmergenceFactor(C3:L12)        ' one-year SE / ultimate SE
=MW_OneYearSE(C3:L12)
=MW_UltimateSE(C3:L12)
=MW_TotalReserve(C3:L12)
```

Optional arguments: `sigmaMethod` (`"loglinear"` default, matches R ChainLadder;
or `"mack"`), `isCumulative` (`False` default; set `True` if the block is
cumulative rather than incremental), and `excludeFirstDev` (`False` default; set
`True` to drop the first development column and the immature most-recent accident
year - useful for long-tail lines like GL where the first column would skew the
result). The first development period's dollars are kept (folded into DP2).

## Macros (`Alt+F8`)

- `MW_Report` - prompts for the triangle, writes a per-accident-year table
  (IBNR, one-year CDR SE, ultimate Mack SE), totals, and the emergence pattern.
- `MW_Sensitivity` - leave-one-out over every age-to-age ratio, ranked by impact
  on the one-year SE - shows which cells/outliers drive (or don't) the risk.
- `MW_RunFromSetup` - **batch**: reads a `setup` sheet listing many triangles and
  analyses them all, writing a comparison to `RiskEmergence_Summary` (pre-built in
  the demo workbook with labels; the macro reuses it if present). Each row shows
  the **aggregate** emergence factor (one-year / ultimate, column F) and the
  **per-development-period** emergence in the `EF Yr1, EF Yr2, ...` columns (each
  = that future period's CDR SD / ultimate SD; their squares sum to 100%).
- `MW_Portfolio` - aggregates `RiskEmergence_Summary` into one portfolio
  emergence factor using a single correlation `rho` (see below).
- `MW_CumulativeToIncremental` - utility: converts a **selected** cumulative
  triangle to incremental (any size, including more accident years than
  development periods); optionally pastes the result directly below the block.
- `MW_BatchCumulativeToIncremental` - same conversion for **every** cumulative
  triangle listed on the `setup` sheet (Cumulative = Y rows), writing each
  incremental copy below its block. Already-incremental rows are skipped.
- `MW_SelfTest` - validation against the built-in GenIns reference.

## Portfolio aggregation (`MW_Portfolio`)

After `MW_RunFromSetup`, run `MW_Portfolio` to combine all the LoBs into one
number. It prompts for a correlation `rho` (0 = independent, 1 = fully
correlated; default 0.25) and appends a portfolio block to the summary sheet.

Emergence factors are **ratios of standard deviations**, so they cannot be
averaged directly. The macro aggregates the dollar SEs with correlation
(`Var = rho*(Sum x)^2 + (1-rho)*Sum x^2`) for the one-year and ultimate horizons
separately, then divides. Because the same `rho` sits in both, it largely
cancels and the portfolio factor is robust - the independence and
full-correlation bookends are shown so you can see the (small) range. A single
`rho` is used in v1; a full LoB correlation matrix is tracked as a future
enhancement.

## Batch runs via the `setup` sheet

Create a sheet named `setup` (the demo workbook includes one). Row 1 is headers,
data from row 2:

| Col | Header | Meaning |
|---|---|---|
| A | Worksheet | name of the sheet holding the triangle (required) |
| B | Range | value block, e.g. `C3:L12` (blank = that sheet's used range) |
| C | Cumulative | `Y`/`N` (default `N` = incremental) |
| D | SigmaMethod | `loglinear` (default) or `mack` |
| E | ExcludeFirstDev | `Y`/`N` (default `N`); drop first dev column + immature recent AY |

Run `MW_RunFromSetup`. It writes one row per listed worksheet to
`RiskEmergence_Summary` - size, total reserve, ultimate SE, one-year SE, the
emergence factor, a status flag, and the emergence-by-year pattern (`EF Yr1`,
`EF Yr2`, ...). Rows that fail (missing sheet, non-square block) are flagged in
the Status column rather than stopping the batch.

## Scope / notes

- **Analytic only.** The simulation/bootstrap method is not ported - VBA's RNG
  and 10k-iteration cost make it impractical. For full tails/VaR use the Python
  `risk_emergence(..., method="simulation")`.
- Input must be a **square** triangle (annual x annual); aggregate quarterly
  data to annual first.
- The numerical algorithm was validated to match the Python/R reference exactly
  via a line-for-line transcription; `MW_SelfTest` re-checks it inside Excel.
- Assumptions are Mack's (chain ladder correct, accident years independent,
  variance proportional to cumulative, no tail beyond the triangle).
