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
or `"mack"`), and `isCumulative` (`False` default; set `True` if the block is
cumulative rather than incremental).

## Macros (`Alt+F8`)

- `MW_Report` - prompts for the triangle, writes a per-accident-year table
  (IBNR, one-year CDR SE, ultimate Mack SE), totals, and the emergence pattern.
- `MW_Sensitivity` - leave-one-out over every age-to-age ratio, ranked by impact
  on the one-year SE - shows which cells/outliers drive (or don't) the risk.
- `MW_SelfTest` - validation against the built-in GenIns reference.

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
