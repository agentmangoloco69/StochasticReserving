### Notes for Python:

- The files in the Reserve_Risk folder can be opened and run in a development environment such as Visual Studio Code (VS Code).
- The main stochastic reserving functions (excluding MCMC) are in StochResFunctions.py. This is called by the other files where the functions are used.
- The MCMC functions can be found in StochResFunctions_MCMC.py. They are in a separate file due the difficulty of installing and using _CmdStanPy_ (see comments in the code). If the MCMC results are not of interest, this file can be ignored and the relevent sections of code where the MCMC functions are used can be commented out to leave just the bootstrap results.
- There are three main analysis scripts: EV_2006_PredictiveDistributions.py, EVW_2019.py, and Example_Modus_Operandi.py. These were converted from the original Jupyter notebooks (preserved in the ../old folder) into plain Python scripts. Run each from within this folder so the CSV data paths resolve.
- The ".stan" files are required when running the MCMC parts of EV_2006_PredictiveDistributions.py. Equivalent ".exe" files will be created the first time they are called - this can take a few minutes.

#### risk_emergence.py  (the entry point)

- Single interface for the **risk emergence factor** - the share of total (ultimate) reserve risk emerging over the next calendar year - plus the full year-by-year emergence pattern. Two methods behind one call:
  - `method="analytic"` (**default**) - closed-form Merz-Wuthrich (2008) one-year CDR vs Mack (1993) ultimate. Fast, exact, validated against R ChainLadder. Engine: `merz_wuthrich.py`.
  - `method="simulation"` - bootstrap the reserve distribution + "Actuary-in-the-Box" CDR. Slower, but gives the full predictive distribution (VaR/tails) and supports ODP / Negative Binomial. Engine: `StochResFunctions.py`.
- Both return the same headline keys (`emergence_factor`, `total_oneyear_se`, `total_ultimate_se`, `total_reserve`, `emergence_pattern`, `detail`), so you can switch methods freely.
- Annual x annual; pass `--periodicity quarterly` to aggregate a quarterly triangle first (a partially developed most-recent year is dropped).
- `--sensitivity` (analytic) runs a leave-one-out over every age-to-age ratio to show which cells/outliers move the one-year risk the most (and which barely touch the emergence factor).
- Python API: `from risk_emergence import risk_emergence, to_triangle` then `risk_emergence(tri)` / `risk_emergence(tri, method="simulation")`.
- **Portfolio aggregation:** `portfolio_emergence(results, rho=0.25)` combines several LoBs into one portfolio emergence factor. Emergence factors are ratios of SDs, so it aggregates the dollar SEs with a single correlation `rho` (same matrix for both horizons, so `rho` largely cancels and the factor is robust) and reports independence/full-correlation bookends. Single `rho` in v1; full correlation matrix tracked in issue #5.
- Examples:
  - `python risk_emergence.py --triangle claims_triangle.csv` (analytic, default)
  - `python risk_emergence.py --triangle claims_triangle.csv --sensitivity`
  - `python risk_emergence.py --triangle paid.csv --method simulation --model ODPNonConstant`
  - `python risk_emergence.py --triangle quarterly.csv --periodicity quarterly`

#### merz_wuthrich.py  (analytic engine)

- The closed-form Merz-Wuthrich (2008) one-year CDR, used by `risk_emergence` for `method="analytic"`; also usable directly.
- Reports the ultimate (Mack) S.E., the one-year (Merz-Wuthrich) S.E., the emergence factor, the emergence pattern, and `sensitivity_oneyear()` (leave-one-out outliers) - all without Monte Carlo.
- Tail-sigma extrapolation: `sigma_method="loglinear"` (default) reproduces R's `ChainLadder::CDR` exactly; `"mack"` gives the classic Mack S.E. (matches this repo's own bootstrap/simulation).
- Validated against R `ChainLadder::CDR(MackChainLadder(GenIns))` - see `test_merz_wuthrich.py` (`python test_merz_wuthrich.py`).
- Assumptions are Mack's: chain ladder is correct, accident years independent, Var proportional to cumulative (alpha = 1), no tail beyond the triangle. A single unusual cell feeds both the factor and the variance of its column, so outliers can move the answer - use the sensitivity scan to find them.

#### triangle_io.py

- Imports triangles (and vectors such as earned premium) from one or more Excel files into a single tidy **long table**: `Region | Entity | LoB | AY | DY | Type | Name | Value`. DY is development length/lag; values are stored **incremental**.
- One table holds many triangles of different sizes/sources; `to_triangle(long, name="GL_2024")` returns a single labelled incremental triangle (DataFrame) that you pass straight to `merz_wuthrich` / `risk_emergence` (both accept a DataFrame or array).
- Driven by a list of **specs** (one per triangle), so different files, sheet names, ranges and sizes are all handled. Each triangle is located by an explicit `range` (e.g. `"B2:L12"`) or a `top_left` corner (auto-detect); layout defaults to a labelled grid; `orientation: cumulative` is converted to incremental on import; `type: Vector` stores AY-indexed vectors (DY = `<NA>`).
- Python API: `from triangle_io import import_triangles, to_triangle`.
- CLI (manifest-driven): `python triangle_io.py --manifest triangles_manifest.example.yaml --out triangles_long.csv` (see the example manifest in this folder).

#### vba/  (Excel-native version)

- A self-contained VBA port of the **analytic** Merz-Wuthrich one-year CDR for running in Excel with no Python install - UDFs (`=MW_EmergenceFactor(range)` etc.), a `MW_Report` macro, a `MW_Sensitivity` (leave-one-out) macro, and `MW_SelfTest`.
- Validated to reproduce the Python/R reference (GenIns: 72.7%, one-year 1,774,014, ultimate 2,441,364). Simulation method is not ported (use Python for that).
- See `vba/README.md` for setup. Import `vba/MerzWuthrich.bas` into the `vba/MerzWuthrich_demo.xlsx` workbook and save as `.xlsm`.



