### Notes for Python:

- The files in the Reserve_Risk folder can be opened and run in a development environment such as Visual Studio Code (VS Code).
- The main stochastic reserving functions (excluding MCMC) are in StochResFunctions.py. This is called by the other files where the functions are used.
- The MCMC functions can be found in StochResFunctions_MCMC.py. They are in a separate file due the difficulty of installing and using _CmdStanPy_ (see comments in the code). If the MCMC results are not of interest, this file can be ignored and the relevent sections of code where the MCMC functions are used can be commented out to leave just the bootstrap results.
- There are three main analysis scripts: EV_2006_PredictiveDistributions.py, EVW_2019.py, and Example_Modus_Operandi.py. These were converted from the original Jupyter notebooks (preserved in the ../old folder) into plain Python scripts. Run each from within this folder so the CSV data paths resolve.
- The ".stan" files are required when running the MCMC parts of EV_2006_PredictiveDistributions.py. Equivalent ".exe" files will be created the first time they are called - this can take a few minutes.

#### risk_emergence.py

- Standalone tool: takes an incremental claims triangle and reports the **risk emergence factor** - the share of total (ultimate) reserve risk that emerges over the next calendar year - plus the full year-by-year emergence pattern.
- It bootstraps the reserve distribution (ultimate view) and runs the "Actuary-in-the-Box" CDR (one-year views) using the functions in StochResFunctions.py.
- Works on an annual x annual triangle. Pass `--periodicity quarterly` to aggregate a quarterly triangle to annual first (a partially developed most-recent year is dropped so the result is a proper triangle).
- Example: `python risk_emergence.py --triangle claims_triangle.csv`
- Example: `python risk_emergence.py --triangle ../simulation/outputs/gl_canada_triangle.csv --periodicity quarterly`

#### merz_wuthrich.py

- Standalone **analytic** (non-simulated) one-year reserve risk via Merz & Wuthrich (2008), the closed-form companion to Mack (1993) for the lifetime view.
- Reports the ultimate (Mack) S.E., the one-year (Merz-Wuthrich) S.E., the **risk emergence factor** (one-year S.E. / ultimate S.E.), and the full year-by-year emergence pattern - all without Monte Carlo.
- `--sensitivity` runs a leave-one-out over every age-to-age ratio to show which cells/outliers move the one-year risk the most (and which barely touch the emergence factor).
- Tail-sigma extrapolation: `--sigma-method loglinear` (default) reproduces R's `ChainLadder::CDR` exactly; `--sigma-method mack` gives the classic Mack S.E. (and matches this repo's own bootstrap/simulation).
- Validated against R `ChainLadder::CDR(MackChainLadder(GenIns))` - see `test_merz_wuthrich.py` (`python test_merz_wuthrich.py`).
- Assumptions are Mack's: chain ladder is correct, accident years independent, Var proportional to cumulative (alpha = 1), no tail beyond the triangle. A single unusual cell feeds both the factor and the variance of its column, so outliers can move the answer - use `--sensitivity` to find them.
- Example: `python merz_wuthrich.py --triangle claims_triangle.csv --sensitivity`

#### triangle_io.py

- Imports triangles (and vectors such as earned premium) from one or more Excel files into a single tidy **long table**: `Region | Entity | LoB | AY | DY | Type | Name | Value`. DY is development length/lag; values are stored **incremental**.
- One table holds many triangles of different sizes/sources; filter + pivot to get the array a tool needs: `to_triangle_array(long, name="GL_2024")` returns an incremental triangle ready for `merz_wuthrich` / `risk_emergence`.
- Driven by a list of **specs** (one per triangle), so different files, sheet names, ranges and sizes are all handled. Each triangle is located by an explicit `range` (e.g. `"B2:L12"`) or a `top_left` corner (auto-detect); layout defaults to a labelled grid; `orientation: cumulative` is converted to incremental on import; `type: Vector` stores AY-indexed vectors (DY = `<NA>`).
- Python API: `from triangle_io import import_triangles, to_triangle_array`.
- CLI (manifest-driven): `python triangle_io.py --manifest triangles_manifest.example.yaml --out triangles_long.csv` (see the example manifest in this folder).



