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



