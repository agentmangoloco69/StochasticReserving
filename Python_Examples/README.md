### Notes for Python:

- Copy the files in the Python_Examples folder to a location on your hard drive, which can then be opened in a development environment. The example files were created using Visual Studio Code (VS Code).
- The main stochastic reserving functions (excluding MCMC) are in StochResFunctions.py. This is called by the other example files where the functions are used.
- The MCMC functions can be found in StochResFunctions_MCMC.py. They are in a separate file due the difficulty of installing and using _CmdStanPy_ (see comments in the code). If the MCMC results are not of interest, this file can be ignored and the relevent sections of code where the MCMC functions are used can be commented out to leave just the bootstrap results.
- There are three main Jupyter example files: EV_2006_PredictiveDistributions.ipynb, EVW_2019.ipynb, and Example_Modus_Operandi.ipynb. These can be opened in VS Code and run.
- The ".stan" files are required when running the MCMC parts of EV_2006_PredictiveDistributions.ipynb. Equivalent ".exe" files will be created the first time they are called - this can take a few minutes.



