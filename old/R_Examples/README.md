### Notes for R:

- Copy the files in the R_Examples folder to a location on your hard drive, then open the StochResExamples.Rproj file in R Studio. All of the files will then be accessible without needing to change the working directory.
- The main stochastic reserving functions are in StochResFunctions.R. This is called by the other example files where the functions are used.
- There are three main R Markdown example files: EV_2006_PredictiveDistributions.Rmd, EVW_2019.Rmd, and Example_Modus_Operandi.Rmd. These can be opened in R Studio and run using "knit". This will create the html output files included in the R_Examples folder.
- Equivalent standard R files are also available with the ".R" extension for those unfamiliar with R Markdown.
- The ".stan" files are required when running the MCMC parts of EV_2006_PredictiveDistributions.Rmd
- Installing and using the rstan package can be problematic. If the MCMC results are not of interest, the relevant sections of EV_2006_PredictiveDistributions.Rmd can be commented out to leave just the bootstrap results.

**Dependencies**

R packages used in the development of the examples presented here include:
- dplyr
- tidyverse
- ggplot2
- ggfan
- ChainLadder
- rstan
- shinystan (optional for reviewing MCMC outputs)




