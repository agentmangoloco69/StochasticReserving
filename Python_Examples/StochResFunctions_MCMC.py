# version 1.0.0
#
## Stochastic reserving MCMC functions in Python to support reproduction of the tables in:

# England & Verrall (2006). Predictive distributions of outstanding liabilities in general insurance,
# Annals of Actuarial Science, 1, II, 221-270. https://doi.org/10.1017/S1748499500000142

# This code is provided by Peter England on behalf of EMC Actuarial and Analytics Ltd as an educational resource.

# Dependent packages
import numpy as np

# For MCMC methods:
# Use CmdStanPy under Windows since Pystan is only supported under Linux (use WSL2 for linux under Windows if required)
# Installation instructions for CmdStanPy: https://cmdstanpy.readthedocs.io/en/v1.0.0/installation.html
# Note that CmdStanPy requires a C++ compiler to be installed (eg Rtools for Windows, or build-essential for Linux)
# g++.exe and make.exe must be in the system PATH for CmdStanPy to work,
# and a copy of make.exe renamed as mingw32-make.exe may also be required.
# It may take a few minutes to compile the Stan model the first time it is run, but subsequent runs will be faster.
from cmdstanpy import CmdStanModel

# Read in helper functions for main stochastic reserving functions excluding MCMC,
# saved as a separate Python file
import StochResFunctions as srf

## MCMC functions

def Main_Mack_MCMC(Triangle, sims_per_chain, chains, seed=None, ForecastDist="Gamma", UserSigma=None):
# Main function for MCMC version of Mack's model given input settings.

  if seed is None:
      seed = np.random.randint(1, 1_000_000)

  # -----------------------------
  # Prepare data
  # -----------------------------
  LRM_factors = srf.LinkRatioMethod(Triangle, Mask=None)
  LRTriangle = LRM_factors["LR_Triangle"]
  LRWeights = LRM_factors["LR_Weights"]
  CL_facs = LRM_factors["LRM_Factors"]
  ChainLadderResult = srf.LinkRatioMethod_forecast(Triangle, CL_facs)
  Latest = ChainLadderResult["Latest"]
  CL_Ultimates = ChainLadderResult["Ultimates"]
  CL_Reserves = ChainLadderResult["Reserves"]
  CL_Cumulatives = ChainLadderResult["Cumulatives"]
  
  Mack_Resids = srf.Mack_Residuals(Triangle)

  nc = len(CL_facs)
  sigmas = Mack_Resids["sigma"]

  row_matrix = np.indices((nc, nc))[0] + 1
  col_matrix = np.indices((nc, nc))[1] + 1

  Row = row_matrix[~np.isnan(LRTriangle)]
  Column = col_matrix[~np.isnan(LRTriangle)]
  Ratio = LRTriangle[~np.isnan(LRTriangle)]
  Weights = LRWeights[~np.isnan(LRTriangle)]

  nr = len(Row)
  Design = np.zeros((nr, nc))

  for i in range(nr):
      for j in range(nc):
          if Column[i] == j + 1:
              Design[i, j] = 1

  Data_for_stan = {
      "N": len(Ratio),
      "C": nc,
      "f": Ratio,
      "w": Weights,
      "col": Column.astype(int),
      "X": Design,
      "sigmas": sigmas
  }

  # -----------------------------
  # Run MCMC
  # -----------------------------
  model = CmdStanModel(stan_file="Macks_model.stan")

  fit = model.sample(
        data=Data_for_stan,
        seed=seed,
        chains=chains,
        iter_sampling=sims_per_chain,
        iter_warmup=1000,
        parallel_chains=chains
    )

  # Extract coefficients
  coefs = fit.stan_variable("coefs")   # shape: (draws, C)
  coefs_mean = np.mean(coefs, axis=0)
  coefs_sd = np.std(coefs, axis=0)

  # -----------------------------
  # Post-processing
  # -----------------------------
  Pseudo_LRs = np.exp(coefs)

  total_draws = chains * sims_per_chain

  Reserves = np.full((total_draws, nc+1), np.nan)
  TotalReserve = np.full(total_draws, np.nan)
  Ultimates = np.full((total_draws, nc+1), np.nan)
  Cumulatives = np.full((total_draws, nc+1, nc+1), np.nan)
  Complete_Cumulatives = np.full((total_draws, nc+1, nc+1), np.nan)

  if ForecastDist=="Lognormal":
    Mack_forecast = srf.Mack_Forecast_Lognormal
  else:
    Mack_forecast = srf.Mack_Forecast_Gamma

  if UserSigma is None:
    ForecastSigma = sigmas
  else:
    ForecastSigma = UserSigma

  kwargs = {'tridata': Triangle, 'sigma': ForecastSigma}


  for i in range(total_draws):
      kwargs.update({'factors': Pseudo_LRs[i]})
      Forecast = Mack_forecast(**kwargs)
      Reserves[i] = Forecast["Reserves"]
      TotalReserve[i] = Forecast["TotalReserve"]
      Cumulatives[i] = Forecast["Cumulatives"]
      Complete_Cumulatives[i] = Forecast["Complete_Forecast"]

  Ultimates = Latest + Reserves
  Avg_Reserve = np.nanmean(Reserves, axis=0)
  SD_Reserve = np.nanstd(Reserves, axis=0, ddof=1)
  CoV_Reserve = abs(srf.safe_divide(SD_Reserve, Avg_Reserve))

  Avg_TotalReserve = np.nanmean(TotalReserve)
  SD_TotalReserve = np.nanstd(TotalReserve, ddof=1)
  CoV_TotalReserve = abs(srf.safe_divide(SD_TotalReserve, Avg_TotalReserve))
  
  iterations = len(coefs)

  finished = True

  return {
      "CL_facs": CL_facs,
      "Latest": Latest,
      "CL_Reserves": CL_Reserves,
      "CL_Ultimates": CL_Ultimates,
      "CL_Cumulatives": CL_Cumulatives,
      "Mack_Resids": Mack_Resids,
      "Pseudo_LRs": Pseudo_LRs,
      "Cumulatives": Cumulatives,
      "Reserves": Reserves,
      "Ultimates": Ultimates,
      "TotalReserve": TotalReserve,
      "Avg_Reserve": Avg_Reserve,
      "SD_Reserve": SD_Reserve,
      "Avg_TotalReserve": Avg_TotalReserve,
      "SD_TotalReserve": SD_TotalReserve,
      "CoV_Reserve": CoV_Reserve,
      "CoV_TotalReserve": CoV_TotalReserve,
      "Complete_Cumulatives": Complete_Cumulatives,
      "iterations": iterations,
      "finished": finished,
      "Coefs_Mean": coefs_mean,
      "Coefs_SD": coefs_sd,
      "Coefs": coefs
  }

def Main_NegBin_MCMC(Triangle, sims_per_chain, chains, seed = np.random.randint(1,1000000), 
                     Scale="NonConstant", ForecastDist="Gamma", UserSqrtScale=None):
# Main function for MCMC version of the over-dispersed Negative Binomial model given input settings.

  if seed is None:
      seed = np.random.randint(1, 1_000_000)

  # -----------------------------
  # Prepare data
  # -----------------------------  
  LRM_factors = srf.LinkRatioMethod(Triangle, Mask=None)
  LRTriangle = LRM_factors["LR_Triangle"]
  LRWeights = LRM_factors["LR_Weights"]
  CL_facs = LRM_factors["LRM_Factors"]
  ChainLadderResult = srf.LinkRatioMethod_forecast(Triangle, CL_facs)
  Latest = ChainLadderResult["Latest"]
  CL_Ultimates = ChainLadderResult["Ultimates"]
  CL_Reserves = ChainLadderResult["Reserves"]
  CL_Cumulatives = ChainLadderResult["Cumulatives"]

  NegBin_Resids =  srf.NegBin_Residuals(Triangle, Scale=Scale)

  nc = len(CL_facs)
  sigmas = NegBin_Resids["sqrtScale"]

  row_matrix = np.indices((nc,nc))[0] + 1
  col_matrix = np.indices((nc,nc))[1] + 1

  Row = row_matrix[~np.isnan(LRTriangle)]
  Column = col_matrix[~np.isnan(LRTriangle)]
  Ratio = LRTriangle[~np.isnan(LRTriangle)]
  Weights = LRWeights[~np.isnan(LRTriangle)]

  nr = len(Row)
  Design = np.zeros((nr,nc))
  #ignore intercept since only one factor
  #Design[:,0] = 1
  for i in range(nr):
      #for j in range(1,nc):
      for j in range(nc):
          if Column[i]==j+1:
              Design[i,j]=1

  Data_for_stan = {"N":len(Ratio),
                        "C": nc,
                        "f": Ratio,
                        "w": Weights,
                        "col": Column,
                        "X": Design,
                        "rootphi": sigmas[Column-1]}

  # -----------------------------
  # Run MCMC
  # -----------------------------
  np.random.seed(seed)

  model = CmdStanModel(stan_file="NegBin_model.stan")

  fit = model.sample(
        data=Data_for_stan,
        seed=seed,
        chains=chains,
        iter_sampling=sims_per_chain,
        iter_warmup=1000,
        parallel_chains=chains
    )
  
  # Extract coefficients
  coefs=fit.stan_variable("coefs")
  coefs_mean = np.mean(coefs, axis=0)
  coefs_sd = np.std(coefs, axis=0)

  # -----------------------------
  # Post-processing
  # -----------------------------
  Pseudo_LRs = np.exp(np.exp((coefs))) # reverse log-log transform

  total_draws = chains * sims_per_chain

  Reserves = np.full((total_draws, nc+1), np.nan)
  TotalReserve = np.full(total_draws, np.nan)
  Ultimates = np.full((total_draws, nc+1), np.nan)
  Cumulatives = np.full((total_draws, nc+1, nc+1), np.nan)
  Complete_Cumulatives = np.full((total_draws, nc+1, nc+1), np.nan)

  if ForecastDist=="Lognormal":
    NegBin_forecast = srf.NegBin_Forecast_Lognormal
  else:
    NegBin_forecast = srf.NegBin_Forecast_Gamma

  if UserSqrtScale is None:
    ForecastSigma = sigmas
  else:
    ForecastSigma = UserSqrtScale


  kwargs = {'tridata': Triangle, 'sigma': ForecastSigma}

  for i in range(total_draws):
    kwargs.update({'factors': Pseudo_LRs[i]})
    Forecast = NegBin_forecast(**kwargs)
    Reserves[i] = Forecast["Reserves"]
    TotalReserve[i] = Forecast["TotalReserve"]
    Cumulatives[i] = Forecast["Cumulatives"]
    Complete_Cumulatives[i] = Forecast["Complete_Forecast"]

  Ultimates = Latest + Reserves  
  Avg_Reserve = np.nanmean(Reserves, axis = 0)
  SD_Reserve = np.nanstd(Reserves, axis = 0, ddof = 1)
  CoV_Reserve = abs(srf.safe_divide(SD_Reserve, Avg_Reserve))

  Avg_TotalReserve = np.nanmean(TotalReserve)
  SD_TotalReserve = np.nanstd(TotalReserve, ddof=1)
  CoV_TotalReserve = abs(srf.safe_divide(SD_TotalReserve, Avg_TotalReserve))

  iterations = len(coefs)

  finished = True

  result = {"CL_facs": CL_facs, 
            "Latest": Latest, 
            "CL_Reserves": CL_Reserves, 
            "CL_Ultimates": CL_Ultimates, 
            "CL_Cumulatives": CL_Cumulatives, 
            "NegBin_Resids": NegBin_Resids, 
            "Pseudo_LRs": Pseudo_LRs, 
            "Cumulatives": Cumulatives, 
            "Reserves": Reserves, 
            "Ultimates": Ultimates,
            "TotalReserve": TotalReserve, 
            "Avg_Reserve": Avg_Reserve, 
            "SD_Reserve": SD_Reserve, 
            "Avg_TotalReserve": Avg_TotalReserve, 
            "SD_TotalReserve": SD_TotalReserve, 
            "CoV_Reserve": CoV_Reserve, 
            "CoV_TotalReserve": CoV_TotalReserve,
            "Complete_Cumulatives": Complete_Cumulatives,
            "iterations": iterations, 
            "finished": finished,
            "Coefs_Mean": coefs_mean,
            "Coefs_SD": coefs_sd
            }

  return result


def Main_ODP_MCMC(Triangle, sims_per_chain, chains, seed = np.random.randint(1,1000000),
                   Scale="NonConstant", ForecastDist="Gamma", UserSqrtScale=None):
# Main function for MCMC version of the over-dispersed Poisson model given input settings.

  if seed is None:
      seed = np.random.randint(1, 1_000_000)

  # -----------------------------
  # Prepare data
  # -----------------------------
  LRM_factors = srf.LinkRatioMethod(Triangle, Mask=None)
  LRTriangle = LRM_factors["LR_Triangle"]
  LRWeights = LRM_factors["LR_Weights"]
  CL_facs = LRM_factors["LRM_Factors"]
  ChainLadderResult = srf.LinkRatioMethod_forecast(Triangle, CL_facs)
  Latest = ChainLadderResult["Latest"]
  CL_Ultimates = ChainLadderResult["Ultimates"]
  CL_Reserves = ChainLadderResult["Reserves"]
  CL_Cumulatives = ChainLadderResult["Cumulatives"]

  ODP_Resids =  srf.ODP_Residuals(Triangle, Scale=Scale)

  nc = len(Triangle)
  sigmas = ODP_Resids["sqrtScale"]

  ODP_Analytic_Result = srf.ODP_ChainLadder(Triangle, Scale=Scale)

  row_matrix = np.indices((nc,nc))[0] + 1
  col_matrix = np.indices((nc,nc))[1] + 1

  Row = row_matrix[~np.isnan(Triangle)]
  Column = col_matrix[~np.isnan(Triangle)]
  Inc_Triangle = srf.Incrementals(Triangle)
  Inc_Claims = Inc_Triangle[~np.isnan(Inc_Triangle)]

  Past_Design = ODP_Analytic_Result["Past_Design"]

  Data_for_stan = {"N":len(Inc_Claims),
                        "C": 2*nc-1,
                        "cl": Inc_Claims,
                        "X": Past_Design,
                        "rootphi": sigmas[Column-1]}

# -----------------------------
  # Run MCMC
  # -----------------------------
  
  np.random.seed(seed)

  model = CmdStanModel(stan_file="ODP_model.stan")

  fit = model.sample(
        data=Data_for_stan,
        seed=seed,
        chains=chains,
        iter_sampling=sims_per_chain,
        iter_warmup=1000,
        parallel_chains=chains
    )

  # Extract coefficients
  coefs=fit.stan_variable("coefs")
  coefs_mean = np.mean(coefs, axis=0)
  coefs_sd = np.std(coefs, axis=0)

  # -----------------------------
  # Post-processing
  # -----------------------------

  total_draws = chains * sims_per_chain

  Reserves = np.full((total_draws, nc), np.nan)
  TotalReserve = np.full(total_draws, np.nan)
  Ultimates = np.full((total_draws, nc), np.nan)
  Cumulatives = np.full((total_draws, nc, nc), np.nan)
  Complete_Cumulatives = np.full((total_draws, nc, nc), np.nan)

  if ForecastDist=="Lognormal":
    ODP_forecast = ODP_MCMC_Forecast_Lognormal
  else:
    ODP_forecast = ODP_MCMC_Forecast_Gamma

  if UserSqrtScale is None:
    ForecastSigma = sigmas
  else:
    ForecastSigma = UserSqrtScale


  kwargs = {'tridata': Triangle, 'sqrtScale': ForecastSigma}

  for i in range(total_draws):
      kwargs.update({'coefs': coefs[i]})
      Forecast = ODP_forecast(**kwargs)
      Reserves[i] = Forecast["Reserves"]
      TotalReserve[i] = Forecast["TotalReserve"]
      Cumulatives[i] = Forecast["Cumulatives"]
      Complete_Cumulatives[i] = Forecast["Complete_Forecast"]

  Ultimates = Latest + Reserves  
  Avg_Reserve = np.nanmean(Reserves, axis = 0)
  SD_Reserve = np.nanstd(Reserves, axis = 0, ddof = 1)
  CoV_Reserve = abs(srf.safe_divide(SD_Reserve, Avg_Reserve))

  Avg_TotalReserve = np.nanmean(TotalReserve)
  SD_TotalReserve = np.nanstd(TotalReserve, ddof=1)
  CoV_TotalReserve = abs(srf.safe_divide(SD_TotalReserve, Avg_TotalReserve))

  iterations = len(coefs)

  finished = True

  result = {"CL_facs": CL_facs, 
            "Latest": Latest,
            "ODP_Resids": ODP_Resids, 
            "Cumulatives": Cumulatives, 
            "Reserves": Reserves,
            "Ultimates": Ultimates,
            "TotalReserve": TotalReserve, 
            "Avg_Reserve": Avg_Reserve, 
            "SD_Reserve": SD_Reserve, 
            "Avg_TotalReserve": Avg_TotalReserve, 
            "SD_TotalReserve": SD_TotalReserve, 
            "CoV_Reserve": CoV_Reserve, 
            "CoV_TotalReserve": CoV_TotalReserve,
            "Complete_Cumulatives": Complete_Cumulatives,
            "iterations": iterations, 
            "finished": finished,
            "Coefs_Mean": coefs_mean,
            "Coefs_SD": coefs_sd
            }

  return result


def ODP_MCMC_Forecast_Gamma(tridata, coefs, sqrtScale):
  # Function for calculating incremental forecasts when using the MCMC version of the ODP model using 
  # a parametric approach and a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but beware, values returned could be negative.

  nc = len(tridata[0])
  incrementals = srf.npNaN(tridata.shape)
  cumulatives=np.copy(tridata)

  row_matrix = np.indices((nc,nc))[0] + 1
  col_matrix = np.indices((nc,nc))[1] + 1

  intercept = coefs[0]
  row_coefs = np.concatenate(([0],coefs[1:nc]))
  col_coefs = np.concatenate(([0],coefs[nc:(2*nc)]))

  tol = 1e-12

  for i in range(1,nc):
      for j in range(nc-i,nc):
        incrementals[i,j] = np.exp(intercept + row_coefs[row_matrix[i,j]-1] + col_coefs[col_matrix[i,j]-1])
        Mean = incrementals[i][j]
        SD = sqrtScale[j]*np.sqrt(abs(Mean))
        
        if Mean > tol:
            if SD < tol:
                incrementals[i,j] = Mean
            else:
                scale = (SD**2)/Mean
                shape = Mean/scale
                incrementals[i,j] = np.random.gamma(shape=shape, scale=scale)

        else:
            incrementals[i,j] = np.random.normal(loc=Mean, scale=SD)
        
        cumulatives[i,j] = cumulatives[i,j-1] + incrementals[i,j]

  complete_forecast = cumulatives

  Ultimates = complete_forecast[:,-1]
  Paid = np.diag(np.fliplr(tridata))
  Reserves = Ultimates - Paid
  TotalReserve = np.nansum(Reserves)

  result = {"Cumulatives": cumulatives, 
            "Ultimates": Ultimates, 
            "Reserves": Reserves, 
            "TotalReserve": TotalReserve, 
            "Complete_Forecast": complete_forecast}

  
  return result

def ODP_MCMC_Forecast_Lognormal(tridata, coefs, sqrtScale):
  # Function for calculating incremental forecasts when using the MCMC version of the ODP model using 
  # a parametric approach and a Lognormal distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but beware, values returned could be negative.
  nc = len(tridata[0])
  incrementals = srf.npNaN(tridata.shape)
  cumulatives=np.copy(tridata)

  row_matrix = np.indices((nc,nc))[0] + 1
  col_matrix = np.indices((nc,nc))[1] + 1

  intercept = coefs[0]
  row_coefs = np.concatenate(([0],coefs[1:nc]))
  col_coefs = np.concatenate(([0],coefs[nc:(2*nc)]))

  tol = 1e-12

  for i in range(1,nc):
      for j in range(nc-i,nc):
        incrementals[i,j] = np.exp(intercept + row_coefs[row_matrix[i,j]-1] + col_coefs[col_matrix[i,j]-1])
        Mean = incrementals[i][j]
        SD = sqrtScale[j]*np.sqrt(abs(Mean))
        
        if Mean > tol:
            sigma_normal = np.sqrt(np.log(1 + (SD/Mean)**2))
            mean_normal = np.log(Mean) - 0.5 * sigma_normal**2
            incrementals[i,j] = np.random.lognormal(mean=mean_normal, sigma=sigma_normal)

        else:
            incrementals[i,j] = np.random.normal(loc=Mean, scale=SD)
        
        cumulatives[i,j] = cumulatives[i,j-1] + incrementals[i,j]

  complete_forecast = cumulatives

  Ultimates = complete_forecast[:,-1]
  Paid = np.diag(np.fliplr(tridata))
  Reserves = Ultimates - Paid
  TotalReserve = np.nansum(Reserves)

  result = {"Cumulatives": cumulatives, 
            "Ultimates": Ultimates, 
            "Reserves": Reserves, 
            "TotalReserve": TotalReserve, 
            "Complete_Forecast": complete_forecast}

  
  return result

def Run_MCMC(triangle, method, sims_per_chain=1000, chains=1, seed = np.random.randint(1, 1000000),
                  ForecastDist="Gamma", UserSqrtScale=None):
    # Control function to run MCMC based on selected method
    
    if method == "Mack":
        result = Main_Mack_MCMC(triangle, sims_per_chain=sims_per_chain, chains=chains, seed=seed, 
                                ForecastDist=ForecastDist, UserSigma=UserSqrtScale)
    elif method == "ODPConstant":
        Scale = "Constant"
        result = Main_ODP_MCMC(triangle, sims_per_chain=sims_per_chain, chains=chains, seed=seed,
                                Scale=Scale, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)
    elif method == "ODPNonConstant":
        Scale = "NonConstant"
        result = Main_ODP_MCMC(triangle, sims_per_chain=sims_per_chain, chains=chains, seed=seed,
                                Scale=Scale, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)
    elif method == "NegBinConstant":
        Scale = "Constant"
        result = Main_NegBin_MCMC(triangle, sims_per_chain=sims_per_chain, chains=chains, seed=seed,
                                   Scale=Scale, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)
    elif method == "NegBinNonConstant":
        Scale = "NonConstant"
        result = Main_NegBin_MCMC(triangle, sims_per_chain=sims_per_chain, chains=chains, seed=seed,
                                   Scale=Scale, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

    return result