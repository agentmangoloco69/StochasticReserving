# Predictive distributions of outstanding liabilities in general insurance.
#
# Python code to reproduce the tables in:
# England & Verrall (2006). Predictive distributions of outstanding liabilities in general insurance.
# Annals of Actuarial Science, 1, II, 221-270. https://doi.org/10.1017/S1748499500000142
#
# Supports Mack's model, Over-dispersed Poisson (ODP), and Over-dispersed Negative Binomial models.
# Select the model via the `method` variable below.

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

import StochResFunctions as srf
import StochResFunctions_MCMC as srfmcmc

try:
    from cmdstanpy import CmdStanModel
    import logging
    logging.getLogger("cmdstanpy").setLevel(logging.CRITICAL)
    MCMC_AVAILABLE = True
except ImportError:
    print("Warning: cmdstanpy not installed. MCMC section will be skipped.")
    MCMC_AVAILABLE = False

# =============================================================================
# Settings
# =============================================================================

Inc_Triangle = pd.read_csv("claims_triangle.csv", index_col=0).to_numpy()
Triangle = srf.Cumulatives(Inc_Triangle)

# Set Mask to exclude link ratios (1=include, 0=exclude). None means all included.
Mask = np.ones((len(Triangle[0]) - 1, len(Triangle[0]) - 1))

# method = "Mack", "ODPConstant", "ODPNonConstant", "NegBinConstant", or "NegBinNonConstant"
method = "ODPConstant"

Seed = 101
iterations = 10000

# Bootstrap: pseudo-data distribution ("Gamma", "Lognormal", or "NonParametric")
BootstrapDist = "Gamma"

# Bootstrap: process (forecast) distribution ("Gamma", "Lognormal", or "NonParametric")
ForecastDist = "Gamma"

# MCMC
chains = 4
sims_per_chain = round(iterations / chains)

# =============================================================================
# Chain Ladder Results
# =============================================================================

LRM_factors = srf.LinkRatioMethod(Triangle, Mask=Mask)
LRs = LRM_factors["LR_Triangle"]
LR_Weights = LRM_factors["LR_Weights"]
CL_facs = LRM_factors["LRM_Factors"]

ChainLadderResult = srf.LinkRatioMethod_forecast(Triangle, CL_facs)
Latest = ChainLadderResult["Latest"]
CL_Ults = ChainLadderResult["Ultimates"]
CL_Res = ChainLadderResult["Reserves"]
TotalReserves = ChainLadderResult["TotalReserves"]
TotalLatest = ChainLadderResult["TotalLatest"]
TotalUltimates = ChainLadderResult["TotalUltimates"]

title = "Incremental Claims Data"
srf.table_plot(srf.arrayRound_and_format(Inc_Triangle, 0),
               ["OP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               ["DP " + str(i) for i in range(1, len(Inc_Triangle) + 1)], title)
srf.line_plot(Inc_Triangle, "Claim Amounts By Development Period (Incremental)",
              "Development Period", "Claim Amounts", "Origin Period", withZeroPoint=True)

title = "Cumulative Claims Data"
srf.table_plot(srf.arrayRound_and_format(Triangle, 0),
               ["OP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               ["DP " + str(i) for i in range(1, len(Inc_Triangle) + 1)], title)
srf.line_plot(Triangle, "Claim Amounts By Development Period (Cumulative)",
              "Development Period", "Claim Amounts", "Origin Period", withZeroPoint=True)

rowlabels_CL = [*["OP " + str(i) for i in range(1, len(LRs) + 1)], "CL Factors"]
CLPlot = srf.arrayRound_and_format(np.vstack((srf.triangleUpper(LRs), CL_facs)), 3)
srf.table_plot(CLPlot, rowlabels_CL,
               ["DP " + str(i) for i in range(1, len(LRs) + 1)], "Link ratios table")
srf.line_plot(LRs, "Link Ratios By Development Period",
              "Development Period", "Link Ratios", "Origin Period")

LatestPlot = srf.arrayRound_and_format(np.array([*Latest, TotalLatest]).reshape(-1, 1), 0)
UltimatesPlot = srf.arrayRound_and_format(np.array([*CL_Ults, TotalUltimates]).reshape(-1, 1), 0)
ReservesPlot = srf.arrayRound_and_format(np.array([*CL_Res, TotalReserves]).reshape(-1, 1), 0)
Table_2 = np.hstack((LatestPlot, ReservesPlot, UltimatesPlot))

srf.table_plot(Table_2,
               [*["OP " + str(i) for i in range(1, len(Table_2))], "Total"],
               ["Latest", "Reserves", "Ultimates"], "Chain Ladder Results")

# =============================================================================
# Analytic Results (closed-form SDs)
# =============================================================================

Analytic_Result = srf.Calc_RMSEPs(Triangle, method, Mask=Mask)

coefs = Analytic_Result["Model"]["coefs"]
parameter_se = Analytic_Result["Model"]["parameter_se"]
Latest = Analytic_Result["Latest"]
Reserves = Analytic_Result["Reserves"]
TotalReserve = Analytic_Result["TotalReserves"]
Ultimates = Analytic_Result["Ultimates"]
ReserveSDs = Analytic_Result["Reserves_SD"]
ReserveCoVs = Analytic_Result["Reserves_CoV"]
TotalReserveSD = Analytic_Result["TotalReserve_SD"]
TotalReserveCoV = Analytic_Result["TotalReserve_CoV"]

CL_Reserves = np.array([*Reserves, TotalReserve]).reshape(-1, 1)
SDs = np.array([*ReserveSDs, TotalReserveSD]).reshape(-1, 1)
CoVs = np.array([*ReserveCoVs, TotalReserveCoV]).reshape(-1, 1)

title = "Max likelihood parameter estimates and standard errors\n(" + method + ")"
Coefs_Plot = srf.arrayRound_and_format(np.array(coefs).reshape(-1, 1), 3)
Coefs_SE_Plot = srf.arrayRound_and_format(np.array(parameter_se).reshape(-1, 1), 3)
ML_Ests_Plot = np.hstack((Coefs_Plot, Coefs_SE_Plot))
if method in ("ODPConstant", "ODPNonConstant"):
    srf.table_plot(ML_Ests_Plot,
                   ["Intercept", *["OP " + str(i) for i in range(2, len(Inc_Triangle) + 1)],
                    *["DP " + str(i) for i in range(2, len(Inc_Triangle) + 1)]],
                   ["Parameter estimate", "Standard error"], title)
else:
    srf.table_plot(ML_Ests_Plot,
                   [*["DP " + str(i) for i in range(2, len(Inc_Triangle) + 1)]],
                   ["Parameter estimate", "Standard error"], title)

LatestPlot = srf.arrayRound_and_format(np.array([*Latest, np.sum(Latest)]).reshape(-1, 1), 0)
ReservesPlot = srf.arrayRound_and_format(CL_Reserves, 0)
UltimatesPlot = srf.arrayRound_and_format(np.array([*Ultimates, np.sum(Ultimates)]).reshape(-1, 1), 0)
SD_Plot = srf.arrayRound_and_format(SDs, 0)
CoV_Plot = srf.arrayRound_and_format(CoVs * 100, 1)
Table_3 = np.hstack((LatestPlot, ReservesPlot, UltimatesPlot, SD_Plot, CoV_Plot))

srf.table_plot(Table_3,
               [*["OP " + str(i) for i in range(1, len(Table_3))], "Total"],
               ["Latest", "Reserves", "Ultimates", "Reserves SD", "Reserves CoV%"],
               "Chain Ladder Results - Max Likelihood\n(" + method + ")")

# =============================================================================
# Residuals
# =============================================================================

Resids = srf.Calc_Residuals(Triangle, method=method, Mask=Mask)
sigma = Resids["sqrtScale"]

label = "Mack's Sigma" if method == "Mack" else "SqrtScale"
titles = ["Adjusted Unscaled Residuals", "Zero-Average Adjusted Scaled Residuals"]
Resids_List = [Resids["adj_unscaled_resids"], Resids["zeroavg_adj_scaled_resids"]]

for i in range(len(titles)):
    title = titles[i]
    R = srf.triangleUpper(Resids_List[i])
    Resids_tmp = srf.arrayRound_and_format(np.vstack((R, sigma)), 3)
    srf.table_plot(Resids_tmp,
                   [*["OP " + str(j) for j in range(1, len(Resids_tmp))], label],
                   ["DP " + str(j) for j in range(1, len(Resids_tmp))],
                   title + "\n(" + method + ")")
    srf.scatter_plot(R, title + " by Origin Period\n(" + method + ")",
                     "Origin Period", title, rowData=False, sigma=sigma)
    srf.scatter_plot(R, title + " by Development Period\n(" + method + ")",
                     "Development Period", title, sigma=sigma)
    srf.scatter_plot(R, title + " by Calendar Period\n(" + method + ")",
                     "Calendar Period", title, calendarData=True, sigma=sigma)

# =============================================================================
# Bootstrap
# =============================================================================

Bstrap_Result = srf.Run_Bootstrap(Triangle, method=method, Mask=Mask, iterations=iterations,
                                   seed=Seed, BootstrapDist=BootstrapDist,
                                   ForecastDist=ForecastDist, UserSqrtScale=None)

Bstrap_Latest = Bstrap_Result["Latest"]
Bstrap_AvgRes = Bstrap_Result["Avg_Reserve"]
Bstrap_AvgUlts = [np.nansum((Bstrap_AvgRes[i], Bstrap_Latest[i])) for i in range(len(Bstrap_AvgRes))]
Bstrap_SDReserve = Bstrap_Result["SD_Reserve"]
Bstrap_CoVReserve = Bstrap_Result["CoV_Reserve"]
Total_Bstrap_AvgReserve = Bstrap_Result["Avg_TotalReserve"]
Total_BstrapLatest = sum(Bstrap_Latest)
Total_Bstrap_AvgUltimates = sum(Bstrap_AvgUlts)
Total_Bstrap_SDReserve = Bstrap_Result["SD_TotalReserve"]
Total_Bstrap_CoVReserve = Bstrap_Result["CoV_TotalReserve"]

Bstrap_Latest_Plot = srf.arrayRound_and_format(
    np.array([*Bstrap_Latest, Total_BstrapLatest]).reshape(-1, 1), 0)
Bstrap_AvgUltimate_Plot = srf.arrayRound_and_format(
    np.array([*Bstrap_AvgUlts, Total_Bstrap_AvgUltimates]).reshape(-1, 1), 0)
Bstrap_AvgReserve_Plot = srf.arrayRound_and_format(
    np.array([*Bstrap_AvgRes, Total_Bstrap_AvgReserve]).reshape(-1, 1), 0)
Bstrap_SDReserve_Plot = srf.arrayRound_and_format(
    np.array([*Bstrap_SDReserve, Total_Bstrap_SDReserve]).reshape(-1, 1), 0)
Bstrap_CoVReserve_Plot = srf.arrayRound_and_format(
    np.array([100 * i for i in [*Bstrap_CoVReserve, Total_Bstrap_CoVReserve]]).reshape(-1, 1), 1)

title = "Bootstrap summary, " + str(Bstrap_Result["iterations"]) + " iterations\n(" + method + ")"
table = np.hstack((Bstrap_Latest_Plot, Bstrap_AvgReserve_Plot, Bstrap_AvgUltimate_Plot,
                   Bstrap_SDReserve_Plot, Bstrap_CoVReserve_Plot))
srf.table_plot(table,
               [*["OP " + str(i) for i in range(1, len(table))], "Total"],
               ["Latest", "Avg Reserves", "Avg Ultimates", "Bstrap SD", "Bstrap CoV (%)"], title)

srf.ShowSummaryStats(Bstrap_Result, Output="Reserves")

Forecast_Cumulatives = Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# MCMC
# =============================================================================

if MCMC_AVAILABLE:
    MCMC_Result = srfmcmc.Run_MCMC(Triangle, method=method, sims_per_chain=sims_per_chain,
                                    chains=chains, seed=Seed, ForecastDist=ForecastDist,
                                    UserSqrtScale=None)

    MCMC_coefs_mean = MCMC_Result["Coefs_Mean"]
    MCMC_coefs_SE = MCMC_Result["Coefs_SD"]

    MCMC_Coefs_Plot = srf.arrayRound_and_format(np.array(MCMC_coefs_mean).reshape(-1, 1), 3)
    MCMC_Coefs_SE_Plot = srf.arrayRound_and_format(np.array(MCMC_coefs_SE).reshape(-1, 1), 3)
    ML_Ests_Plot = np.hstack((Coefs_Plot, Coefs_SE_Plot, MCMC_Coefs_Plot, MCMC_Coefs_SE_Plot))
    if method in ("ODPConstant", "ODPNonConstant"):
        srf.table_plot(ML_Ests_Plot,
                       ["Intercept", *["OP " + str(i) for i in range(2, len(Inc_Triangle) + 1)],
                        *["DP " + str(i) for i in range(2, len(Inc_Triangle) + 1)]],
                       ["ML Parameter estimate", "ML Standard error", "MCMC Mean", "MCMC Standard Error"],
                       "Parameter estimates and standard errors\n" + method)
    else:
        srf.table_plot(ML_Ests_Plot,
                       [*["DP " + str(i) for i in range(2, len(Inc_Triangle) + 1)]],
                       ["ML Parameter estimate", "ML Standard error", "MCMC Mean", "MCMC Standard Error"],
                       "Parameter estimates and standard errors\n" + method)

    MCMC_Latest = MCMC_Result["Latest"]
    MCMC_AvgRes = MCMC_Result["Avg_Reserve"]
    MCMC_AvgUlts = [np.nansum((MCMC_AvgRes[i], MCMC_Latest[i])) for i in range(len(MCMC_AvgRes))]
    MCMC_SDReserve = MCMC_Result["SD_Reserve"]
    MCMC_CoVReserve = MCMC_Result["CoV_Reserve"]
    Total_MCMC_AvgReserve = MCMC_Result["Avg_TotalReserve"]
    Total_MackMCMCLatest = sum(MCMC_Latest)
    Total_MCMC_AvgUltimates = sum(MCMC_AvgUlts)
    Total_MCMC_SDReserve = MCMC_Result["SD_TotalReserve"]
    Total_MCMC_CoVReserve = MCMC_Result["CoV_TotalReserve"]

    MCMC_Latest_Plot = srf.arrayRound_and_format(
        np.array([*MCMC_Latest, Total_MackMCMCLatest]).reshape(-1, 1), 0)
    MCMC_AvgUltimate_Plot = srf.arrayRound_and_format(
        np.array([*MCMC_AvgUlts, Total_MCMC_AvgUltimates]).reshape(-1, 1), 0)
    MCMC_AvgReserve_Plot = srf.arrayRound_and_format(
        np.array([*MCMC_AvgRes, Total_MCMC_AvgReserve]).reshape(-1, 1), 0)
    MCMC_SDReserve_Plot = srf.arrayRound_and_format(
        np.array([*MCMC_SDReserve, Total_MCMC_SDReserve]).reshape(-1, 1), 0)
    MCMC_CoVReserve_Plot = srf.arrayRound_and_format(
        np.array([100 * i for i in [*MCMC_CoVReserve, Total_MCMC_CoVReserve]]).reshape(-1, 1), 1)

    title = "MCMC summary, " + str(MCMC_Result["iterations"]) + " iterations\n" + method
    table = np.hstack((MCMC_Latest_Plot, MCMC_AvgReserve_Plot, MCMC_AvgUltimate_Plot,
                       MCMC_SDReserve_Plot, MCMC_CoVReserve_Plot))
    srf.table_plot(table,
                   [*["OP " + str(i) for i in range(1, len(table))], "Total"],
                   ["Latest", "Avg Reserves", "Avg Ultimates", "MCMC SD", "MCMC CoV (%)"], title)

    srf.ShowSummaryStats(MCMC_Result, Output="Reserves")

    Forecast_Cumulatives = MCMC_Result["Complete_Cumulatives"]
    for op in [1, 3, 5, 7, 9]:
        srf.fan_plot(op, Forecast_Cumulatives)

    # =============================================================================
    # Bootstrap and MCMC Combined Summary
    # =============================================================================

    title = "MCMC and Bootstrap summary\n" + method
    table_9 = np.hstack((MCMC_AvgReserve_Plot, MCMC_SDReserve_Plot, MCMC_CoVReserve_Plot,
                          Bstrap_AvgReserve_Plot, Bstrap_SDReserve_Plot, Bstrap_CoVReserve_Plot))
    srf.table_plot(table_9,
                   [*["OP " + str(i) for i in range(1, len(table_9))], "Total"],
                   ["MCMC Avg Reserves", "MCMC SD", "MCMC CoV (%)",
                    "Bstrap Avg Reserves", "Bstrap SD", "Bstrap CoV (%)"], title)
