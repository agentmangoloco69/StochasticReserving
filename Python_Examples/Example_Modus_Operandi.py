# Stochastic Reserving: Modus Operandi
#
# Python code demonstrating how to quantify variability in chain ladder reserve estimates
# using analytic (closed form) and bootstrap methods.
#
# Steps 1-15 follow a structured workflow for analysing a claims triangle,
# from data inspection through to scaling results to target ultimates.
# The lifetime ("ultimo") view of reserve risk is covered here.
# For the one-year view, see EVW_2019.py.

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

import StochResFunctions as srf

# =============================================================================
# Steps 1 & 2: Look at the data and fit a baseline chain ladder model
# =============================================================================

Inc_Triangle = pd.read_csv("liability_claims_triangle.csv", index_col=0).to_numpy()
Triangle = srf.Cumulatives(Inc_Triangle)

LRM_factors = srf.LinkRatioMethod(Triangle)
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

srf.table_plot(srf.arrayRound_and_format(Inc_Triangle, 0),
               ["OP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               ["DP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               "Incremental Claims Data")
srf.line_plot(Inc_Triangle, "Claim Amounts By Development Period (Incremental)",
              "Development Period", "Claim Amounts", "Origin Period", withZeroPoint=True)

srf.table_plot(srf.arrayRound_and_format(Triangle, 0),
               ["OP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               ["DP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               "Cumulative Claims Data")
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
# Step 3: Apply Mack's method analytically
# =============================================================================

Mack_Analytic_Result = srf.Mack_ChainLadder(Triangle, Mask=None)

coefs = Mack_Analytic_Result["coefs"]
parameter_se = Mack_Analytic_Result["parameter_se"]
LinkRatios = Mack_Analytic_Result["LinkRatios"]
LinkRatiosSE = Mack_Analytic_Result["LinkRatiosSE"]
sigma = Mack_Analytic_Result["sigma"]
Mack_Latest = Mack_Analytic_Result["Latest"]
Mack_Reserves = Mack_Analytic_Result["Reserves"]
Mack_TotalReserve = Mack_Analytic_Result["TotalReserves"]
Mack_Ultimates = Mack_Analytic_Result["Ultimates"]
Mack_ReserveSDs = Mack_Analytic_Result["Reserves_SD"]
Mack_ReserveCoVs = Mack_Analytic_Result["Reserves_CoV"]
Mack_TotalReserveSD = Mack_Analytic_Result["TotalReserve_SD"]
Mack_TotalReserveCoV = Mack_Analytic_Result["TotalReserve_CoV"]

CL_Reserves = np.array([*Mack_Reserves, Mack_TotalReserve]).reshape(-1, 1)
Mack_SDs = np.array([*Mack_ReserveSDs, Mack_TotalReserveSD]).reshape(-1, 1)
Mack_CoVs = np.array([*Mack_ReserveCoVs, Mack_TotalReserveCoV]).reshape(-1, 1)

LatestPlot = srf.arrayRound_and_format(np.array([*Mack_Latest, np.sum(Mack_Latest)]).reshape(-1, 1), 0)
UltimatesPlot = srf.arrayRound_and_format(np.array([*Mack_Ultimates, np.sum(Mack_Ultimates)]).reshape(-1, 1), 0)
ReservesPlot = srf.arrayRound_and_format(CL_Reserves, 0)
Mack_SD_Plot = srf.arrayRound_and_format(Mack_SDs, 0)
Mack_CoV_Plot = srf.arrayRound_and_format(Mack_CoVs * 100, 1)
Table_2_Mack = np.hstack((LatestPlot, ReservesPlot, UltimatesPlot, Mack_SD_Plot, Mack_CoV_Plot))

srf.table_plot(Table_2_Mack,
               [*["OP " + str(i) for i in range(1, len(Table_2_Mack))], "Total"],
               ["Latest", "Reserves", "Ultimates", "Reserves SD", "Reserves CoV%"],
               "Analytic Mack Chain Ladder Results\n(using recursive formulae from England & Verrall (2002))")

# =============================================================================
# Step 4: Residuals and variance parameters for Mack's model
# =============================================================================

method = "Mack"
Resids = srf.Calc_Residuals(Triangle, method=method, Mask=None)
sigma = Resids["sqrtScale"]
sigma_original = np.copy(sigma)

titles = ["Adjusted Unscaled Residuals", "Zero-Average Adjusted Scaled Residuals"]
Resids_List = [Resids["adj_unscaled_resids"], Resids["zeroavg_adj_scaled_resids"]]

for i in range(len(titles)):
    title = titles[i]
    R = srf.triangleUpper(Resids_List[i])
    Resids_tmp = srf.arrayRound_and_format(np.vstack((R, sigma)), 3)
    srf.table_plot(Resids_tmp,
                   [*["OP " + str(j) for j in range(1, len(Resids_tmp))], "Mack's Sigma"],
                   ["DP " + str(j) for j in range(1, len(Resids_tmp))], title)
    srf.scatter_plot(R, title + " by Origin Period\n(" + method + ")",
                     "Origin Period", title, rowData=False, sigma=sigma)
    srf.scatter_plot(R, title + " by Development Period\n(" + method + ")",
                     "Development Period", title, sigma=sigma)
    srf.scatter_plot(R, title + " by Calendar Period\n(" + method + ")",
                     "Calendar Period", title, calendarData=True, sigma=sigma)

# =============================================================================
# Step 5: Bootstrap Mack's model with all ratios included
# =============================================================================

Seed = 100
iterations = 10000
BootstrapDist = "Gamma"
ForecastDist = "Gamma"

Mack_Bstrap_Result = srf.Run_Bootstrap(Triangle, method="Mack", Mask=None, iterations=iterations,
                                         seed=Seed, BootstrapDist=BootstrapDist,
                                         ForecastDist=ForecastDist, UserSqrtScale=None)

print("Mack's model bootstrap results: Reserves")
srf.ShowSummaryStats(Mack_Bstrap_Result, Output="Reserves")
print("Mack's model bootstrap results: Ultimates")
srf.ShowSummaryStats(Mack_Bstrap_Result, Output="Ultimates")

# Fan charts
Forecast_Cumulatives = Mack_Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Step 6: Sensitivity analysis to identify influential link ratios
# =============================================================================

Sensitivity_Results = srf.Sensitivities(Triangle)

S_Reserves = Sensitivity_Results["S_Reserves"]
S_Reserves_diff = Sensitivity_Results["S_Reserves_diff"]
S_Reserves_absdiff = Sensitivity_Results["S_Reserves_absdiff"]
S_Reserves_rank = Sensitivity_Results["S_Reserves_rank"]
S_ReservesSD = Sensitivity_Results["S_ReservesSD"]
S_ReservesSD_diff = Sensitivity_Results["S_ReservesSD_diff"]
S_ReservesSD_absdiff = Sensitivity_Results["S_ReservesSD_absdiff"]
S_ReservesSD_rank = Sensitivity_Results["S_ReservesSD_rank"]
S_ReservesCoV = Sensitivity_Results["S_ReservesCoV"]
S_ReservesCoV_diff = Sensitivity_Results["S_ReservesCoV_diff"]
S_ReservesCoV_absdiff = Sensitivity_Results["S_ReservesCoV_absdiff"]
S_ReservesCoV_rank = Sensitivity_Results["S_ReservesCoV_rank"]

rowlabels = [*["OP " + str(i) for i in range(1, len(S_Reserves) + 1)]]
collabels = ["DP " + str(i) for i in range(1, len(S_Reserves) + 1)]

srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_Reserves))), 0),
               rowlabels, collabels, "Total Reserves table")
srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_Reserves_diff))), 0),
               rowlabels, collabels, "Reserves difference table")
srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_Reserves_rank))), 0),
               rowlabels, collabels, "Reserves ranks table\n(based on largest reduction)")

srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_ReservesSD))), 0),
               rowlabels, collabels, "Total Reserves SD table")
srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_ReservesSD_diff))), 0),
               rowlabels, collabels, "Reserves SD difference table")
srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_ReservesSD_rank))), 0),
               rowlabels, collabels, "Reserves SD ranks table\n(based on largest reduction)")

srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_ReservesCoV * 100))), 1),
               rowlabels, collabels, "Total Reserves CoV% table")
srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_ReservesCoV_diff * 100))), 1),
               rowlabels, collabels, "Reserves CoV% difference table")
srf.table_plot(srf.arrayRound_and_format(np.vstack((srf.triangleUpper(S_ReservesCoV_rank))), 0),
               rowlabels, collabels, "Reserves CoV% ranks table\n(based on largest reduction)")

# =============================================================================
# Step 7: Exclude top 3 influential ratios and re-apply Mack's method analytically
# =============================================================================

n = len(Triangle[0]) - 1
Masktmp = np.ones((n, n))

for rank_num in [1, 2, 3]:
    positions = np.argwhere(S_ReservesSD_rank == rank_num)
    pos = positions[0]
    Masktmp[pos[0], pos[1]] = 0

Mack_Analytic_Result = srf.Mack_ChainLadder(Triangle, Mask=Masktmp)

coefs = Mack_Analytic_Result["coefs"]
parameter_se = Mack_Analytic_Result["parameter_se"]
LinkRatios = Mack_Analytic_Result["LinkRatios"]
LinkRatiosSE = Mack_Analytic_Result["LinkRatiosSE"]
sigma = Mack_Analytic_Result["sigma"]
Mack_Latest = Mack_Analytic_Result["Latest"]
Mack_Reserves = Mack_Analytic_Result["Reserves"]
Mack_TotalReserve = Mack_Analytic_Result["TotalReserves"]
Mack_Ultimates = Mack_Analytic_Result["Ultimates"]
Mack_ReserveSDs = Mack_Analytic_Result["Reserves_SD"]
Mack_ReserveCoVs = Mack_Analytic_Result["Reserves_CoV"]
Mack_TotalReserveSD = Mack_Analytic_Result["TotalReserve_SD"]
Mack_TotalReserveCoV = Mack_Analytic_Result["TotalReserve_CoV"]

CL_Reserves = np.array([*Mack_Reserves, Mack_TotalReserve]).reshape(-1, 1)
Mack_SDs = np.array([*Mack_ReserveSDs, Mack_TotalReserveSD]).reshape(-1, 1)
Mack_CoVs = srf.safe_divide(Mack_SDs, CL_Reserves)

LatestPlot = srf.arrayRound_and_format(np.array([*Mack_Latest, np.sum(Mack_Latest)]).reshape(-1, 1), 0)
UltimatesPlot = srf.arrayRound_and_format(np.array([*Mack_Ultimates, np.sum(Mack_Ultimates)]).reshape(-1, 1), 0)
ReservesPlot = srf.arrayRound_and_format(CL_Reserves, 0)
Mack_SD_Plot = srf.arrayRound_and_format(Mack_SDs, 0)
Mack_CoV_Plot = srf.arrayRound_and_format(Mack_CoVs * 100, 1)
Table_2_Mack = np.hstack((LatestPlot, ReservesPlot, UltimatesPlot, Mack_SD_Plot, Mack_CoV_Plot))

srf.table_plot(Table_2_Mack,
               [*["OP " + str(i) for i in range(1, len(Table_2_Mack))], "Total"],
               ["Latest", "Reserves", "Ultimates", "Reserves SD", "Reserves CoV%"],
               "Analytic Mack Chain Ladder Results (excl. top 3 influential ratios)\n"
               "(using recursive formulae from England & Verrall (2002))")

# =============================================================================
# Step 8: Residuals and variance parameters after excluding influential ratios
# =============================================================================

method = "Mack"
Mack_Resids = srf.Calc_Residuals(Triangle, method=method, Mask=Masktmp)
sigma = Mack_Resids["sqrtScale"]

titles = ["Adjusted Unscaled Residuals", "Zero-Average Adjusted Scaled Residuals"]
Resids_List = [Mack_Resids["adj_unscaled_resids"], Mack_Resids["zeroavg_adj_scaled_resids"]]

for i in range(len(titles)):
    title = titles[i]
    R = srf.triangleUpper(Resids_List[i])
    Resids_tmp = srf.arrayRound_and_format(np.vstack((R, sigma)), 3)
    srf.table_plot(Resids_tmp,
                   [*["OP " + str(j) for j in range(1, len(Resids_tmp))], "Mack's Sigma"],
                   ["DP " + str(j) for j in range(1, len(Resids_tmp))], title)
    srf.scatter_plot(R, title + " by Origin Period\n(" + method + ")",
                     "Origin Period", title, rowData=False, sigma=sigma)
    srf.scatter_plot(R, title + " by Development Period\n(" + method + ")",
                     "Development Period", title, sigma=sigma)
    srf.scatter_plot(R, title + " by Calendar Period\n(" + method + ")",
                     "Calendar Period", title, calendarData=True, sigma=sigma)

# =============================================================================
# Step 9: Bootstrap Mack's model excluding top 3 influential ratios
# =============================================================================

Mack_Bstrap_Result = srf.Run_Bootstrap(Triangle, method="Mack", Mask=Masktmp, iterations=iterations,
                                         seed=Seed, BootstrapDist=BootstrapDist,
                                         ForecastDist=ForecastDist, UserSqrtScale=None)

print("Mack's model bootstrap results: Reserves")
srf.ShowSummaryStats(Mack_Bstrap_Result, Output="Reserves")
print("Mack's model bootstrap results: Ultimates")
srf.ShowSummaryStats(Mack_Bstrap_Result, Output="Ultimates")

Forecast_Cumulatives = Mack_Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Step 10: Manually adjust variance parameters (Mack's sigma)
# =============================================================================

UserSqrtScale = sigma_original.copy()
UserSqrtScale[5] = 13

Mack_Bstrap_Result = srf.Run_Bootstrap(Triangle, method="Mack", Mask=None, iterations=iterations,
                                         seed=Seed, BootstrapDist=BootstrapDist,
                                         ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

print("Mack's model bootstrap results: Reserves")
srf.ShowSummaryStats(Mack_Bstrap_Result, Output="Reserves")
print("Mack's model bootstrap results: Ultimates")
srf.ShowSummaryStats(Mack_Bstrap_Result, Output="Ultimates")

Forecast_Cumulatives = Mack_Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Step 11: ODP model with non-constant scale parameters
# =============================================================================

method = "ODPNonConstant"
ODP_Resids = srf.Calc_Residuals(Triangle, method=method, Mask=None)
sqrtScale = ODP_Resids["sqrtScale"]
sqrtScale_original = np.copy(sqrtScale)

titles = ["Adjusted Unscaled Residuals", "Zero-Average Adjusted Scaled Residuals"]
Resids_List = [ODP_Resids["adj_unscaled_resids"], ODP_Resids["zeroavg_adj_scaled_resids"]]

for i in range(len(titles)):
    title = titles[i]
    R = srf.triangleUpper(Resids_List[i])
    Resids_tmp = srf.arrayRound_and_format(np.vstack((R, sqrtScale)), 3)
    srf.table_plot(Resids_tmp,
                   [*["OP " + str(j) for j in range(1, len(Resids_tmp))], "SqrtScale"],
                   ["DP " + str(j) for j in range(1, len(Resids_tmp))], title)
    srf.scatter_plot(R, title + " by Origin Period\n(" + method + ")",
                     "Origin Period", title, rowData=False, sigma=sqrtScale)
    srf.scatter_plot(R, title + " by Development Period\n(" + method + ")",
                     "Development Period", title, sigma=sqrtScale)
    srf.scatter_plot(R, title + " by Calendar Period\n(" + method + ")",
                     "Calendar Period", title, calendarData=True, sigma=sqrtScale)

ODP_Bstrap_Result = srf.Run_Bootstrap(Triangle, method="ODPNonConstant", Mask=None, iterations=iterations,
                                        seed=Seed, BootstrapDist=BootstrapDist,
                                        ForecastDist=ForecastDist, UserSqrtScale=None)

print("ODP model bootstrap results: Reserves")
srf.ShowSummaryStats(ODP_Bstrap_Result, Output="Reserves")
print("ODP model bootstrap results: Ultimates")
srf.ShowSummaryStats(ODP_Bstrap_Result, Output="Ultimates")

Forecast_Cumulatives = ODP_Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Step 12: ODP model excluding influential ratios
# =============================================================================

method = "ODPNonConstant"
ODP_Resids = srf.Calc_Residuals(Triangle, method=method, Mask=Masktmp)
sqrtScale = ODP_Resids["sqrtScale"]

titles = ["Adjusted Unscaled Residuals", "Zero-Average Adjusted Scaled Residuals"]
Resids_List = [ODP_Resids["adj_unscaled_resids"], ODP_Resids["zeroavg_adj_scaled_resids"]]

for i in range(len(titles)):
    title = titles[i]
    R = srf.triangleUpper(Resids_List[i])
    Resids_tmp = srf.arrayRound_and_format(np.vstack((R, sqrtScale)), 3)
    srf.table_plot(Resids_tmp,
                   [*["OP " + str(j) for j in range(1, len(Resids_tmp))], "SqrtScale"],
                   ["DP " + str(j) for j in range(1, len(Resids_tmp))], title)
    srf.scatter_plot(R, title + " by Origin Period\n(" + method + ")",
                     "Origin Period", title, rowData=False, sigma=sqrtScale)
    srf.scatter_plot(R, title + " by Development Period\n(" + method + ")",
                     "Development Period", title, sigma=sqrtScale)
    srf.scatter_plot(R, title + " by Calendar Period\n(" + method + ")",
                     "Calendar Period", title, calendarData=True, sigma=sqrtScale)

ODP_Bstrap_Result = srf.Run_Bootstrap(Triangle, method="ODPNonConstant", Mask=Masktmp, iterations=iterations,
                                        seed=Seed, BootstrapDist=BootstrapDist,
                                        ForecastDist=ForecastDist, UserSqrtScale=None)

print("ODP model bootstrap results: Reserves")
srf.ShowSummaryStats(ODP_Bstrap_Result, Output="Reserves")
print("ODP model bootstrap results: Ultimates")
srf.ShowSummaryStats(ODP_Bstrap_Result, Output="Ultimates")

Forecast_Cumulatives = ODP_Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Step 13: Manually adjust ODP variance parameters (sqrtScale)
# =============================================================================

UserSqrtScale = sqrtScale_original.copy()
UserSqrtScale[6] = 40

ODP_Bstrap_Result = srf.Run_Bootstrap(Triangle, method="ODPNonConstant", Mask=None, iterations=iterations,
                                        seed=Seed, BootstrapDist=BootstrapDist,
                                        ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

print("ODP model bootstrap results: Reserves")
srf.ShowSummaryStats(ODP_Bstrap_Result, Output="Reserves")
print("ODP model bootstrap results: Ultimates")
srf.ShowSummaryStats(ODP_Bstrap_Result, Output="Ultimates")

Forecast_Cumulatives = ODP_Bstrap_Result["Complete_Cumulatives"]
for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Steps 14 & 15: Final model selection and scaling to target ultimates
# =============================================================================

Unscaled_Results = ODP_Bstrap_Result

# Target ultimates: latest + 110% of mean reserve (example only; use best estimate in practice)
Target_Ultimates = Unscaled_Results["Latest"] + Unscaled_Results["Avg_Reserve"] * 1.1

num_origin_periods = Unscaled_Results["Reserves"].shape[1]
# Additive for first 5 periods (preserves absolute SD), multiplicative for last 5 (preserves CoV)
scaling_method = np.concatenate((['Additive'] * 5, ['Multiplicative'] * 5))

Scaled_Results = srf.Scaled_Results(Unscaled_Results, Target_Ultimates, scaling_method)

print("Bootstrap results: Unscaled")
srf.ShowSummaryStats(Unscaled_Results, Output="Reserves")

print("Bootstrap results: Scaled")
srf.ShowSummaryStats(Scaled_Results, Output="Reserves")
