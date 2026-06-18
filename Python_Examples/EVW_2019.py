# On the lifetime and one-year views of reserve risk, with application to IFRS 17 and Solvency II risk margins.
#
# Python code to reproduce the tables in:
# England, Verrall & Wuthrich (2018/2019). On the lifetime and one-year views of reserve risk,
# with application to IFRS 17 and Solvency II risk margins.
# Insurance: Mathematics and Economics (2019). https://doi.org/10.1016/j.insmatheco.2018.12.002
#
# Supports Mack's model, ODP, and Negative Binomial. Select via `method` below.

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy.optimize import brentq

import StochResFunctions as srf

# =============================================================================
# Settings
# =============================================================================

Inc_Triangle = pd.read_csv("claims_triangle.csv", index_col=0).to_numpy()
Triangle = srf.Cumulatives(Inc_Triangle)

# Set Mask to exclude link ratios (1=include, 0=exclude). None means all included.
Mask = np.ones((len(Triangle[0]) - 1, len(Triangle[0]) - 1))
# Mask[2, 5] = 0  # example exclusion

# method = "Mack", "ODPConstant", "ODPNonConstant", "NegBinConstant", or "NegBinNonConstant"
method = "Mack"

Seed = 101
iterations = 10000

BootstrapDist = "Gamma"
ForecastDist = "Gamma"

# =============================================================================
# Chain Ladder Results (Table 1 equivalent)
# =============================================================================

LRM_factors = srf.LinkRatioMethod(Triangle, Mask)
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
               "Incremental claims data")
srf.line_plot(Inc_Triangle, "Claim Amounts By Development Period (Incremental)",
              "Development Period", "Claim Amounts", "Origin Period", withZeroPoint=True)

srf.table_plot(srf.arrayRound_and_format(Triangle, 0),
               ["OP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               ["DP " + str(i) for i in range(1, len(Inc_Triangle) + 1)],
               "Cumulative Claims Data")
srf.line_plot(Triangle, "Claim Amounts By Development Period (Cumulative)",
              "Development Period", "Claim Amounts", "Origin Period", withZeroPoint=True)

rowlabels_CL = [*["OP " + str(i) for i in range(1, len(LRs) + 1)], "Chain Ladder Factors"]
CLPlot = srf.arrayRound_and_format(np.vstack((srf.triangleUpper(LRs), CL_facs)), 3)
srf.table_plot(CLPlot, rowlabels_CL,
               ["DP " + str(i) for i in range(1, len(LRs) + 1)], "Link ratios table")
srf.line_plot(LRs, "Link Ratios By Development Period",
              "Development Period", "Link Ratios", "Origin Period")

LatestPlot = srf.arrayRound_and_format(np.array([*Latest, TotalLatest]).reshape(-1, 1), 0)
UltimatesPlot = srf.arrayRound_and_format(np.array([*CL_Ults, TotalUltimates]).reshape(-1, 1), 0)
ReservesPlot = srf.arrayRound_and_format(np.array([*CL_Res, TotalReserves]).reshape(-1, 1), 0)
Table_2_tmp = np.hstack((LatestPlot, ReservesPlot, UltimatesPlot))

srf.table_plot(Table_2_tmp,
               [*["OP " + str(i) for i in range(1, len(Table_2_tmp))], "Total"],
               ["Latest", "Reserves", "Ultimates"], "Chain Ladder Results")

# =============================================================================
# Analytic Results - Table 2
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

LatestPlot = srf.arrayRound_and_format(np.array([*Latest, np.sum(Latest)]).reshape(-1, 1), 0)
ReservesPlot = srf.arrayRound_and_format(CL_Reserves, 0)
UltimatesPlot = srf.arrayRound_and_format(np.array([*Ultimates, np.sum(Ultimates)]).reshape(-1, 1), 0)
SD_Plot = srf.arrayRound_and_format(SDs, 0)
CoV_Plot = srf.arrayRound_and_format(CoVs * 100, 1)
Table_2 = np.hstack((LatestPlot, ReservesPlot, UltimatesPlot, SD_Plot, CoV_Plot))

srf.table_plot(Table_2,
               [*["OP " + str(i) for i in range(1, len(Table_2))], "Total"],
               ["Latest", "Reserves", "Ultimates", "Reserves SD", "Reserves CoV%"],
               "Table 2: Analytic Chain Ladder Results\n(" + method + ")")

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
# Bootstrap + CDR (Claims Development Result) - Tables 4, 6, 7
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

Forecast_Cumulatives = Bstrap_Result["Complete_Cumulatives"]
CDR_Result = srf.CDR_Full_Picture(Triangle, Forecast_Cumulatives, VAR_p=0.995, Mask=Mask)

SD_CDR = CDR_Result["SD_CDR"][0, :].reshape(-1, 1)
CDR_Ratio = 100 * srf.safe_divide(SD_CDR[:, 0], Bstrap_AvgRes).reshape(-1, 1)
SD_TotalCDR = CDR_Result["SD_TotalCDR"][0].reshape(-1, 1)
TotalCDR_Ratio = (100 * SD_TotalCDR[:, 0] / Total_Bstrap_AvgReserve).reshape(-1, 1)

SD_CDR_Plot = srf.arrayRound_and_format(np.vstack((SD_CDR, SD_TotalCDR)), 0)
CDR_Ratio_Plot = srf.arrayRound_and_format(np.vstack((CDR_Ratio, TotalCDR_Ratio)), 1)

title = ("Table 4: Bootstrap and One-yr CDR summary, " + str(Bstrap_Result["iterations"])
         + " iterations\n(" + method + ")")
table = np.hstack((Bstrap_Latest_Plot, Bstrap_AvgReserve_Plot, Bstrap_AvgUltimate_Plot,
                   Bstrap_SDReserve_Plot, Bstrap_CoVReserve_Plot, SD_CDR_Plot, CDR_Ratio_Plot))
srf.table_plot(table,
               [*["OP " + str(i) for i in range(1, len(table))], "Total"],
               ["Latest", "Avg Reserves", "Avg Ultimates", "Bstrap SD", "Bstrap CoV (%)",
                "CDR SD", "CDR SD Ratio (%)"], title)

# Table 6: Incremental RMSEPs of CDRs
Table_6 = np.vstack((np.transpose(CDR_Result["SD_CDR"]), CDR_Result["SD_TotalCDR"]))
Table_6_SS = np.sqrt(np.nansum(Table_6 * Table_6, 1))
Table_6_BstrapSD = np.hstack((Bstrap_Result["SD_Reserve"], Bstrap_Result["SD_TotalReserve"]))
Table_6 = srf.arrayRound_and_format(
    np.hstack((Table_6, Table_6_SS.reshape(-1, 1), Table_6_BstrapSD.reshape(-1, 1))), 0)
srf.table_plot(Table_6,
               [*["OP " + str(i + 1) for i in range(len(Table_6) - 1)], "Total"],
               [*["CDR(" + str(i + 1) + ")" for i in range(len(Table_6) - 2)], "Sqrt SS", "BStrap SD"],
               "Table 6: Incremental simulated CDR RMSEPs, " + str(iterations) + " iterations\n(" + method + ")")

# Table 6 cumulative
Table_6_cumul = np.vstack((np.transpose(CDR_Result["SD_cumsum_CDR"]), CDR_Result["SD_cumsum_TotalCDR"]))
Table_6_SS_cumul = srf.npNaN(len(Table_6))
Table_6_cumul = srf.arrayRound_and_format(
    np.hstack((Table_6_cumul, Table_6_SS_cumul.reshape(-1, 1), Table_6_BstrapSD.reshape(-1, 1))), 0)
srf.table_plot(Table_6_cumul,
               [*["OP " + str(i + 1) for i in range(len(Table_6_cumul) - 1)], "Total"],
               [*["CDR(" + str(i + 1) + ")" for i in range(len(Table_6_cumul) - 2)], "Sqrt SS", "BStrap SD"],
               "Table 6: Cumulative simulated CDR RMSEPs, " + str(iterations) + " iterations\n(" + method + ")")

# Table 7: VaR(CDR) @ 99.5%
Table_7 = np.vstack((np.transpose(CDR_Result["CDR_VAR"]), CDR_Result["TotalCDR_VAR"]))
Table_7 = srf.arrayRound_and_format(Table_7, 0)
srf.table_plot(Table_7,
               [*["OP " + str(i + 1) for i in range(len(Table_7) - 1)], "Total"],
               [*["CDR(" + str(i + 1) + ")" for i in range(len(Table_7) - 2)]],
               "Table 7: VaR(CDR) @ 99.5%, " + str(iterations) + " iterations\n(" + method + ")")

srf.ShowSummaryStats(Bstrap_Result, Output="Reserves")
srf.ShowSummaryStats(Bstrap_Result, Output="Ultimates")

# =============================================================================
# Fan Charts
# =============================================================================

for op in [1, 3, 5, 7, 9]:
    srf.fan_plot(op, Forecast_Cumulatives)

# =============================================================================
# Discounted Reserves - Table 5
# =============================================================================

disc_rate = 0.03

Disc_Res = srf.Bstrap_Disc_Reserves(Forecast_Cumulatives, disc_rate, 0.5)

Avg_Disc_Res = Disc_Res["Avg_Disc_Res"]
SD_Disc_Res = Disc_Res["SD_Disc_Res"]
CoV_Disc_Res = Disc_Res["CoV_Disc_Res"]
Avg_Disc_TotalRes = Disc_Res["Avg_Disc_TotalRes"]
SD_Disc_TotalRes = Disc_Res["SD_Disc_TotalRes"]
Cov_Disc_TotalRes = Disc_Res["Cov_Disc_TotalRes"]
Disc_TotalReserve = Disc_Res["TotalReserve"]

Disc_AvgReserve_Plot = srf.arrayRound_and_format(
    np.array([*Avg_Disc_Res, Avg_Disc_TotalRes]).reshape(-1, 1), 0)
Disc_SDReserve_Plot = srf.arrayRound_and_format(
    np.array([*SD_Disc_Res, SD_Disc_TotalRes]).reshape(-1, 1), 0)
Disc_CoVReserve_Plot = srf.arrayRound_and_format(
    np.array([100 * i for i in [*CoV_Disc_Res, Cov_Disc_TotalRes]]).reshape(-1, 1), 1)

title = "Table 5: Bootstrap reserves, discounted at " + str(int(disc_rate * 100)) + "%\n(" + method + ")"
table = np.hstack((Disc_AvgReserve_Plot, Disc_SDReserve_Plot, Disc_CoVReserve_Plot))
srf.table_plot(table,
               [*["OP " + str(i) for i in range(1, len(table))], "Total"],
               ["Avg Reserves", "Bstrap SD", "Bstrap CoV (%)"], title)

srf.ShowSummaryStats(Disc_Res, Output="Reserves")

# =============================================================================
# Solvency II Cost-of-Capital Risk Margins - Tables 8 & 9
# =============================================================================

RM_Initial_Capital = CDR_Result["TotalCDR_VAR"][0]
CoC_rate = 0.06

Disc_Fut_Res = srf.Disc_Future_Reserves(srf.Incrementals(ChainLadderResult["CompleteForecast"]), disc_rate, 0.5)
Disc_BE_Profile = srf.Capital_Profile(Disc_Fut_Res)
Disc_RM_BE = srf.CoC_RM(RM_Initial_Capital, Disc_BE_Profile, CoC_rate, disc_rate, 1)
CoC_Risk_Margin = Disc_RM_BE["RM"]

Fut_Res_Plot = srf.arrayRound_and_format(np.array([*Disc_Fut_Res, np.nan]).reshape(-1, 1), 0)
Capital_plot = srf.arrayRound_and_format(np.array([*Disc_RM_BE["Capital"], np.nan]).reshape(-1, 1), 0)
Capital_Profile_Plot = srf.arrayRound_and_format(np.array([*Disc_BE_Profile, np.nan]).reshape(-1, 1), 3)
CoC_Plot = srf.arrayRound_and_format(np.array([*Disc_RM_BE["CoC"], np.nan]).reshape(-1, 1), 0)
Disc_CoC_Plot = srf.arrayRound_and_format(np.array([*Disc_RM_BE["Disc_CoC"], CoC_Risk_Margin]).reshape(-1, 1), 0)

table = np.hstack((Fut_Res_Plot, Capital_plot, Capital_Profile_Plot, CoC_Plot, Disc_CoC_Plot))
srf.table_plot(table,
               [*["Future Yr " + str(i) for i in range(len(table) - 1)], "CoC Risk Margin"],
               ["Disc Fut Res", "Capital", "Capital Profile", "Cost of Capital", "Disc CoC"],
               "Table 8: Cost-of-Capital Risk Margin\n(" + method + ")")

CDR_SD_Profile = srf.Capital_Profile(CDR_Result["SD_TotalCDR"])
CDR_VAR_Profile = srf.Capital_Profile(CDR_Result["TotalCDR_VAR"])
Disc_RM_SD = srf.CoC_RM(RM_Initial_Capital, CDR_SD_Profile, CoC_rate, disc_rate, 1)
Disc_RM_VAR = srf.CoC_RM(RM_Initial_Capital, CDR_VAR_Profile, CoC_rate, disc_rate, 1)

BE_Plot = srf.arrayRound_and_format(np.array([*Disc_RM_BE["Capital"], Disc_RM_BE["RM"]]).reshape(-1, 1), 0)
SD_plot = srf.arrayRound_and_format(np.array([*Disc_RM_SD["Capital"], Disc_RM_SD["RM"]]).reshape(-1, 1), 0)
VAR_Plot = srf.arrayRound_and_format(np.array([*Disc_RM_VAR["Capital"], Disc_RM_VAR["RM"]]).reshape(-1, 1), 0)

table = np.hstack((BE_Plot, SD_plot, VAR_Plot))
srf.table_plot(table,
               [*["Future Yr " + str(i) for i in range(len(table) - 1)], "CoC Risk Margin"],
               ["Best Estimate", "CDR SD", "CDR VaR@99.5%"],
               "Table 9: Capital under different bases\n(" + method + ")")

# =============================================================================
# Risk Measures for IFRS 17 Risk Adjustments - Tables 10 & 11
# =============================================================================

VAR_level = 0.75
TVAR_level = 0.4
PHT_param = 1.85

DiscTotalReserve_VAR = -1 * srf.VAR(-Disc_TotalReserve, (1 - VAR_level))
DiscTotalReserve_TVAR = srf.TVAR(Disc_TotalReserve, TVAR_level)
DiscTotalReserve_PHT = srf.PHT(Disc_TotalReserve, PHT_param)
DiscTotalReserve_VAR_RM = DiscTotalReserve_VAR - Avg_Disc_TotalRes
DiscTotalReserve_TVAR_RM = DiscTotalReserve_TVAR - Avg_Disc_TotalRes
DiscTotalReserve_PHT_RM = DiscTotalReserve_PHT - Avg_Disc_TotalRes
DiscTotalReserve_VAR_RM_ratio = DiscTotalReserve_VAR_RM / Avg_Disc_TotalRes
DiscTotalReserve_TVAR_RM_ratio = DiscTotalReserve_TVAR_RM / Avg_Disc_TotalRes
DiscTotalReserve_PHT_RM_ratio = DiscTotalReserve_PHT_RM / Avg_Disc_TotalRes

VAR_plot = srf.arrayRound_and_format(
    np.array([VAR_level * 100, DiscTotalReserve_VAR_RM, Avg_Disc_TotalRes,
              DiscTotalReserve_VAR_RM + Avg_Disc_TotalRes,
              DiscTotalReserve_VAR_RM_ratio * 100]).reshape(-1, 1), 1)
TVAR_plot = srf.arrayRound_and_format(
    np.array([TVAR_level * 100, DiscTotalReserve_TVAR_RM, Avg_Disc_TotalRes,
              DiscTotalReserve_TVAR_RM + Avg_Disc_TotalRes,
              DiscTotalReserve_TVAR_RM_ratio * 100]).reshape(-1, 1), 1)
PHT_plot = srf.arrayRound_and_format(
    np.array([PHT_param * 100, DiscTotalReserve_PHT_RM, Avg_Disc_TotalRes,
              DiscTotalReserve_PHT_RM + Avg_Disc_TotalRes,
              DiscTotalReserve_PHT_RM_ratio * 100]).reshape(-1, 1), 1)

table_10 = np.hstack((VAR_plot, TVAR_plot, PHT_plot))
srf.table_plot(table_10,
               ["Risk Tolerance Level (%)", "Risk Adjustment", "Best Estimate (Disc)", "Total",
                "Risk Adjustment (%)"],
               ["Value-at-Risk", "Tail Value-at-Risk", "Proportional Hazards Transform"],
               "Table 10: Risk adjustments using VaR, TVaR and PHT\n(" + method + ")")

# Find risk tolerance levels matching Cost-of-Capital Risk Margin
RM = Disc_RM_BE["RM"]
RM_ratio = RM / Disc_Fut_Res[0]
AvgDiscTotalRes = Disc_Res["Avg_Disc_TotalRes"]
target = RM

VAR_level = brentq(lambda p: -srf.VAR(-Disc_Res["TotalReserve"], (1 - p)) - AvgDiscTotalRes - target, 0.01, 1)
TVAR_level = brentq(lambda p: srf.TVAR(Disc_Res["TotalReserve"], p) - AvgDiscTotalRes - target, 0.01, 0.99)
PHT_param = brentq(lambda p: srf.PHT(Disc_Res["TotalReserve"], p) - AvgDiscTotalRes - target, 1, 1000)

DiscTotalReserve_VAR = -1 * srf.VAR(-Disc_TotalReserve, (1 - VAR_level))
DiscTotalReserve_TVAR = srf.TVAR(Disc_TotalReserve, TVAR_level)
DiscTotalReserve_PHT = srf.PHT(Disc_TotalReserve, PHT_param)
DiscTotalReserve_VAR_RM = DiscTotalReserve_VAR - Avg_Disc_TotalRes
DiscTotalReserve_TVAR_RM = DiscTotalReserve_TVAR - Avg_Disc_TotalRes
DiscTotalReserve_PHT_RM = DiscTotalReserve_PHT - Avg_Disc_TotalRes
DiscTotalReserve_VAR_RM_ratio = DiscTotalReserve_VAR_RM / Avg_Disc_TotalRes
DiscTotalReserve_TVAR_RM_ratio = DiscTotalReserve_TVAR_RM / Avg_Disc_TotalRes
DiscTotalReserve_PHT_RM_ratio = DiscTotalReserve_PHT_RM / Avg_Disc_TotalRes

VAR_plot = srf.arrayRound_and_format(
    np.array([VAR_level * 100, DiscTotalReserve_VAR_RM, Avg_Disc_TotalRes,
              DiscTotalReserve_VAR_RM + Avg_Disc_TotalRes,
              DiscTotalReserve_VAR_RM_ratio * 100]).reshape(-1, 1), 1)
TVAR_plot = srf.arrayRound_and_format(
    np.array([TVAR_level * 100, DiscTotalReserve_TVAR_RM, Avg_Disc_TotalRes,
              DiscTotalReserve_TVAR_RM + Avg_Disc_TotalRes,
              DiscTotalReserve_TVAR_RM_ratio * 100]).reshape(-1, 1), 1)
PHT_plot = srf.arrayRound_and_format(
    np.array([PHT_param * 100, DiscTotalReserve_PHT_RM, Avg_Disc_TotalRes,
              DiscTotalReserve_PHT_RM + Avg_Disc_TotalRes,
              DiscTotalReserve_PHT_RM_ratio * 100]).reshape(-1, 1), 1)
CoC_plot = srf.arrayRound_and_format(
    np.array([np.nan, RM, Disc_Fut_Res[0], RM + Disc_Fut_Res[0], RM_ratio * 100]).reshape(-1, 1), 1)

table_11 = np.hstack((CoC_plot, VAR_plot, TVAR_plot, PHT_plot))
srf.table_plot(table_11,
               ["Risk Tolerance Level (%)", "Risk Adjustment", "Best Estimate (Disc)", "Total",
                "Risk Adjustment (%)"],
               ["Cost of Capital", "Value-at-Risk", "Tail Value-at-Risk", "Proportional Hazards Transform"],
               "Table 11: Risk adjustments using VaR, TVaR and PHT\n(" + method + ")")

# =============================================================================
# Table 12: CoC Risk Margin under different bases
# =============================================================================

BStrap_Disc_Fut_Res = srf.Bstrap_Disc_Future_Reserves(Forecast_Cumulatives, disc_rate, 0.5)
BStrap_Undisc_Fut_Res = srf.Bstrap_Disc_Future_Reserves(Forecast_Cumulatives, 0, 0)

VAR_level_res = brentq(lambda p: -srf.VAR(-Disc_Res["TotalReserve"], (1 - p)) -
                        Disc_Res["Avg_Disc_TotalRes"] - CDR_Result["TotalCDR_VAR"][0], 0.01, 1)

Disc_Fut_Res_VAR_root = -np.array([
    srf.VAR(-BStrap_Disc_Fut_Res["FutureReserves"][:, j], 1 - VAR_level_res)
    for j in range(BStrap_Disc_Fut_Res["FutureReserves"].shape[1])
]) - BStrap_Disc_Fut_Res["Avg_Disc_Fut_Res"]

Disc_Fut_Res_VAR_995 = -np.array([
    srf.VAR(-BStrap_Disc_Fut_Res["FutureReserves"][:, j], 1 - 0.995)
    for j in range(BStrap_Disc_Fut_Res["FutureReserves"].shape[1])
]) - BStrap_Disc_Fut_Res["Avg_Disc_Fut_Res"]

RM_Initial_Capital_T12 = Disc_Fut_Res_VAR_root[0]
Disc_AvgRes_RM = srf.CoC_RM(RM_Initial_Capital_T12,
                              srf.Capital_Profile(BStrap_Disc_Fut_Res["Avg_Disc_Fut_Res"]), CoC_rate, disc_rate, 1)
Disc_SDRes_RM = srf.CoC_RM(RM_Initial_Capital_T12,
                             srf.Capital_Profile(BStrap_Disc_Fut_Res["SD_Disc_Fut_Res"]), CoC_rate, disc_rate, 1)
UnDisc_SDRes_RM = srf.CoC_RM(RM_Initial_Capital_T12,
                               srf.Capital_Profile(BStrap_Undisc_Fut_Res["SD_Disc_Fut_Res"]), CoC_rate, disc_rate, 1)
Disc_VARRes_RM_root = srf.CoC_RM(RM_Initial_Capital_T12,
                                   srf.Capital_Profile(Disc_Fut_Res_VAR_root), CoC_rate, disc_rate, 1)
Disc_VARRes_RM_995 = srf.CoC_RM(Disc_Fut_Res_VAR_995[0],
                                  srf.Capital_Profile(Disc_Fut_Res_VAR_995), CoC_rate, disc_rate, 1)

BE_Plot = srf.arrayRound_and_format(
    np.array([*BStrap_Disc_Fut_Res["Avg_Disc_Fut_Res"], Disc_AvgRes_RM["RM"]]).reshape(-1, 1), 0)
SD_plot_Disc = srf.arrayRound_and_format(
    np.array([*BStrap_Disc_Fut_Res["SD_Disc_Fut_Res"], Disc_SDRes_RM["RM"]]).reshape(-1, 1), 0)
SD_plot_Undisc = srf.arrayRound_and_format(
    np.array([*BStrap_Undisc_Fut_Res["SD_Disc_Fut_Res"], UnDisc_SDRes_RM["RM"]]).reshape(-1, 1), 0)
VAR_Plot_root = srf.arrayRound_and_format(
    np.array([*Disc_Fut_Res_VAR_root, Disc_VARRes_RM_root["RM"]]).reshape(-1, 1), 0)
VAR_Plot_995 = srf.arrayRound_and_format(
    np.array([*Disc_Fut_Res_VAR_995, Disc_VARRes_RM_995["RM"]]).reshape(-1, 1), 0)

table = np.hstack((BE_Plot, SD_plot_Disc, SD_plot_Undisc, VAR_Plot_root, VAR_Plot_995))
srf.table_plot(table,
               [*["Future Yr " + str(i) for i in range(len(table) - 1)], "CoC Risk Margin"],
               ["Avg(Disc Res)", "SD(Disc Res)", "SD(Undisc Res)",
                "VaR(Disc Res)@" + str(round(VAR_level * 100, 1)) + "%", "VaR(Disc Res)@99.5%"],
               "Table 12: CoC Risk Margin under different bases\n(" + method + ")")

# =============================================================================
# Table 13: CoC Risk Margin using Reverse Sum of CDRs
# =============================================================================

CDRRevSum = srf.CDR_Rev_Sum(CDR_Result["TotalCDR"])
CDRRevSum_SD = CDRRevSum["SD_RevSum_CDR"]
CDRRevSum_SD_MW = srf.npNaN(CDRRevSum_SD.shape)

VAR_level_cdr = brentq(lambda p: -srf.VAR(CDRRevSum["RevSum_CDR"][0,], (1 - p)) +
                         CDRRevSum["Avg_RevSum_CDR"][0] - CDR_Result["TotalCDR_VAR"][0], 0.01, 1)

CDRRevSum_VAR_root = -np.array([
    srf.VAR(CDRRevSum["RevSum_CDR"][j, :], 1 - VAR_level_cdr)
    for j in range(CDRRevSum["RevSum_CDR"].shape[0])
]) + CDRRevSum["Avg_RevSum_CDR"]

CDRRevSum_VAR_995 = -np.array([
    srf.VAR(CDRRevSum["RevSum_CDR"][j, :], 1 - 0.995)
    for j in range(CDRRevSum["RevSum_CDR"].shape[0])
]) + CDRRevSum["Avg_RevSum_CDR"]

RM_Initial_Capital_T13 = CDRRevSum_VAR_root[0]
CDRRevSum_SD_RM = srf.CoC_RM(RM_Initial_Capital_T13, srf.Capital_Profile(CDRRevSum_SD), CoC_rate, disc_rate, 1)
CDRRevSum_VAR_RM_root = srf.CoC_RM(RM_Initial_Capital_T13,
                                     srf.Capital_Profile(CDRRevSum_VAR_root), CoC_rate, disc_rate, 1)
CDRRevSum_VAR_RM_995 = srf.CoC_RM(CDRRevSum_VAR_995[0],
                                    srf.Capital_Profile(CDRRevSum_VAR_995), CoC_rate, disc_rate, 1)

MW_SD_Plot = np.array([*CDRRevSum_SD_MW, np.nan]).reshape(-1, 1)
SD_plot = srf.arrayRound_and_format(
    np.array([*CDRRevSum_SD, CDRRevSum_SD_RM["RM"]]).reshape(-1, 1), 0)
VAR_Plot_root = srf.arrayRound_and_format(
    np.array([*CDRRevSum_VAR_root, CDRRevSum_VAR_RM_root["RM"]]).reshape(-1, 1), 0)
VAR_Plot_995 = srf.arrayRound_and_format(
    np.array([*CDRRevSum_VAR_995, CDRRevSum_VAR_RM_995["RM"]]).reshape(-1, 1), 0)

table = np.hstack((MW_SD_Plot, SD_plot, VAR_Plot_root, VAR_Plot_995))
srf.table_plot(table,
               [*["Future Yr " + str(i) for i in range(len(table) - 1)], "CoC Risk Margin"],
               ["MW RMSEP", "SD(Simulated)",
                "VaR @ " + str(round(VAR_level_cdr * 100, 1)) + "%", "VaR @ 99.5%"],
               "Table 13: CoC Risk Margin using Reverse Sum of CDRs\n(" + method + ")")

# =============================================================================
# Figure 1: Capital Profiles
# =============================================================================

Disc_Res_VAR_Profile = srf.Capital_Profile(Disc_Fut_Res_VAR_root)

plt.figure(figsize=(10, 6))
plt.plot(range(len(Disc_BE_Profile)), 100 * Disc_BE_Profile,
         marker='o', linewidth=2, label='Best estimate basis')
plt.plot(range(len(CDR_SD_Profile)), 100 * CDR_SD_Profile,
         marker='s', linewidth=2, label='Standard deviation')
plt.plot(range(len(CDR_VAR_Profile)), 100 * CDR_VAR_Profile,
         marker='^', linewidth=2, label='VaR@99.5%(CDR)')
plt.plot(range(len(Disc_Res_VAR_Profile)), 100 * Disc_Res_VAR_Profile,
         marker='d', linewidth=2,
         label='VaR@' + str(round(VAR_level_res * 100, 1)) + '%(Reserves)')
plt.title('Fig.1: Capital Profiles by Year', fontsize=14, fontweight='bold')
plt.xlabel('Future Year', fontsize=12)
plt.ylabel('Percent of Opening Capital', fontsize=12)
plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), ncol=2, frameon=True)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
