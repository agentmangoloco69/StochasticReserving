## Stochastic Reserving: Modus Operandi

# This code is provided by Peter England on behalf of EMC Actuarial and Analytics Ltd as an educational resource.

# R code to show how to quantify the variability in chain ladder reserve estimates using analytic (closed form) and 
# bootstrap methods. Analytic methods give a standard deviation of the forecasts (root mean-squared error of prediction) only.
# Bootstrap methods give a predictive distribution of the forecasts, where the standard deviation of the simulated forecasts 
# should closely match the analytic result if the method has been applied appropriately.

# The approach is illustrated with an example dataset, which has an interesting feature that affects the variability of the forecasts.

# This example considers the lifetime ("ultimo") view of reserve risk only. For the one-year view of risk (and beyond), 
# see the examples in "EVW_2019.r" and the CDR_Full_Picture function.

 
# A good approach to quantifying reserve volatility starts with understanding what is driving variability, and understanding each model's 
# characteristics and limitations, before selecting a final result. The following steps are a useful guide:
# 
# Step 1: Look at the data and try to identify unusual behaviour that might drive volatility.
# 
# Step 2: Fit a baseline chain ladder model, and identify which link ratios are associated with any
#   unusual behaviour identified at step 1.
# 
# Step 3: Apply Mack's method analytically to quickly obtain a baseline measure of the standard deviation of the forecasts.
# 
# Step 4: Calculate unscaled and scaled residuals for Mack's model, and plot against origin, development and calendar period.
#   Inspect the variance parameters (Mack's sigma) by development period to identify magnitude and anything unusual.
#   The variance parameters (unsurprisingly) control the volatility in the forecasts.
# 
# Step 5: Apply a bootstrap model using Mack's assumptions with all link ratios included.
#   Create and inspect summary statistics. In particular, check that the mean is close to the chain ladder estimates,
#   check that the standard deviation of the forecasts is close to the analytic result from Mack's method, and check 
#   the coefficient of variation.
#   Also check the minimum and maximum simulated values by origin period for reasonablesness. When bootstrapping Mack's model, 
#   the minimum "reserves" can be negative, such that the ultimate is less than the latest position. This might be reasonable with
#   incurred data, but would not be expected with paid data. If this occurs with paid data, it might be better to bootstrap 
#   the over-dispersed Poisson model instead (see later).
#   Plot reserve development graphs (ie fan charts) and look for anything unusual, for example, the chart suddenly fanning
#   out at a particular development period would indicate a high sigma parameter at that point. Similarly, very high volatility 
#   in the most recent origin period would indicate a high sigma parameter in the first development period, which should be
#   investigated further.
# 
# Step 6: Perform a sensitivity analysis where each link ratio is excluded in turn, and Mack's method is re-applied analytically.
#   This is a quick way to identify which link ratios are influencing volatility in the forecasts.
#   Rank the changes in reserves, standard deviation, and coefficients of variation from the sensitivity analysis to identify 
#   which link ratios are driving volatility in the forecasts.
#   For the most influential ratios, go back to the underlying data, link ratios and Mack's sigma parameters to understand 
#   what is driving the volatility.
# 
# Step 7: Exclude the top n influential ratios (eg top 3) and re-apply Mack's method analytically to see how much the 
#   standard deviation of the forecasts and coefficient of variation reduces relative to the baseline model (with all ratios included).
#   This is a quick way to see how much of the volatility in the forecasts is explained by the most influential link ratios.
#   If the reduction in standard deviation and coefficient of variation is insignificant, then the development pattern of the 
#   claims triangle is reasonably stable.
# 
# Step 8: Recalculate the residuals and variance parameters (Mack's sigma) after excluding the most influential ratios.
#   In particular, check the impact on the variance parameters.
# 
# Step 9: Re-apply the bootstrap method using Mack's assumptions after excluding the most influential ratios, 
#   and check the summary statistics and reserve development (fan) graphs again.
# 
# Step 10: To further understand the impact of the variance parameters, include all link ratios again but manually adjust 
#   the variance parameters (Mack's sigma) where appropriate. Re-apply the bootstrap model with user-defined "SqrtScale" parameters 
#   (ie Mack's sigma). Investigate the impact on the summary statistics and reserve development (fan) graphs.
# 
# Step 11: Now investigate similar results for the over-dispersed Poisson model (ODP) with non-constant scale parameters.
#   Initially include all link ratios. Calculate unscaled and scaled residuals for the ODP model. Inspect the residuals and
#   check the variance parameters (ODP "sqrtScale") by development period.
#   Apply a bootstrap model using ODP assumptions (with non-constant scale parameters) with all link ratios included, and 
#   check the summary statistics and reserve development (fan) graphs. In particular, check the minimum simulated values by
#   origin period. If all of the incremental observed values in the claims triangle are positive and the parametric bootstrap
#   options have been selected, the minimum "reserves" should be positive.
# 
# Step 12: Investigate the impact of influential link ratios. The influential ratios identified at Step 6 using Mack's assumptions
#   will also be influential under the ODP model. Repeat the analysis at Step 11 for the ODP model, but excluding the most 
#   influential link ratios. In particular, observe the effect on the residuals, sqrtScale parameters, summary statistics and 
#   reserve development (fan) graphs.
# 
# Step 13: Investigate the impact of manually adjusting the variance parameters (sqrtScale) for the ODP model.
# 
# Step 14: Finally, compare the results and characteristics of Mack's model with the ODP model before selecting which
#   model to use, whether to exclude influential ratios, and whether it is appropriate to over-ride the variance parameters 
#   (ie Mack's sigma or sqrtScale for the ODP model) with user-defined values.
# 
# Step 15: After following the steps above, the mean of the distributions by origin period from the variability analysis are
#   unlikely to match the values used by the Reserving Team for the reported reserves (which may include additional judgement
#   and prudence). It is common to scale the final result from the variability analysis to an alternative mean while preserving
#   the absolute standard deviation or coefficient of variation.


## Steps 1 & 2: Look at the data, and fit a baseline chain ladder model. Identify which link ratios are 
#   associated with any unusual behaviour:

## Set up libraries
library(dplyr)
library(ggplot2)
library(tidyverse) # for graphs #masks rstan::extract()
library(ggfan) # For fan charts

# Read in stochastic reserving functions
source("StochResFunctions.R")

# Read in data from csv file
Liability <- read.table("Liability.csv", sep=",")

# Select triangle to work on
Triangle <- Liability

# Look at data first
# Incremental claims data:
Inc_Triangle <- Incrementals(Triangle)
Inc_Triangle
plotTriangleGraph(Inc_Triangle)

# Cumulative claims data
#Triangle <- Cumulatives(Inc_Triangle)
Triangle
plotTriangleGraph(Triangle)

# Calculate link ratio triangle and volume-weighted chain ladder factors
LRs <- LR_Tri(Triangle)
CL_facs <- CL_factors(LRs$LRs, LRs$Weight)

LRs$LRs
CL_facs
plotLRsGraph(LRs$LRs)

# Calculate volume-weighted chain ladder results
CL_Result <- Tri_forecast(Triangle, CL_facs)
Latest_by_yr <- CL_Result$Latest
TotalLatest <- sum(Latest_by_yr)
Latest <- c(Latest_by_yr, TotalLatest)
Reserves_by_yr <- CL_Result$Reserves
TotalReserve <- CL_Result$TotalReserve
Reserves <- c(Reserves_by_yr, TotalReserve)
Ultimates_by_yr <- CL_Result$Ultimates
TotalUltimate <- sum(Ultimates_by_yr)
Ultimates <- c(Ultimates_by_yr, TotalUltimate)

Table_0 <- cbind(round(Latest,0), round(Reserves,0), round(Ultimates,0))
dimnames(Table_0) <- list(c(1:length(Triangle), "Total"), c("Latest", "Reserves", "Ultimates"))
names(dimnames(Table_0)) <- c("Year","Chain Ladder Results")
Table_0

## Commentary:
#  
# Looking at graphs of the incremental and cumulative amounts, we can see a large incremental value at development period 7 for
#   origin period 3. It is likely that this will be influential on volatility estimates. It is often easier to see this kind of
#   feature when inspecting a graph of incremental values.
#
# Consistent with the observation above, we can see a large link ratio at development period 6 for origin period 3 (representing
#   the link ratio between the 6th and 7th development periods). We can also see that this link ratio is impacting the 
#   volume-weighted average development factor at development period 6, which reverses the trend of decreasing development factors
#   up until that point.
#
# We observe that including all ratios gives a reserve estimate of 331,038.


## Step 3: Apply Mack's method analytically to quickly obtain a baseline measure of the standard deviation of the forecasts:

# Mack's model
# Analytic Result using weighted Gaussian GLM and
# recursive formulae from England & Verrall (2002) for RMSEPs
Mack_Analytic_Result <- Mack_ChainLadder(Triangle)
Latest_by_yr <- Mack_Analytic_Result$Latest
TotalLatest <- sum(Latest_by_yr)
Latest <- c(Latest_by_yr, TotalLatest)
Reserves_by_yr <- Mack_Analytic_Result$Reserves
TotalReserve <- Mack_Analytic_Result$TotalReserves
Reserves <- c(Reserves_by_yr, TotalReserve)
Ultimates_by_yr <- Mack_Analytic_Result$Ultimates
TotalUltimate <- sum(Ultimates_by_yr)
Ultimates <- c(Ultimates_by_yr, TotalUltimate)
Mack_Res_SD_by_yr <- Mack_Analytic_Result$Reserves_SD
Mack_Res_TotalSD <- Mack_Analytic_Result$TotalReserve_SD
Mack_Res_SD <- c(Mack_Res_SD_by_yr, Mack_Res_TotalSD)
Mack_Res_CoV <- safe_divide(Mack_Res_SD, abs(Reserves))

Table_1 <- cbind(round(Latest,0), round(Reserves,0), round(Ultimates,0), round(Mack_Res_SD,0), round(100*Mack_Res_CoV,1))
dimnames(Table_1) <- list(c(1:length(Triangle), "Total"), c("Latest", "Reserves", "Ultimates", "Reserve SD", "Reserve CoV %"))
names(dimnames(Table_1)) <- c("Year","Analytic")
Table_1

## Commentary:
#  
# Applying Mack's model analytically, we observe a standard deviation of 69,077 giving a coefficient of variation of 21%, which
# is quite high given the cumulative claims show a generally stable pattern.


## Step 4: Calculate unscaled and scaled residuals for Mack's model, and review the variance parameters (Mack's sigma):

# Calculate residuals for Mack's model including all ratios:
method <- "Mack"
Resids <- Calc_Residuals(Triangle, method=method)

# show adjusted unscaled residuals
adj_unscaled_resids <- round(Resids$adjunscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)-1), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)-1), sep=" ")
dimnames(adj_unscaled_resids) <- list(op_labels_2, dp_labels_2)
adj_unscaled_resids

# show zero-average scaled residuals
zeroavg_scaled_resids <- round(Resids$zeroavgscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)-1), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)-1), sep=" ")
dimnames(zeroavg_scaled_resids) <- list(op_labels_2, dp_labels_2)
zeroavg_scaled_resids

# Show variance parameters (Mack's sigma or ODP sqrtScale)
sigma <- round(Resids$sqrtScale, 1)
names(sigma) <- paste0("DP", 1:(length(Triangle)-1), sep=" ")
cat("Mack's sigma parameters:\n")
sigma
sigma_original <- Resids$sqrtScale # save for later

# plot residuals
plotResiduals(Resids, resid_type="adjunscaledresids", bstrap_method=method)
plotResiduals(Resids, resid_type="zeroavgscaledresids", bstrap_method=method)

## Commentary:
#  
# The unscaled residuals are generally difficult to interpret, since we have no reference point for their range. However, 
#   we can see that the range is generally reducing by development period, except at development period 6 (Mack's model 
#   is really a model of the link ratios, so for a 10 by 10 triangle there is a 9 by 9 triangle of residuals, and the 
#   6th development factor represents the development between the 6th and 7th development periods).
#
# The (adjusted) unscaled residuals are used to estimate Mack's sigma parameters (the variance parameters). The sigma parameters
#   are estimated as the square root of the average squared adjusted unscaled residuals at each development period. A large 
#   unscaled residual will cause Mack's sigma to be large.
#
# We can see that the unscaled residual at development period 6 for origin period 3 is large, causing Mack's sigma to spike at
#   that point when the other values are decreasing.
#                                                                                                                                                                                                                                 
# Dividing the unscaled residuals by Mack's sigma scales the residuals in a way that they can now be interpreted like Gaussian
#   theory residuals. That is, we expect them to be approximately i.i.d. with mean 0 and variance 1. We expect a pattern free
#   set of residuals when plotting them by origin, development and calendar period, and we expect 95% to be (approximately) in
#   the range +/- 2. We still expect 5% to be outside that range.
#
# The main concern with this example is the high value of Mack's sigma at development period 6, which is driven by the large 
#   incremental value at development period 7, which impacts the link ratio between the 6th and 7th development periods for 
#   origin period 3.


## Step 5: Apply a bootstrap model using Mack's assumptions with all link ratios included, and review the summary statistics:

## Bootstrap settings:
## method = "Mack", "ODPConstant", "ODPNonConstant", "NegBinConstant", or "NegBinNonConstant"
## (For ODP and NegBin, NonConstant is usually more appropriate)
## replications = number of bootstrap replications
## seed = seed point for random number generator (if blank, it is different each time)
## BootstrapDist = "NP" for non-parametric, or "Gamma" or "Lognormal" for parametric (this is for the pseudo_data distribution)
## ForecastDist = "NP" for non-parametric, or "Gamma" or "Lognormal" for parametric (this is for the process distribution)

# Select bootstrap method
method <- "Mack"

#BootstrapDist = "NP" # non-parametric bootstrapping
BootstrapDist = "Gamma" # generating pseudo-data with Gamma distribution
#BootstrapDist = "Lognormal" # generating pseudo-data with Lognormal distribution

#ForecastDist <- "NP" # non-parametric forecasting
ForecastDist <- "Gamma" # forecasting with Gamma distribution
#ForecastDist <- "Lognormal" # forecasting with Lognormal distribution

replications <- 10000
seed <- 100 # seed point for bootstrapping

UserSqrtScale <- NULL # do not use user-defined scale parameters initially

Mack_Bstrap_Result <- Run_Bootstrap(Triangle, method=method, 
                               replications=replications, seed=seed, BootstrapDist=BootstrapDist, 
                               ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

#Table of results
Table_2 <- cbind(round(Mack_Bstrap_Result$Latest,0), round(Mack_Bstrap_Result$Avg_Reserve,0), 
                 round(Mack_Bstrap_Result$Latest+Mack_Bstrap_Result$Avg_Reserve,0), 
                 round(Mack_Bstrap_Result$SD_Reserve,0), round(100*Mack_Bstrap_Result$CoV_Reserve,1))
Table_2_Totals <- cbind(round(sum(Mack_Bstrap_Result$Latest),0), round(Mack_Bstrap_Result$Avg_TotalReserve,0), 
                        round(sum(Mack_Bstrap_Result$Latest)+Mack_Bstrap_Result$Avg_TotalReserve,0), 
                        round(Mack_Bstrap_Result$SD_TotalReserve,0), round(100*Mack_Bstrap_Result$CoV_TotalReserve,1))
Table_2 <- rbind(Table_2, Table_2_Totals)
dimnames(Table_2) <- list(c(1:length(Triangle), "Total"), c("Latest", "Avg Reserves", 
                                                            "Avg Ultimates", "Bstrap SD", "Reserves CoV %"))
names(dimnames(Table_2)) <- c("Year","Simulated (Undiscounted)")
Table_2

# Show set of summary statistics
ShowStats <- ShowSummaryStats(Mack_Bstrap_Result, Output="Reserves")
ShowStats <- ShowSummaryStats(Mack_Bstrap_Result, Output="Ultimates")

## Commentary:
#  
# Bootstrapping Mack's model with all link ratios included, we observe that the mean, standard deviation and coefficient of
#   variation of the reserves in total and by origin period all match closely the results from Mack's model applied analytically.
#
# The advantage of bootstrapping is that we obtain a full predictive distribution. We can observe that the minimum simulated
#   reserves are negative up to the 7th development period, implying that the minimum simulated ultimates are less than the latest
#   claims payments for those origin periods. For incurred data, this may be reasonable (since the cumulative incurred values are
#   often overstated initially), but for paid data, this is unlikely. For paid data where all incremental values are positive, it
#   might be better to bootstrap using the ODP model (see later).

## Step 5 (continued): Review the reserve development (fan) graphs:

BStrap_Cumulatives <- Mack_Bstrap_Result$Cumulatives

for (yr in 2:ncol(BStrap_Cumulatives)) {
  print(plotDevelopmentGraph(BStrap_Cumulatives,yr))
}

# Histograms - Bootstrap undiscounted reserves
plotHistogram_Total(Mack_Bstrap_Result$TotalReserve)
#plotHistogram_by_yr(Mack_Bstrap_Result$Reserves)

## Commentary:
#  
# The reserve development (fan) charts are useful since they tell us a lot about the characteristics of the model and forecasts.
# In this example, we can see that:
#  
#   1) For the older origin periods, the cumulative amounts reduce for some simulations, and the minimum simulated ultimates are
#     less than the latest payments. This is a characeristic of bootstrapping Mack's model. Although the simulated cumulative amounts
#     must be positive, they could be less than the previous cumulative amounts.
#
#   2) The fan charts suddenly expand at development period 7. This is a direct result of the large value of Mack's sigma observed
#     at development period 6, impacting the cumulative claims development between the 6th and 7th development periods


## Step 6: Perform a sensitivity analysis:

# Calculate sensitivities of each link ratio to Chain-ladder reserves and Mack's SD
# Use Mack's SD calculated analytically for speed
# This is a quick and dirty way to identify influential data points
Sensitivity_Results <- Sensitivities(Triangle)

Sensitivity_Results$S_Reserves
Sensitivity_Results$S_Reserves_diff
#Sensitivity_Results$S_Reserves_absdiff
Sensitivity_Results$S_Reserves_rank
Sensitivity_Results$S_ReservesSD
Sensitivity_Results$S_ReservesSD_diff
#Sensitivity_Results$S_ReservesSD_absdiff
Sensitivity_Results$S_ReservesSD_rank
Sensitivity_Results$S_ReservesCoV*100
Sensitivity_Results$S_ReservesCoV_diff*100
#Sensitivity_Results$S_ReservesCoV_absdiff*100
Sensitivity_Results$S_ReservesCoV_rank

## Commentary:
#  
# Excluding each link ratio in turn and recalculating Mack's model applied analytically is a quick way to identify
#   influential link ratios.
#
# In this example, the total reserves, standard deviation of the total reserves, and coefficient of variation of the
#   total reserves are shown as each link ratio is excluded. The reductions in reserves, standard deviations, and 
#   coefficients of variation are then shown and ranked according to the largest reductions.
#
# We can see that excluding the link ratio at development period 6 for origin period 3 causes the biggest reduction in
#   reserves, standard deviation and coefficient of variation. In fact, that link ratio alone reduces the coefficient
#   of variation from 20% to 13%, highlighting its influence. Note that excluding that ratio also reduces the reserves.
#
# The link ratio at development period 7 for origin period 3 then causes the second biggest reduction in standard 
#   deviation and coefficient of variation, but the reduction is much smaller.


## Step 7: Identify and exclude the top n influential ratios (eg top 3) and re-apply Mack's method analytically:

Masktmp <- matrix(1,nrow(Triangle)-1, ncol(Triangle)-1) # include all data initially
#Indicators[3,6] <- 0

# Find positions of top 3 ranks using Total Reserve SDs
for (rank_num in c(1, 2, 3)) {
  positions <- which(Sensitivity_Results$S_ReservesSD_rank == rank_num, arr.ind = TRUE)
  pos <- positions[1, ]
  Masktmp[pos[1], pos[2]] <- 0
}

# Apply Mack's method analytically, excluding the top 3 influential link ratios
Mack_Analytic_Result <- Mack_ChainLadder(Triangle, Mask=Masktmp)
Latest_by_yr <- Mack_Analytic_Result$Latest
TotalLatest <- sum(Latest_by_yr)
Latest <- c(Latest_by_yr, TotalLatest)
Reserves_by_yr <- Mack_Analytic_Result$Reserves
TotalReserve <- Mack_Analytic_Result$TotalReserves
Reserves <- c(Reserves_by_yr, TotalReserve)
Ultimates_by_yr <- Mack_Analytic_Result$Ultimates
TotalUltimate <- sum(Ultimates_by_yr)
Ultimates <- c(Ultimates_by_yr, TotalUltimate)
Mack_Res_SD_by_yr <- Mack_Analytic_Result$Reserves_SD
Mack_Res_TotalSD <- Mack_Analytic_Result$TotalReserve_SD
Mack_Res_SD <- c(Mack_Res_SD_by_yr, Mack_Res_TotalSD)
Mack_Res_CoV <- safe_divide(Mack_Res_SD, abs(Reserves))

Table_1 <- cbind(round(Latest,0), round(Reserves,0), round(Ultimates,0), round(Mack_Res_SD,0), round(100*Mack_Res_CoV,1))
dimnames(Table_1) <- list(c(1:length(Triangle), "Total"), c("Latest", "Reserves", "Ultimates", "Reserve SD", "Reserve CoV %"))
names(dimnames(Table_1)) <- c("Year","Analytic")
Table_1

## Commentary:
#  
# If the top 3 link ratios influencing the standard deviation are excluded, the reserves reduce from 331k to 296k, the standard
#   deviation reduces from 69k to 30k, and the coefficient of variation reduces from 21% to 10%.
#
# It may not be appropriate to exclude any of the link ratios (that is for the analyst to justify), but we are in a more informed
#   position knowing which data points are driving volatility.


## Step 8: Investigate the impact of influential link ratios on the residuals and variance parameters for the Mack's model:

method <- "Mack"
Resids <- Calc_Residuals(Triangle, method=method, Mask=Masktmp)

# show adjusted unscaled residuals
adj_unscaled_resids <- round(Resids$adjunscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)-1), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)-1), sep=" ")
dimnames(adj_unscaled_resids) <- list(op_labels_2, dp_labels_2)
adj_unscaled_resids

# show zero-average scaled residuals
zeroavg_scaled_resids <- round(Resids$zeroavgscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)-1), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)-1), sep=" ")
dimnames(zeroavg_scaled_resids) <- list(op_labels_2, dp_labels_2)
zeroavg_scaled_resids

# Show variance parameters (Mack's sigma or ODP sqrtScale)
sigma <- round(Resids$sqrtScale, 1)
names(sigma) <- paste0("DP", 1:(length(Triangle)-1), sep=" ")
cat("Mack's sigma parameters:\n")
sigma

# Plot residuals
plotResiduals(Resids, resid_type="adjunscaledresids", bstrap_method=method)
plotResiduals(Resids, resid_type="zeroavgscaledresids", bstrap_method=method)

## Commentary:
#  
# Excluding link ratios also excludes the associated residuals, which has a follow-on impact on the calculation of the 
#   parameters in the variance. In particular, excluding the link ratio at development period 6 for origin period 3 has
#   dramatically reduced Mack's sigma at development period 6. This, in turn, reduces the standard deviation of the 
#   reserves significantly.


## Step 9: Re-apply the bootstrap method using Mack's assumptions after excluding the most influential ratios:

Mack_Bstrap_Result <- Run_Bootstrap(Triangle, Mask=Masktmp, method=method, 
                               replications=replications, seed=seed, BootstrapDist=BootstrapDist, 
                               ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

#Table of results
Table_2 <- cbind(round(Mack_Bstrap_Result$Latest,0), round(Mack_Bstrap_Result$Avg_Reserve,0), 
                 round(Mack_Bstrap_Result$Latest+Mack_Bstrap_Result$Avg_Reserve,0), 
                 round(Mack_Bstrap_Result$SD_Reserve,0), round(100*Mack_Bstrap_Result$CoV_Reserve,1))
Table_2_Totals <- cbind(round(sum(Mack_Bstrap_Result$Latest),0), round(Mack_Bstrap_Result$Avg_TotalReserve,0), 
                        round(sum(Mack_Bstrap_Result$Latest)+Mack_Bstrap_Result$Avg_TotalReserve,0), 
                        round(Mack_Bstrap_Result$SD_TotalReserve,0), round(100*Mack_Bstrap_Result$CoV_TotalReserve,1))
Table_2 <- rbind(Table_2, Table_2_Totals)
dimnames(Table_2) <- list(c(1:length(Triangle), "Total"), c("Latest", "Avg Reserves", 
                                                            "Avg Ultimates", "Bstrap SD", "Reserves CoV %"))
names(dimnames(Table_2)) <- c("Year","Simulated (Undiscounted)")
Table_2

# Show set of summary statistics
ShowStats <- ShowSummaryStats(Mack_Bstrap_Result, Output="Reserves")
ShowStats <- ShowSummaryStats(Mack_Bstrap_Result, Output="Ultimates")

## Commentary:
#  
# Bootstrapping Mack's model excluding the top 3 most influential link ratios on the standard deviation, we observe that the
#   mean, standard deviation and coefficient of variation of the reserves in total and by origin period all match closely 
#   the results from Mack's model applied analytically with the same exclusions.
#
# Furthermore, there is a much lower chance of negative reserves with this example.


## Step 9 (continued): Review the reserve development (fan) charts again after excluding the most influential ratios:

BStrap_Cumulatives <- Mack_Bstrap_Result$Cumulatives

for (yr in 2:ncol(BStrap_Cumulatives)) {
  print(plotDevelopmentGraph(BStrap_Cumulatives,yr))
}

# Histograms - Bootstrap undiscounted reserves
plotHistogram_Total(Mack_Bstrap_Result$TotalReserve)
#plotHistogram_by_yr(Mack_Bstrap_Result$Reserves)

## Commentary:
#  
#  From the reserve development (fan) charts we can see that:
#  
#   1) For the older origin periods, the cumulative amounts do not appear to reduce for any simulations.
#
#   2) The fan charts no longer suddenly expand at development period 7. The values of Mack's sigma have 
#     a smoother development, which is evident in the fan charts.


## Step 10: To further understand the impact of the variance parameters, manually adjust the variance parameters
#   (Mack's sigma) where appropriate:

# Excluding link ratios obviously impacts the mean as well as the variance. The level of volatility can also be controlled by
# manually adjusting the parameters in the variance (ie Mack's sigma or sqrtScale of the ODP model).

# Initially set the user-defined variance parameters to the original values with all link ratios included,
# then reduce the sixth value that is causing the development graph to fan out significantly.

UserSqrtScale <- sigma_original
UserSqrtScale

# Mack:
UserSqrtScale[6] <- 13
UserSqrtScale

# Repeat bootstrapping with user-defined scale parameters

Indicators <- matrix(1,nrow(Triangle)-1, ncol(Triangle)-1) # include all data

Mack_Bstrap_Result <- Run_Bootstrap(Triangle, Mask=Indicators, method=method, 
                               replications=replications, seed=seed, BootstrapDist=BootstrapDist, 
                               ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

#Table of results
Table_2 <- cbind(round(Mack_Bstrap_Result$Latest,0), round(Mack_Bstrap_Result$Avg_Reserve,0), 
                 round(Mack_Bstrap_Result$Latest+Mack_Bstrap_Result$Avg_Reserve,0), 
                 round(Mack_Bstrap_Result$SD_Reserve,0), round(100*Mack_Bstrap_Result$CoV_Reserve,1))
Table_2_Totals <- cbind(round(sum(Mack_Bstrap_Result$Latest),0), round(Mack_Bstrap_Result$Avg_TotalReserve,0), 
                        round(sum(Mack_Bstrap_Result$Latest)+Mack_Bstrap_Result$Avg_TotalReserve,0), 
                        round(Mack_Bstrap_Result$SD_TotalReserve,0), round(100*Mack_Bstrap_Result$CoV_TotalReserve,1))
Table_2 <- rbind(Table_2, Table_2_Totals)
dimnames(Table_2) <- list(c(1:length(Triangle), "Total"), c("Latest", "Avg Reserves", 
                                                            "Avg Ultimates", "Bstrap SD", "Reserves CoV %"))
names(dimnames(Table_2)) <- c("Year","Simulated (Undiscounted)")
Table_2

# Show set of summary statistics
ShowStats <- ShowSummaryStats(Mack_Bstrap_Result, Output="Reserves")
ShowStats <- ShowSummaryStats(Mack_Bstrap_Result, Output="Ultimates")

## Commentary:
#  
# If all link ratios are included again, but the value of Mack's sigma is reduced manually from the original value of 55 to
#   13, the mean is unchanged from its original value (subject to simulation error), but the standard deviation and coefficient
#   of variation are reduced.
#
# Note also that there is less of an issue with negative reserves.
#
# User-defined variance parameters are an alternative to excluding link ratios for controlling the level of volatility, and 
#   can be used carefully and knoweledgably once the drivers of volatility have been identified.


## Step 10 (continued): Review the reserve development (fan) charts again after manually adjusting the variance parameters:

BStrap_Cumulatives <- Mack_Bstrap_Result$Cumulatives

for (yr in 2:ncol(BStrap_Cumulatives)) {
  print(plotDevelopmentGraph(BStrap_Cumulatives,yr))
}

# Histograms - Bootstrap undiscounted reserves
plotHistogram_Total(Mack_Bstrap_Result$TotalReserve)
#plotHistogram_by_yr(Mack_Bstrap_Result$Reserves)

## Commentary:
#  
# Notice that the reserve development (fan) charts no longer suddenly expand at development period 7 since the values of Mack's 
#   sigma have a smoother development.


## Step 11: Investigate similar results for the ODP model with non-constant scale parameters. Review residuals and variance
#   parameters for the ODP model:

# Start with all link ratios included. There is no need to calculate the SD of the forecasts analytically (closed form),
# we can proceed straight to bootstrapping. First, look at the residuals and the square root of the scale parameters.

# Calculate residuals for the ODP model with non-constant scale parameters including all ratios:
method <- "ODPNonConstant"
Resids <- Calc_Residuals(Triangle, method=method)

# show adjusted unscaled residuals
adj_unscaled_resids <- round(Resids$adjunscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)), sep=" ")
dimnames(adj_unscaled_resids) <- list(op_labels_2, dp_labels_2)
adj_unscaled_resids

# show zero-average scaled residuals
zeroavg_scaled_resids <- round(Resids$zeroavgscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)), sep=" ")
dimnames(zeroavg_scaled_resids) <- list(op_labels_2, dp_labels_2)
zeroavg_scaled_resids

# Show variance parameters (Mack's sigma or ODP sqrtScale)
sqrtScale <- round(Resids$sqrtScale, 1)
names(sqrtScale) <- paste0("DP", 1:(length(Triangle)), sep=" ")
cat("ODP sqrtScale parameters:\n")
sqrtScale
sqrtScale_original <- Resids$sqrtScale # save for later

# Plot residuals
plotResiduals(Resids, resid_type="adjunscaledresids", bstrap_method=method)
plotResiduals(Resids, resid_type="zeroavgscaledresids", bstrap_method=method)

## Commentary:
#  
# Using the over-dispersed Poisson (ODP) model, the inference is similar, except it is a model of the incremental amounts
#   with a 10 by 10 triangle of residuals for this example.
#
# When all link ratios are included, we observe a large unscaled residual at development period 7 for origin period 3 
#   (consistent with the large incremental value at that position), which is driving the large value of the variance 
#   parameter (sqrtScale) at development period 7.


## Step 11 (continued): Bootstrap the over-dispersed Poisson model (ODP) with non-constant scale parameters, review summary 
#   statistics and compare with Mack's model results:

UserSqrtScale <- NULL # do not use user-defined scale parameters initially

ODP_Bstrap_Result <- Run_Bootstrap(Triangle, method=method, 
                               replications=replications, seed=seed, BootstrapDist=BootstrapDist, 
                               ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

#Table of results
Table_3 <- cbind(round(ODP_Bstrap_Result$Latest,0), round(ODP_Bstrap_Result$Avg_Reserve,0), 
                 round(ODP_Bstrap_Result$Latest+ODP_Bstrap_Result$Avg_Reserve,0), 
                 round(ODP_Bstrap_Result$SD_Reserve,0), round(100*ODP_Bstrap_Result$CoV_Reserve,1))
Table_3_Totals <- cbind(round(sum(ODP_Bstrap_Result$Latest),0), round(ODP_Bstrap_Result$Avg_TotalReserve,0), 
                        round(sum(ODP_Bstrap_Result$Latest)+ODP_Bstrap_Result$Avg_TotalReserve,0), 
                        round(ODP_Bstrap_Result$SD_TotalReserve,0), round(100*ODP_Bstrap_Result$CoV_TotalReserve,1))
Table_3 <- rbind(Table_3, Table_3_Totals)
dimnames(Table_3) <- list(c(1:length(Triangle), "Total"), c("Latest", "Avg Reserves", 
                                                            "Avg Ultimates", "Bstrap SD", "Reserves CoV %"))
names(dimnames(Table_3)) <- c("Year","Simulated (Undiscounted)")
Table_3

# Show set of summary statistics
ShowStats <- ShowSummaryStats(ODP_Bstrap_Result, Output="Reserves")
ShowStats <- ShowSummaryStats(ODP_Bstrap_Result, Output="Ultimates")

## Commentary:
#  
# Notice that when using non-constant scale parameters (where sqrtScale is calculated at each development period rather 
#   than being constant across all development periods), the standard deviation of total reserves for Mack's model and 
#   the ODP model are close (as we might expect).
#
# However, note that when bootstrapping the ODP model (with the parametric bootstrapping options for "BootstrapDist"), 
#   the minimum values of the reserves are positive at all origin periods when all observed incremental values are positive.


## Step 11 (continued): Review the reserve development (fan) charts for the ODP model:

BStrap_Cumulatives <- ODP_Bstrap_Result$Cumulatives

for (yr in 2:ncol(BStrap_Cumulatives)) {
  print(plotDevelopmentGraph(BStrap_Cumulatives,yr))
}

# Histograms - Bootstrap undiscounted reserves
plotHistogram_Total(ODP_Bstrap_Result$TotalReserve)
#plotHistogram_by_yr(ODP_Bstrap_Result$Reserves)

## Commentary:
#  
#   1) For the older origin periods, unlike Mack's model, the cumulative amounts do not reduce for some simulations when using
#   the ODP model. This is because the ODP model is a model of incremental amounts, and if the forecasts are simulated from a
#   parametric distribution (using ForecastDist=Gamma or Lognormal), the forecast incremental amounts must be positive.
#
#   2) Like Mack's model when all link ratios are included, the fan charts suddenly expand at development period 7. This is a 
#   direct result of the large value of sqrtScale observed at development period 7.


## Step 12: Investigate the impact of influential link ratios on the residuals and variance parameters for the ODP model:

# The ODP model will be influenced by the same link ratios as Mack's model, so we can use the results of the sensitivity analysis
# performed above.
#
# Repeat the analysis using the ODP model, excluding the top 3 influential ratios associated with Total Reserve SDs
#
# Residuals for the ODP model excluding the top 3 influential ratios on Total Reserve SDs:

Resids <- Calc_Residuals(Triangle, method=method, Mask=Masktmp)

# show adjusted unscaled residuals
adj_unscaled_resids <- round(Resids$adjunscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)), sep=" ")
dimnames(adj_unscaled_resids) <- list(op_labels_2, dp_labels_2)
adj_unscaled_resids

# show zero-average scaled residuals
zeroavg_scaled_resids <- round(Resids$zeroavgscaledresids,3)
dp_labels_2 <- paste("DP", 1:(length(Triangle)), sep=" ")
op_labels_2 <- paste("OP", 1:(length(Triangle)), sep=" ")
dimnames(zeroavg_scaled_resids) <- list(op_labels_2, dp_labels_2)
zeroavg_scaled_resids

# Show variance parameters (Mack's sigma or ODP sqrtScale)
sqrtScale <- round(Resids$sqrtScale, 1)
names(sqrtScale) <- paste0("DP", 1:(length(Triangle)), sep=" ")
cat("ODP sqrtScale parameters:\n")
sqrtScale

# Plot residuals
plotResiduals(Resids, resid_type="adjunscaledresids", bstrap_method=method)
plotResiduals(Resids, resid_type="zeroavgscaledresids", bstrap_method=method)

## Commentary:
#
# Excluding the most influential link ratios identified from the sensitivity analysis above for Mack's model also excludes
#   the associated residuals for the ODP model, which has a follow-on impact on the calculation of the parameters in the 
#   variance (sqrtScale).
#
#By convention, if link ratios between development periods 1-2 are excluded, then both ODP residuals at development periods
#   1 and 2 are excluded, otherwise just exclude the residual associated with the position of the numerator of the link ratio.
#
#Excluding the link ratio at development period 6 for origin period 3 excludes the ODP residual at development period 7, 
#   which dramatically reduces sqrtScale at development period 7. This, in turn, will reduce the standard deviation of the 
#   reserves significantly.


## Step 12 (continued): Re-apply the bootstrap method using the ODP model after excluding the most influential ratios:

ODP_Bstrap_Result <- Run_Bootstrap(Triangle, Mask=Masktmp, method=method, 
                               replications=replications, seed=seed, BootstrapDist=BootstrapDist, 
                               ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

#Table of results
Table_3 <- cbind(round(ODP_Bstrap_Result$Latest,0), round(ODP_Bstrap_Result$Avg_Reserve,0), 
                 round(ODP_Bstrap_Result$Latest+ODP_Bstrap_Result$Avg_Reserve,0), 
                 round(ODP_Bstrap_Result$SD_Reserve,0), round(100*ODP_Bstrap_Result$CoV_Reserve,1))
Table_3_Totals <- cbind(round(sum(ODP_Bstrap_Result$Latest),0), round(ODP_Bstrap_Result$Avg_TotalReserve,0), 
                        round(sum(ODP_Bstrap_Result$Latest)+ODP_Bstrap_Result$Avg_TotalReserve,0), 
                        round(ODP_Bstrap_Result$SD_TotalReserve,0), round(100*ODP_Bstrap_Result$CoV_TotalReserve,1))
Table_3 <- rbind(Table_3, Table_3_Totals)
dimnames(Table_3) <- list(c(1:length(Triangle), "Total"), c("Latest", "Avg Reserves", 
                                                            "Avg Ultimates", "Bstrap SD", "Reserves CoV %"))
names(dimnames(Table_3)) <- c("Year","Simulated (Undiscounted)")
Table_3

# Show set of summary statistics
ShowStats <- ShowSummaryStats(ODP_Bstrap_Result, Output="Reserves")
ShowStats <- ShowSummaryStats(ODP_Bstrap_Result, Output="Ultimates")

## Commentary:
#
# Bootstrapping the ODP model excluding the top 3 most influential link ratios on the standard deviation, we observe that
#   the mean, standard deviation and coefficient of variation of the reserves in total and by origin period all reduce 
#   significantly. Note that they match closely the results from Mack's model with the same exclusions, with the coefficient 
#   of variation of the total reserves being approximatley 10%.


## Step 12 (continued): Review the reserve development (fan) charts again after excluding the most influential ratios:

BStrap_Cumulatives <- ODP_Bstrap_Result$Cumulatives

for (yr in 2:ncol(BStrap_Cumulatives)) {
  print(plotDevelopmentGraph(BStrap_Cumulatives,yr))
}

# Histograms - Bootstrap undiscounted reserves
plotHistogram_Total(ODP_Bstrap_Result$TotalReserve)
#plotHistogram_by_yr(ODP_Bstrap_Result$Reserves)

## Commentary:
#  
# From the reserve development charts we can see that the fan charts no longer suddenly expand at development period 7. 
#   The values of sqrtScale have a smoother development, which is evident in the fan charts.


## Step 13: To further understand the impact of the variance parameters, manually adjust the variance parameters (ODP sqrtScale)
#   where appropriate:

# Excluding link ratios obviously impacts the mean as well as the variance. The level of volatility can also be controlled by
# manually adjusting the parameters in the variance (ie Mack's sigma or sqrtScale of the ODP model).
#
# Initially set the user-defined variance parameters to the original values with all link ratios included,
# then reduce the sixth value that is causing the development graph to fan out significantly.

UserSqrtScale <- sqrtScale_original
UserSqrtScale

# ODP
UserSqrtScale[7] <- 40
UserSqrtScale

# Repeat bootstrapping with user-defined scale parameters

Indicators <- matrix(1,nrow(Triangle)-1, ncol(Triangle)-1) # include all data

ODP_Bstrap_Result <- Run_Bootstrap(Triangle, Mask=Indicators, method=method, 
                               replications=replications, seed=seed, BootstrapDist=BootstrapDist, 
                               ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

#Table of results
Table_3 <- cbind(round(ODP_Bstrap_Result$Latest,0), round(ODP_Bstrap_Result$Avg_Reserve,0), 
                 round(ODP_Bstrap_Result$Latest+ODP_Bstrap_Result$Avg_Reserve,0), 
                 round(ODP_Bstrap_Result$SD_Reserve,0), round(100*ODP_Bstrap_Result$CoV_Reserve,1))
Table_3_Totals <- cbind(round(sum(ODP_Bstrap_Result$Latest),0), round(ODP_Bstrap_Result$Avg_TotalReserve,0), 
                        round(sum(ODP_Bstrap_Result$Latest)+ODP_Bstrap_Result$Avg_TotalReserve,0), 
                        round(ODP_Bstrap_Result$SD_TotalReserve,0), round(100*ODP_Bstrap_Result$CoV_TotalReserve,1))
Table_3 <- rbind(Table_3, Table_3_Totals)
dimnames(Table_3) <- list(c(1:length(Triangle), "Total"), c("Latest", "Avg Reserves", 
                                                            "Avg Ultimates", "Bstrap SD", "Reserves CoV %"))
names(dimnames(Table_3)) <- c("Year","Simulated (Undiscounted)")
Table_3

# Show set of summary statistics
ShowStats <- ShowSummaryStats(ODP_Bstrap_Result, Output="Reserves")
ShowStats <- ShowSummaryStats(ODP_Bstrap_Result, Output="Ultimates")

## Commentary:
#  
# If all link ratios are included again, but the value of sqrtScale is reduced manually from the original value of 112 to 40, 
#   the mean is unchanged from its original value (subject to simulation error), but the standard deviation and coefficient of
#   variation are reduced.
#
# As with Mack's model, user-defined variance parameters are an alternative to excluding link ratios for controlling the 
#   level of volatility, and can be used carefully and knoweledgably once the drivers of volatility have been identified.


## Step 13 (continued): Review the reserve development (fan) charts again after manually adjusting the variance parameters:

BStrap_Cumulatives <- ODP_Bstrap_Result$Cumulatives

for (yr in 2:ncol(BStrap_Cumulatives)) {
  print(plotDevelopmentGraph(BStrap_Cumulatives,yr))
}

# Histograms - Bootstrap undiscounted reserves
plotHistogram_Total(ODP_Bstrap_Result$TotalReserve)
#plotHistogram_by_yr(ODP_Bstrap_Result$Reserves)

## Commentary:
#  
# Notice that the reserve development (fan) charts no longer suddenly expand at development period 7 since the values of 
#   sqrtScale have a smoother development.


## Step 14: Final selection of which model to use, which ratios to exclude (if any), and whether to apply user-defined 
#   variance parameters (Mack's sigma or the ODP sqrtScale parameters):

# After understanding what is driving volatility, and the characteristics and limitations of the models chosen, the 
#   actuarial analyst is in a better position to make final selections based on informed judgement.
#
# Where forecasts are required beyond the final development period in the triangle ('tail' forecasts), it is common 
#   practice to fit curves to the fitted development factors (or to the triangle of link ratios) and extrapolate into 
#   the tail. Such practical extensions can be incorporated into the bootstrap process, but are not considered in this 
#   example code.
#
# In practice, a further final step might be to scale the distribution of results to target ultimate claims estimates 
#   from a "best estimate" reserving analysis.


## Step 15: Scale the results to target ultimate claims if required:

# Test scaling of results to a target ultimate

# Rationale: Bootstrapping usually relies on simple link ratio type models, and the mean of the simulations reflects this.
# However, the "best estimate" reserves from a reserving analysis may use diferent methods, or incorporate elements 
# of expert judgement, and may differ from the mean of the simulations. 
# Scaling allows us to adjust the simulation results to match a target ultimate, while preserving the distributional characteristics of 
# the simulations.

# With Additive scaling, the absolute standard deviation is preserved, but the coefficient of variation changes.
# With Multiplicative scaling, the coefficient of variation is preserved, but the absolute standard deviation changes.

#Unscaled_Results = Mack_Bstrap_Result
Unscaled_Results <- ODP_Bstrap_Result

# Create target ultimate as latest plus 1.1 times the average reserve across all iterations just as an example
# Usually the target ultimates would be derived from a "best estimate" reserving exercise,
# ideally excluding any prudential margin
Target_Ultimates <- Unscaled_Results$Latest + Unscaled_Results$Avg_Reserve * 1.1
num_origin_periods <- length(Unscaled_Results$Avg_Reserve)
#scaling_method <- rep("Additive", num_origin_periods) # Additive for all periods
#scaling_method <- rep("Multiplicative", num_origin_periods) # Multiplicative for all periods
scaling_method <- c(rep("Additive", 5), rep("Multiplicative", num_origin_periods-5)) # Additive for the first five periods, multiplicative thereafter

Scaled_Results_Test <- Scaled_Results(Unscaled_Results, Target_Ultimates, scaling_method)

# Calculate summary statistics for unscaled results
Unscaled_Stats <- ShowSummaryStats(Unscaled_Results)

# Calculate summary statistics for scaled results
Scaled_Stats <- ShowSummaryStats(Scaled_Results_Test)

## Commentary:
#  
#  In this fictitious example, target ultimate claims have been calculated as the latest cumulative claims plus the 
#   mean reserves from the variability analysis increased by 10%. This is simply to highlight the approach.
#
# With "Additive" scaling, the reserves by origin period for each simulation are simply shifted by the difference between
#   the target reserves and the mean reserves from the bootstrap analysis. This preserves the absolute standard deviation, 
#   but changes the coefficient of variation.
#
# With "Multiplicative" scaling, the reserves by origin period for each simulation are simply scaled by the ratio of the
#   target reserves to the mean reserves from the bootstrap analysis. This preserves the coefficient of variation, but 
#   changes the absolute standard deviation.
#
# The user can select which scaling approach to use for each origin period. In the example above, Additive scaling has 
#   been used for the first 5 origin periods, and Multiplicative scaling has been used for the remaining 5 origin periods.
#
# Note that when comparing the unscaled and scaled summary statistics, the means have changed, but the absolute standard 
#   deviation has been retained for the first 5 origin periods and the coefficient of variation has been retained for the
#   remaining 5 origin periods.
#
# If the cash flows are also required, then it is also necessary to scale the cash flows from the bootstrap simulations in 
#   an appropriate way.
#
# It should be noted that scaling the bootstrap results to a target ultimate is a pragmatic approach that recognises that 
#   a simple stochastic model cannot replicate the nuances that might be included in reserve estimates for other purposes. 
#   Ideally, minimal scaling would be applied, and if too much scaling is required, the analyst should stop and investigate 
#   further. Obviously "too much" is subjective and is a matter of opinion.
#
# Furthermore, the stochastic models presented here are based on traditional volume-weighted chain ladder models. If the 
#   underlying data are such that chain ladder type models would never be used for reserve analysis, they should not be 
#   used for reserve variability analysis either; alternative more appropriate models should be used instead.


## Final comments: Common errors and fallacies
#
# It is common to hear actuarial analysts say that they used "THE" bootstrap model in their variability analysis. 
#   Bootstrapping is just a statistical procedure that can be applied to well-specified statistical models. When actuarial
#   analysts say they used "THE" bootstrap model, they usually mean the over-dispersed Poisson chain ladder model with 
#   constant scale parameter, originally published by England & Verall (1999). England & Verrall subsequently showed how to
#   bootstrap other models, including the over-dispersed Poisson model with non-constant scale parameters and Mack's model, 
#   as shown in the examples above. As such, there is no such thing as "THE" bootstrap model, and it is necessary to be more 
#   precise in the description of the model used. In general, when the over-dispersed Poisson model is used, non-constant 
#   scale parameters are preferable.
#
# The choice between using the over-dispersed Poisson model and Mack's model depends on negative observed incremental values
#   and the desirability (or otherwise) of having forecast negative incremental values. Strictly, the ODP model requires chain
#   ladder development factors greater than 1, and since it is a model of incremental values, it will give positive simulated 
#   forecast incremental claims. As such, it is less suitable for bootstrapping incurred claims triangles, which often display 
#   negative incremental amounts. [Note: the functions for bootstrapping the ODP model in this example code have been adapted 
#   to allow negative values in a pragmatic way.]
#
# Mack's model is a model of cumulative amounts (or, in fact, a model of link ratios), and can be used with negative 
#   incremental amounts and when chain ladder development factors are less than 1. Even if observed incremental values are all 
#   positive, it is always possible to obtain simulated negative incremental forecast values due the random nature of the 
#   simulation process when bootstrapping Mack's model.
#
# It is sometimes believed that bootstrapping cannot be used with incurred data due to the presence of negative incremental 
#   values. Clearly this is not true. With incurred data, if there are development factors less than 1, just bootstrap using 
#   Mack's model assumptions. It should be noted that when using incurred data, the bootstrap procedure gives a distribution of 
#   "IBNR+IBNER" (not "reserves") and also a distribution of ultimates. A distribution of "reserves" can easily be obtained by 
#   subtracting the latest paid amounts from each simulated ultimate value. Since the latest paid amounts are constant, the 
#   absolute standard deviation of the "reserves" will be the same as the standard deviation of the ultimates (and the standard 
#   deviation of the "IBNR+IBNER"). However, if the focus is on the coefficient of variation of the reserves, the standard 
#   deviation (of the "IBNR+IBNER" from the incurred analysis) must be divided by the mean reserves to obtain a meaningful number.
#
# A further common fallacy is that parametric bootstrapping involves simulating residuals from a parametric distribution 
#   (eg Normal) before inverting to obtain pseudo-data in the bootstrap process. This is not parametric bootstrapping. 
#   Parametric bootstrapping involves simulating pseudo-data directly from a parametric distribution, given the mean and 
#   variance of the pseudo-data. This is generally preferable to non-parametric bootstrapping, where residuals are resampled 
#   before inverting to obtain pseudo-data. Details of non-parametric and parametric bootstrapping can be obtained for Mack's 
#   model and the ODP model by reviewing the relevant example functions.


