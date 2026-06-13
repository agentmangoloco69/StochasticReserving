# version 1.0.0
#
## Stochastic reserving functions in R to support reproduction of the tables in:
#
# England, Verrall & Wüthrich (2018/2019). On the lifetime and one-year views of reserve risk, 
# with application to IFRS 17 and Solvency II risk margins.
# Insurance: Mathematics and Economics (2019) https://doi.org/10.1016/j.insmatheco.2018.12.002
# (Pre-print version available from SSRN (2018) https://ssrn.com/abstract=3141239)
#
# and
#
# England & Verrall (2006). Predictive distributions of outstanding liabilities in general insurance,
# Annals of Actuarial Science, 1, II, 221-270.
#
# This code is provided by Peter England on behalf of EMC Actuarial and Analytics Ltd as an educational resource.


## General functions

safe_divide <- function(numerator, denominator, fill_value = 0) {
  # Return user-defined value where denominator is zero
  ifelse(denominator == 0, fill_value, numerator / denominator)
  }

LinkRatiofunction <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1), periods=0){
  # Function to calculate link ratios between development periods for a triangle, based on a vol-wtd average
  # Input is a triangle, output is a vector of link ratios
  # If periods > 0, ratios are calculated from cumulatives including future calendar period of data (this
  # is useful for CDR calculations)
  nr <- nrow(tridata)
  nc = ncol(tridata)
  LinkRatio <- vector("double",length=nc-1)
  LRTriangle <- matrix(NA,nr,nc-1)
  LRweights <- matrix(NA,nr,nc-1)
  Linkratio_tmp <- matrix(NA,nr,nc-1)
  LRweights_tmp <- matrix(NA,nr,nc-1)
  
  for (ii in 1:min(nr-1+periods,nr)){
    for (jj in 1:min(nc-ii+periods,nc-1)){ 
      LRTriangle[ii,jj] <- tridata[ii,jj+1]/tridata[ii,jj]
      LRweights[ii,jj] <- tridata[ii,jj]
    }
  }
  
  Mask <- rbind(Mask, rep(1,ncol(Mask)))
  
  LRweights_tmp <- LRweights*Mask
  Linkratio_tmp <- LRweights_tmp*LRTriangle
  for (ii in 1:(nc-1)){
    LinkRatio[ii]<-sum(Linkratio_tmp[1:min(nr-ii+periods,nr),ii],na.rm=TRUE)/sum(LRweights_tmp[1:min(nr-ii+periods,nr),ii],na.rm=TRUE)
  }
  LinkRatio[is.na(LinkRatio)] <- 1.0
  return(LinkRatio)
}

Augment_Tri <- function(tridata, cumulatives, iteration, periods) {
  # This funtion augments a claims triangle with future calendar periods of claims
  # The function is not used since all CDR calculations can be done from complete set of
  # simulated cumulatives without cutting down
  nc <- ncol(tridata)
  newtri <- matrix(NA,nc,nc)
  for (ii in 1:nc) {
    for (jj in 1:min(nc-ii+1+periods,nc)) {
      newtri[ii,jj] <- cumulatives[iteration,ii,jj]
    }
  }
  return(newtri)
}

LR_Tri <- function(tridata){
  #Function to calculate a link ratio triangle and weights
  n <- ncol(tridata)
  LinkRatioTri <- matrix(NA,(n-1),(n-1))
  Weight <- matrix(NA,(n-1),(n-1))
  for (ii in 1:(n-1)){
    for (jj in 1:(n-ii)){ 
      LinkRatioTri[ii,jj] <- safe_divide(tridata[ii,(jj+1)],tridata[ii,jj])
      Weight[ii,jj] <- tridata[ii,jj]
    }
  }
  result <- list()
  result$LRs <- LinkRatioTri
  result$Weight <- Weight
  return(result)
}

CL_factors <- function(LRTriangle, LRweights, Mask=matrix(1,nrow(LRTriangle), ncol(LRTriangle))){
  # Function to calculate link ratios between development periods for a triangle, based on a weighted average
  # of ratios (Volume weighted avg)
  n <- ncol(LRTriangle)
  LinkRatio <- vector("double",length=n)
  Linkratio_tmp <- matrix(NA,n,n)
  LRweights_tmp <- matrix(NA,n,n)
 
  LRweights_tmp <- LRweights*Mask
  Linkratio_tmp <- LRweights_tmp*LRTriangle
  for (ii in 1:n){
    LinkRatio[ii]<-sum(Linkratio_tmp[1:(n-ii+1),ii],na.rm=TRUE)/sum(LRweights_tmp[1:(n-ii+1),ii],na.rm=TRUE)
  }
  LinkRatio[is.na(LinkRatio)] <- 1.0
  return(LinkRatio)
}

Incrementals <- function(Cumulatives){
  # Function to convert a triangle of cumulatives to incrementals
  # assumes no tail
  nc <- ncol(Cumulatives)
  Incrementals <- matrix(NA, nc,nc)
  Incrementals[,1] <- Cumulatives[,1]
  for (jj in 2:nc) {
    Incrementals[,jj] <- Cumulatives[,jj] - Cumulatives[,jj-1]
  }
  return(Incrementals)
}

Cumulatives <- function(Incrementals) {
  # Function to convert a triangle of incrementals to cumulatives
  # assumes no tail
  nc <- ncol(Incrementals)
  Cumulatives <- matrix(NA, nc,nc)
  Cumulatives[,1] <- Incrementals[,1]
  for (jj in 2:nc) {
    Cumulatives[,jj] <- Incrementals[,jj] + Cumulatives[,jj-1]
  }
  return(Cumulatives)
}

Tri_forecast <- function(tridata, factors, periods=0) {
  # Function to forecast cumulative claims given a set of development factors
  # If periods > 0, forecasting is from a given future calendar period (this
  # is useful for CDR calculations)
  nc <- ncol(tridata)
  cumulatives <- tridata
  if (periods<(nc-1)) {
    for (ii in min(2+periods,nc):nc) {
      for (jj in min(nc-ii+2+periods,nc):nc) {
        cumulatives[ii,jj] <- cumulatives[ii,jj-1]*factors[jj-1]
      }
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  Latest <- vector("numeric",nc)
  for (ii in 1:nc) {
    Latest[ii] <- cumulatives[ii,min(nc-ii+1+periods,nc)]
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - Latest[ii]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Latest <- Latest
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <- TotalReserve
  return(result)
}

Recursive_SD <- function(tridata, LinkRatios_SE=rep(0.1, ncol(tridata)-1), sqrtScale=rep(1, ncol(tridata)-1), model="Mack", Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)) {
  
  # Function to Calculate SD of Chain Ladder Reserves using recursive formulae from Appendix of England & Verrall (2006) 
  # This function works for Mack's model or the Negative Binomial chain ladder model
  
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  Cum_CL_facs <- rev(cumprod(rev(CL_facs)))
  
  np <- length(CL_facs)
  var_factors <- array(0,np)
  var_factors[np] <- LinkRatios_SE[np]^2
  for (ii in (np-1):1) {
    var_factors[ii] <- CL_facs[ii]^2*var_factors[ii+1]+Cum_CL_facs[ii+1]^2*LinkRatios_SE[ii]^2+LinkRatios_SE[ii]^2*var_factors[ii+1]
  }
  Latest <- CL_Result$Latest[(np+1):2]
  Est_Error_tmp <- (Latest^2)*var_factors
  Est_Error_Cov <- matrix(0,np,np)
  for (ii in 1:(np-1)) {
    for (jj in (ii+1):np) {
      Est_Error_Cov[ii,jj] <- Latest[jj]*var_factors[jj]*Latest[jj-ii]*prod(CL_facs[(jj-ii):(jj-1)])
    }
  }
  Est_Error_OP <- sqrt(c(0,rev(Est_Error_tmp)))
  Est_Error_Total <- sqrt(sum(Est_Error_tmp)+2*sum(Est_Error_Cov))
  
  dispersion <- sqrtScale^2
  if (model=="Mack") {
    Process_Err_var <- dispersion
  }  else if (model=="NegBin") {
    Process_Err_var <- dispersion*CL_facs*(CL_facs-1)
  }
  Cum_Cl_facs_squared <- c(Cum_CL_facs^2,1)
  Process_Error_tmp <- matrix(0,np,np)
  for (ii in 1:np) {
    for (jj in (np-ii+1):np) {
      Process_Error_tmp[ii,jj] <- Process_Err_var[jj]*CL_Result$Cumulatives[ii+1,jj]*Cum_Cl_facs_squared[jj+1]
    }
  }
  
  Process_Err_OP <- sqrt(c(0,rowSums(Process_Error_tmp)))
  Process_Err_Total <- sqrt(sum(Process_Error_tmp))
  
  CL_Reserves <- CL_Result$Reserves
  CL_Reserves_SD <- sqrt(Process_Err_OP^2 + Est_Error_OP^2)
  CL_Reserves_CoV <- safe_divide(CL_Reserves_SD, abs(CL_Reserves))
  CL_TotalReserve <- CL_Result$TotalReserve
  CL_TotalReserve_SD <- sqrt(Process_Err_Total^2 + Est_Error_Total^2)
  CL_TotalReserve_CoV <- safe_divide(CL_TotalReserve_SD, abs(CL_TotalReserve))
  
  result <- list()
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_TotalReserve <- CL_Result$TotalReserve
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  
  result$CL_Reserves_SD <- CL_Reserves_SD
  result$CL_Reserves_CoV <- CL_Reserves_CoV
  result$CL_TotalReserve_SD <- CL_TotalReserve_SD
  result$CL_TotalReserve_CoV <- CL_TotalReserve_CoV
  
  return(result)
}

Row_covar_sum <- function(Covmatrix, nc) {
  # Helper function to sum blocks of increasing size down the leading diagonal of a matrix
  # Used for ODP RMSEPs
  Rowcovsum <- rep(0, nc+1)
  Vsum <- rep(0, nc)
  counter <- 1
  for(i in 1:(nc-1)) {
    Rtot <- 0
    counter=counter+i-1
    for(j in 1:i) {
      Vsum[i+1] <- Vsum[i+1] + Covmatrix[counter+j-1,counter+j-1]
      Rtot <- Rtot + sum(Covmatrix[counter+j-1, counter:(counter+j-1)])
    }
    Rowcovsum[i+1] <- 2 * Rtot - Vsum[i+1]
  }
  Rowcovsum[nc+1] <- sum(Covmatrix)
  Rowcovsum
}

Calc_RMSEPs <- function(tridata, method, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)) {
# Calculate analytic Reserve SDs and CDR SDs (where available)
  if (method=="Mack") {
    # add extra row and column to Mask for weights in ChainLadder package
    weights <- Mask
    weights <- cbind(Mask, rep(1,nrow(Mask)))
    weights <- rbind(weights,rep(1,ncol(weights)))
    # ensure final sigma value is min of previous 2 values to be consistent with bootstrap approach
    MackCL <- MackChainLadder(tridata, est.sigma="Mack", weights=weights)
    sigma <- MackCL$sigma
    final_sigma <- min(sigma[length(sigma)-1], sigma[length(sigma)-2])
    # apply MackChainLadder with selected final sigma value from min of previous 2 values
    MackCL <- MackChainLadder(tridata, est.sigma=final_sigma, weights=weights)
    CDR_SD <- CDR(MackCL, dev="all")
    CL_Res <- CDR_SD[,1]
    Res_SD <- CDR_SD[,dim(CDR_SD)[2]]
    CDR_SD_1Yr <- CDR_SD[,2]
    Model <- NULL
  } else if (method=="ODPConstant") {
    Scale <- "Constant"
    Model <- ODP_ChainLadder(tridata, Scale, Mask=Mask)
    CL_Res <- c(Model$Reserves, Model$TotalReserves)
    Res_SD <- c(Model$Reserves_SD, Model$TotalReserve_SD)
    nr <- length(Res_SD)
    CDR_SD <- matrix(NA, nrow=nr, ncol=nr+1)
    CDR_SD_1Yr <- rep(NA, nr)
  } else if (method=="ODPNonConstant") {
    Scale <- "NonConstant"
    Model <- ODP_ChainLadder(tridata, Scale, Mask=Mask)
    CL_Res <- c(Model$Reserves, Model$TotalReserves)
    Res_SD <- c(Model$Reserves_SD, Model$TotalReserve_SD)
    nr <- length(Res_SD)
    CDR_SD <- matrix(NA, nrow=nr, ncol=nr+1)
    CDR_SD_1Yr <- rep(NA, nr)
  } else if (method=="NegBinConstant") {
    Scale <- "Constant"
    Model <- NegBin_ChainLadder(tridata, Scale, Mask=Mask)
    CL_Res <- c(Model$Reserves, Model$TotalReserves)
    Res_SD <- c(Model$Reserves_SD, Model$TotalReserve_SD)
    nr <- length(Res_SD)
    CDR_SD <- matrix(NA, nrow=nr, ncol=nr+1)
    CDR_SD_1Yr <- rep(NA, nr)
  } else if (method=="NegBinNonConstant") {
    Scale <- "NonConstant"
    Model <- NegBin_ChainLadder(tridata, Scale, Mask=Mask)
    CL_Res <- c(Model$Reserves, Model$TotalReserves)
    Res_SD <- c(Model$Reserves_SD, Model$TotalReserve_SD)
    nr <- length(Res_SD)
    CDR_SD <- matrix(NA, nrow=nr, ncol=nr+1)
    CDR_SD_1Yr <- rep(NA, nr)
  }  
    
  result <- list()
  result$CL_Res <- CL_Res
  result$Res_SD <- Res_SD
  result$CDR_SD <- CDR_SD
  result$CDR_SD_1Yr <- CDR_SD_1Yr
  result$Model <- Model
  
  return(result)   
}

Analytic_RMSEPs <- function(tridata, method, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)) {
  # Calculate analytic Reserve SDs and CDR SDs (where available)
  if (method=="Mack") {
    result <- Mack_ChainLadder(Triangle, Mask=Indicators)
  } else if (method=="ODPConstant") {
    result <- ODP_ChainLadder(Triangle, Scale="Constant", Mask=Indicators)
  } else if (method=="ODPNonConstant") {
    result <- ODP_ChainLadder(Triangle, Scale="NonConstant", Mask=Indicators)
  } else if (method=="NegBinConstant") {
    result <- NegBin_ChainLadder(Triangle, Scale="Constant", Mask=Indicators)
  } else if (method=="NegBinNonConstant") {
    result <- NegBin_ChainLadder(Triangle, Scale="NonConstant", Mask=Indicators)
  }  
  
  return(result)   
}

Resample_Resids <- function(residsvector, nc) {
  # Function to provide a resampled set of residuals
  # This is set-up to provide residuals for both bootstrapping observed data (past triangle) and 
  # non-parametric forecasting (future triangle). Works for Mack and ODP bootstrap
  nr <- length(residsvector)
  ids <- sample(1:nr, nc*nc, replace=TRUE)
  # future triangle needed for non-parametric process error propagation
  randoms <- vector("numeric", nc*nc)
  randoms <- residsvector[ids]
  randoms <- matrix(randoms, nrow=nc)
  result <- randoms
  return(result)  
}

Run_Bootstrap <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1), 
                          method, replications=1000, seed = sample(1e6,1), 
                          BootstrapDist = "NP", ForecastDist="Gamma", UserSqrtScale=NULL) {
  # Control function for bootstrapping
  if (method=="Mack") {
    result <- Main_Mack_Bstrap(tridata, Mask, replications, seed, BootstrapDist, ForecastDist, UserSqrtScale)
  }
  else if (method=="ODPNonConstant") {
    result <- Main_ODP_Bstrap(tridata, Mask, replications, seed, Scale="NonConstant", BootstrapDist, ForecastDist, UserSqrtScale)
  }
  else if (method=="ODPConstant") {
    result <- Main_ODP_Bstrap(tridata, Mask, replications, seed, Scale="Constant", BootstrapDist, ForecastDist, UserSqrtScale)
  }
  else if (method=="NegBinNonConstant") {
    result <- Main_NegBin_Bstrap(tridata, Mask, replications, seed, Scale="NonConstant", BootstrapDist, ForecastDist, UserSqrtScale)
  }
  else if (method=="NegBinConstant") {
    result <- Main_NegBin_Bstrap(tridata, Mask, replications, seed, Scale="Constant", BootstrapDist, ForecastDist, UserSqrtScale)
  }
  return(result)
}

ODP_ChainLadder <- function(Triangle, Scale="NonConstant", Mask=matrix(1,nrow(Triangle)-1, ncol(Triangle)-1)) {
  ## Calculates analytic prediction errors for the ODP chain ladder model
  
  # Construct indicators from link ratio Mask using same method as for residuals
  # By convention, if ratios between development periods 1-2 are excluded, then
  # both observations at development periods 1 and 2 are excluded, otherwise
  # just the observation associated with the position of the numerator of the link ratio is excluded
  # Note: ODP reserves will not match vol-wtd chain ladder reserves when a Mask with zero values is used
  nc <- length(Triangle)
  
  Indicators <- matrix(1, nrow = nc, ncol = nc)
  
  for (i in 1:(nc - 1)) {
    for (j in 1:(nc-i+1)) {
      if (j <= 2) {
        Indicators[i, j] <- Mask[i, 1]
      } else {
        Indicators[i, j] <- Mask[i, j - 1]
      }
    }
  }
  
  # convert to Incremental data
  Inc_Triangle <- Incrementals(Triangle)
  Claims <- as.vector(Inc_Triangle)
  Indicators_vector <- as.vector(Indicators)
  cl_tmp <- Claims * Indicators_vector
  cl_tmp <- cl_tmp[!is.na(Claims)]
  ind_tmp <- Indicators_vector[!is.na(Claims)]
  
  #test_positive <- if (any(cl_tmp <= 0 & ind_tmp !=0)) {FALSE} else {TRUE}
  test_positive <- if (any(cl_tmp <= 0)) {FALSE} else {TRUE} # R doesn't like any negatives even if weighted out
  
  # Test for sum(incrementals) by dev period <= 0, which implies a development factor <= 1,
  # which invalidates the ODP model
  Inc_Triangle_tmp <- Inc_Triangle * Indicators
  col_sums <- colSums(Inc_Triangle_tmp, na.rm = TRUE)
  test_dev_factors <- if (any(col_sums <= 0)) {FALSE} else {TRUE}
    
  if (!test_positive || !test_dev_factors) {
    message("Invalid data for ODP model applied analytically: Negative values or Dev factors less than or equal to one detected")
    message("Proceed with bootstrapping instead")
    
    coefs          <- rep(NaN, 2*nc-1)
    parameter_se   <- rep(NaN, 2*nc-1)
    Latest         <- rep(NaN, nc)
    #TotalLatest    <- NaN
    Reserves       <- rep(NaN, nc)
    TotalReserves  <- NaN
    Ultimates      <- rep(NaN, nc)
    #TotalUltimates <- NaN
    Reserves_SD    <- rep(NaN, nc)
    Reserves_CoV   <- rep(NaN, nc)
    TotalReserve_SD  <- NaN
    TotalReserve_CoV <- NaN
    
  } else {
    # fails if any included Claims values are negative - need to trap this
    op <- as.vector(row(Triangle))
    #Row <- op[!is.na(Claims)]
    Row <- op
    Row <- ordered(as.factor(Row))
    dp <- as.vector(col(Triangle))
    #Col <- dp[!is.na(Claims)]
    Col <- dp
    Col <- ordered(as.factor(Col))
    Weights <- rep(1, nc*nc)
    Weights[is.na(Claims)] <- 0
    Weights <- Weights * Indicators_vector
    # use quasi family so some negatives are allowed
    # (in theory - R still doesn't seem to like it)
    etaStart <- log((abs(Claims)+1)) # for etastart
    options(contrasts=c("contr.treatment","contr.treatment"))
    fitcl <- glm(Claims ~ Row + Col, family = quasi(link="log", variance="mu"), 
                 weights = Weights, etastart = etaStart, control=glm.control(epsilon=0.00001))
    # R automatically removes observations where response value is NA
    fv <- predict(fitcl, type="response") # fitted values
    coefs <- summary(fitcl)$coefficients[,1]
    
    # obtain scaled covariance matrix of the parameters
    if (Scale=="Constant") {
      Sigma <- vcov(fitcl)
    } else {
      resids <- ODP_Residuals(Triangle, Scale="NonConstant", Mask=Mask)
      glmscale <- resids$sqrtScale^2 # dispersion parameters
      disp <- rep(1,length(Claims))
      disp <- glmscale[Col]
      disp[disp==0] <- 0.00000001
      disp <- disp[!is.na(Claims)]
      Dmat <- model.matrix(fitcl) # Design matrix
      Wmat <- diag(fv/disp) # Weight matrix in GLM fitting process
      Sigma <- solve(t(Dmat) %*% (Wmat %*% Dmat)) # Scaled covariance matrix of parameters
    }
    # standard error of parameters
    parameter_se <- sqrt(diag(Sigma))
    
    ## Calculate forecasts and prediction errors
    # Create future design matrix first
    op_fut <- op[is.na(Triangle)] # note: counts from bottom left first
    dp_fut <- dp[is.na(Triangle)] # note: counts from bottom left first
    y <- rep(1,length(Claims))
    Fmat_tmp <- model.matrix(y ~ as.factor(dp) + as.factor(op)) # design matrix for all observations, including missing
    Fmat <- Fmat_tmp[is.na(Triangle),] # design matrix for future observations only. Note: counts from top right first
    
    # calculate forecast values
    fv_fut <- exp(Fmat %*% coefs) # fitted values. Note: counts from top right first
    diag_fv_fut <- diag(as.vector(fv_fut))
    
    # associate dispersion parameters with each fitted value
    disp_fut <- if (Scale=="Constant") {
      rep(summary(fitcl)$dispersion, length(fv_fut))
    } else {
      glmscale[op_fut] # note: uses op_fut not dp_fut due to change in ordering in design matrix
    }
    diag_disp_fut <- diag(disp_fut)
    
    # calculate covariance matrix of linear predictors and fitted values
    cov1 <- Fmat %*% Sigma %*% t(Fmat) # covariance matrix of linear predictors 
    cov2 <- diag_fv_fut %*% cov1 %*% diag_fv_fut # covariance matrix of fitted values (for estimation error)
    cov3 <- cov2 + diag_disp_fut * diag_fv_fut # adding process error along diagonal component
    
    # origin period prediction variances are then just the appropriate sum of elements
    # take square root for prediction errors (RMSEPs)
    ODP_Res_SD <- sqrt(Row_covar_sum(cov3, length(Triangle)))
    
    ODP_Res <- Row_covar_sum(diag_fv_fut, length(Triangle)) # row reserves - just use Row_covar_sum function for simplicity
    
    Latest <- diag(apply(Triangle, 2, rev))
    Reserves <- head(ODP_Res, -1)
    TotalReserves <- tail(ODP_Res, 1)
    Ultimates <- Latest + Reserves
    Reserves_SD <- head(ODP_Res_SD, -1)
    Reserves_CoV <- safe_divide(Reserves_SD, Reserves)
    TotalReserve_SD <- tail(ODP_Res_SD, 1)
    TotalReserve_CoV <- safe_divide(TotalReserve_SD, TotalReserves)
  }
  
  result <- list()
  result$coefs <- coefs
  result$parameter_se <- parameter_se
  result$Latest <- Latest
  result$Reserves <- Reserves
  result$TotalReserves <- TotalReserves
  result$Ultimates <- Ultimates
  result$Reserves_SD <- Reserves_SD
  result$Reserves_CoV <- Reserves_CoV
  result$TotalReserve_SD <- TotalReserve_SD
  result$TotalReserve_CoV <- TotalReserve_CoV
  
  return(result)
  
}

ODP_Residuals <- function(tridata, Scale="NonConstant", Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)) {
  # Function to calculate residuals and scale parameters for the ODP chain ladder model
  # Various residual definitions are calculated, which are useful when bootstrapping
  # Scale="Constant" for constant scale parameter or "NonConstant" for varying scale parameter by development period
  n <- ncol(tridata)
  
  n_jj <- rep(NA,n)
  Indicators <- matrix(1,n,n)
  fitted_cumulative <- matrix(NA,n,n)
  fitted_incremental <- matrix(NA,n,n)
  unscaled_resids <- matrix(NA,n,n)
  adj_unscaled_resids <- matrix(NA,n,n)
  scaled_resids <- matrix(NA,n,n)
  adj_scaled_resids <- matrix(NA,n,n)
  zeroavg_adj_scaled_resids <- matrix(NA,n,n)
  scale_tmp <- matrix(NA,n,n)
  sqrtScale <- rep(NA,n)
  
  LRs <- LR_Tri(tridata)
  FittedFactors <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  CL_Result <- Tri_forecast(tridata, FittedFactors)
  Observed_Incrementals <- Incrementals(tridata)
  
  for (ii in 1:(n-1)) {
    fitted_cumulative[ii,n-ii+1] <- tridata[ii,n-ii+1]
    for (jj in (n-ii):1){
      fitted_cumulative[ii,jj] <- fitted_cumulative[ii,jj+1]/FittedFactors[jj]
    }
  }
  fitted_cumulative[n,1] <- tridata[n,1]
  
  fitted_incremental[,1] <- fitted_cumulative[,1]
  for (jj in 2:n) {
    fitted_incremental[,jj] <- fitted_cumulative[,jj] - fitted_cumulative[,jj-1]
  }
  
  # set Mask=0 if LinkRatio=0 for counting n_j
  Mask_tmp <- Mask
  Mask_tmp[LRs$LRs==0] <- 0
  
  # construct residual indicators from link ratio Mask
  # By convention, if ratios between development periods 1-2 are excluded, then
  # both ODP residuals at development periods 1 and 2 are excluded, otherwise
  # just the residual associated with the position of the numerator of the link ratio is excluded
  for (ii in 1:(n-1)){
    for (jj in 1:(n-ii+1)){
      if (jj<=2) {
        Indicators[ii,jj] <- Mask_tmp[ii,1]
      }
      else {
        Indicators[ii,jj] <- Mask_tmp[ii,jj-1]
      }
    }
  }

  for (jj in 1:n){
    n_jj[jj] <- sum(Indicators[1:(n-jj+1),jj])
  }

  n_obs <- sum(n_jj)
  n_params <- 2*n-1
  bias <- sqrt(n_obs/(n_obs-n_params))
  
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      unscaled_resids[ii,jj] <- Indicators[ii,jj]*(Observed_Incrementals[ii,jj] - fitted_incremental[ii,jj])/sqrt(abs(fitted_incremental[ii,jj]))
      adj_unscaled_resids[ii,jj] <- bias * unscaled_resids[ii,jj]
#      scale_tmp[ii,jj] <- (adj_unscaled_resids[ii,jj])^2
    }
  }
  
  # exclude zero residuals
  unscaled_resids[unscaled_resids==0] <- NA
  adj_unscaled_resids[is.na(unscaled_resids)] <- NA
  
  if (Scale=="Constant") {
    sqrtScale <- rep(sqrt(sum(adj_unscaled_resids^2, na.rm=TRUE)/n_obs),n)
  } else {
    sqrtScale_tmp <- apply(adj_unscaled_resids^2,2,sum, na.rm=TRUE)
    for (ii in 1:(n-1)){
      if(n_jj[ii]<=1) {
        if (ii==1) {
          sqrtScale[ii] <- 0
        } else {
          sqrtScale[ii] <- sqrtScale[ii-1]
        }
      } else {
        sqrtScale[ii] <- sqrt(sqrtScale_tmp[ii]/n_jj[ii])
      }
    }
    sqrtScale[n] <- min(sqrtScale[n-1], sqrtScale[n-2])
  }
  
  # set sqrtScale to zero if cumulative dev factors are 1
  rev_cumprod <- rev(cumprod(rev(FittedFactors)))
  for (i in 1:n) {
    if (i == 1) {
      if (rev_cumprod[1] == 1) {
        sqrtScale[1] <- 0
      }
    } else {
      if (rev_cumprod[i - 1] == 1) {
        sqrtScale[i] <- 0
      }
    }
  }
  
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      scaled_resids[ii,jj] <- safe_divide(unscaled_resids[ii,jj], sqrtScale[jj], NA)
      adj_scaled_resids[ii,jj] <- safe_divide(adj_unscaled_resids[ii,jj], sqrtScale[jj], NA)
    }
  }
  # exclude zero residual from calculation of average
  avg_resid <- mean(adj_scaled_resids,na.rm=TRUE)
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      zeroavg_adj_scaled_resids[ii,jj] <- adj_scaled_resids[ii,jj]-avg_resid
    }
  }
  # exclude zero residuals
  zeroavg_adj_scaled_resids[abs(adj_scaled_resids) < 1e-10] <- NA
  
  result <- list()
  result$sqrtScale <- sqrtScale
  result$unscaledresids <- unscaled_resids
  result$adjunscaledresids <- adj_unscaled_resids
  result$scaledresids <- scaled_resids
  result$adjscaledresids <- adj_scaled_resids
  result$zeroavgscaledresids <- zeroavg_adj_scaled_resids
  result$avgresid <- avg_resid
  result$FittedCumulative <- fitted_cumulative
  result$FittedIncremental <- fitted_incremental
  return(result)
}

ODP_pseudo_data <- function(Resids, sqrtScale, Fitted_Incremental ) {
  # Function to calculate pseudo incrementals when bootstrapping the ODP model
  nc <- ncol(Resids)
  pseudo_incrementals <- matrix(NA,nc,nc)
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      pseudo_incrementals[ii,jj] <- Resids[ii,jj] * sqrtScale[jj] * sqrt(abs(Fitted_Incremental[ii,jj])) + Fitted_Incremental[ii,jj]
    }
  }
  return(pseudo_incrementals)
}

ODP_pseudo_data_Gamma <- function(sqrtScale, Fitted_Incremental) {
  # Function to calculate pseudo incrementals when bootstrapping the ODP model
  # using parametric bootstrapping and Gamma distribution
  nc <- ncol(Fitted_Incremental)
  pseudo_incrementals <- matrix(NA,nc,nc)
  tol <- 1e-12
  
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      Mean <- Fitted_Incremental[ii,jj]
      SD <- sqrtScale[jj] * sqrt(abs(Mean))
      if (Mean > tol) {
        if (SD < tol) {
          pseudo_incrementals[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          pseudo_incrementals[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        pseudo_incrementals[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  return(pseudo_incrementals)
}

ODP_pseudo_data_Lognormal <- function(sqrtScale, Fitted_Incremental) {
  # Function to calculate pseudo incrementals when bootstrapping the ODP model
  # using parametric bootstrapping and Gamma distribution
  nc <- ncol(Fitted_Incremental)
  pseudo_incrementals <- matrix(NA,nc,nc)
  tol <- 1e-12
  
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      Mean <- Fitted_Incremental[ii,jj]
      SD <- sqrtScale[jj] * sqrt(abs(Mean))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        pseudo_incrementals[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      }
      else {
        pseudo_incrementals[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  return(pseudo_incrementals)
}

ODP_Bstrap_Forecast_NP <- function(tridata, factors, resids, Pseudo_Cumulatives, sqrtScale) {
  # Simulates Incrementals using a non-parametric approach. Values returned could be negative.
  # Values could be censored at a small positive number, but this could result in a bias.
  #
  # (Exclude bias adjustment from residuals when forecasting? Need to think about this!)
  nc <- ncol(tridata)
  incrementals <- matrix(NA, nc, nc)
  
  cumulatives <- tridata
  
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Pseudo_Cumulatives[ii,jj] <- Pseudo_Cumulatives[ii,jj-1]*factors[jj-1]
      incrementals[ii,jj] <- Pseudo_Cumulatives[ii,jj] - Pseudo_Cumulatives[ii,jj-1]
      incrementals[ii,jj] <- resids[ii,jj] * sqrtScale[jj] * sqrt(abs(incrementals[ii,jj])) + incrementals[ii,jj]
      cumulatives[ii,jj] <- cumulatives[ii,jj-1] + incrementals[ii,jj]
    }
  }
  
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
  
}

ODP_Bstrap_Forecast_Gamma <- function(tridata, factors, Pseudo_Cumulatives, sqrtScale) {
  # Simulates Incrementals from a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but values returned could be negative.
  # Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  incrementals <- matrix(NA, nc, nc)
  tol <- 1e-12
  cumulatives <- tridata
  
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Pseudo_Cumulatives[ii,jj] <- Pseudo_Cumulatives[ii,jj-1]*factors[jj-1]
      incrementals[ii,jj] <- Pseudo_Cumulatives[ii,jj] - Pseudo_Cumulatives[ii,jj-1]
      Mean <- incrementals[ii,jj]
      SD <- sqrtScale[jj] * sqrt(abs(Mean))
      if (Mean > tol) {
        if (SD < tol) {
          incrementals[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          incrementals[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        incrementals[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
      cumulatives[ii,jj] <- cumulatives[ii,jj-1] + incrementals[ii,jj]
    }
  }
  
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
  
}

ODP_Bstrap_Forecast_Lognormal <- function(tridata, factors, Pseudo_Cumulatives, sqrtScale) {
  # Simulates Incrementals from a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but values returned could be negative.
  # Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  incrementals <- matrix(NA, nc, nc)
  tol <- 1e-12
  cumulatives <- tridata
  
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Pseudo_Cumulatives[ii,jj] <- Pseudo_Cumulatives[ii,jj-1]*factors[jj-1]
      incrementals[ii,jj] <- Pseudo_Cumulatives[ii,jj] - Pseudo_Cumulatives[ii,jj-1]
      Mean <- incrementals[ii,jj]
      SD <- sqrtScale[jj] * sqrt(abs(Mean))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        incrementals[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      }
      else {
        incrementals[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
      cumulatives[ii,jj] <- cumulatives[ii,jj-1] + incrementals[ii,jj]
    }
  }
  
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
  
}

Main_ODP_Bstrap <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1), iterations, seed = sample(1e6,1), Scale="NonConstant",
                            BootstrapDist = "NP", ForecastDist="Gamma", UserSqrtScale=NULL) {
  # Main function for bootstrapping the ODP chain ladder model. Choice of constant or non-constant scale paremeters,
  # and non-parametric or Gamma forecast distributions
  # and non-parametric or Gamma distribution for pseudo-data when bootstrapping
  # Note that with the ODP model, incremental claims are forecast
  n <- length(tridata)
  nc <- n-1
  Pseudo_LRs <- matrix(NA, iterations, nc)
  Reserves <- matrix(NA, iterations, n)
  Ultimates <- matrix(NA, iterations, n)
  TotalReserve <- vector("numeric",iterations)
  Cumulatives <- array(0, c(iterations,n,n))
  
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  ODP_Resids <- ODP_Residuals(tridata, Scale=Scale, Mask)
  
  ResidsVector <- as.vector(ODP_Resids$zeroavgscaledresids)
  # no need to take out NAs
  ResidsVector <- sort(ResidsVector,TRUE)
  
  if (is.null(UserSqrtScale)) {
    ForecastSqrtScale <- ODP_Resids$sqrtScale
  } else {
    ForecastSqrtScale <- UserSqrtScale
  }
  
  # set seed
  set.seed(seed)
  # Do bootstrapping and forecasting
  for (ii in 1:iterations) {
    # Not efficient to have if..else statement inside bootstrap loop. Fix later.
    if (BootstrapDist=="Gamma") {
      Pseudo_Incrementals <- ODP_pseudo_data_Gamma(ODP_Resids$sqrtScale, ODP_Resids$FittedIncremental)
    } else if (BootstrapDist=="Lognormal") {
      Pseudo_Incrementals <- ODP_pseudo_data_Lognormal(ODP_Resids$sqrtScale, ODP_Resids$FittedIncremental)
    }
    else {
      Resampled_Resids <- Resample_Resids(ResidsVector, n)
      Pseudo_Incrementals <- ODP_pseudo_data(Resampled_Resids, ODP_Resids$sqrtScale, ODP_Resids$FittedIncremental )
    }
    
    Pseudo_Cumulatives <- Cumulatives(Pseudo_Incrementals)
    LRs <- LR_Tri(Pseudo_Cumulatives)
    Pseudo_LRs[ii,] <- CL_factors(LRs$LRs, LRs$Weight, Mask)

        # Not efficient to have if..else statement inside bootstrap loop. Fix later.
    if (ForecastDist=="Lognormal") {
      Forecast <- ODP_Bstrap_Forecast_Lognormal(tridata, Pseudo_LRs[ii,], Pseudo_Cumulatives, ForecastSqrtScale)
    } else if (ForecastDist=="Gamma") {
      Forecast <- ODP_Bstrap_Forecast_Gamma(tridata, Pseudo_LRs[ii,], Pseudo_Cumulatives, ForecastSqrtScale)
    } else {
      Resampled_Resids <- Resample_Resids(ResidsVector, nc+1)
      Forecast <- ODP_Bstrap_Forecast_NP(tridata, Pseudo_LRs[ii,],Resampled_Resids, Pseudo_Cumulatives, ForecastSqrtScale)      
    }
    Reserves[ii,] <- Forecast$Reserves
    Ultimates[ii,] <- Forecast$Ultimates
    TotalReserve[ii] <- Forecast$TotalReserve
    Cumulatives[ii,,] <- as.matrix(Forecast$Cumulatives)
  }
  
  Avg_Reserve <- apply(Reserves,2,mean)
  SD_Reserve <- apply(Reserves,2,sd)
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  Avg_TotalReserve <- mean(TotalReserve)
  SD_TotalReserve <- sd(TotalReserve)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  result <- list()
  result$CL_facs <- CL_facs
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  result$Resids <- ODP_Resids
  result$PseudoLRs <- Pseudo_LRs
  result$Cumulatives <- Cumulatives
  result$Reserves <- Reserves
  result$Ultimates <- Ultimates
  result$TotalReserve <- TotalReserve
  result$Avg_Reserve <- Avg_Reserve
  result$SD_Reserve <- SD_Reserve
  result$Avg_TotalReserve <- Avg_TotalReserve
  result$SD_TotalReserve <- SD_TotalReserve
  result$CoV_Reserve <- CoV_Reserve
  result$CoV_TotalReserve <- CoV_TotalReserve
  result$iterations <- iterations
  result$finished <- TRUE
  
  return(result)
}

## Mack's model

# Analytic result - Use ChainLadder package for Mack's SE using Mack's formula, or fit weighted Gaussian regression model with non-constant sigmas
# and calculate reserve SEs using function for recursive models

# Mack's model using weighted Gaussian regression model with non-constant sigmas

Mack_ChainLadder <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)) {
  
  nc <- length(tridata)-1
  n_j <- rep(NA, nc)
  
  # Compute n_j
  for (j in 1:nc) {
    n_j[j] <- sum(Mask[1:(nc - j + 1), j], na.rm = TRUE)
  }
  
  if (any(n_j < 1)) {
    message("Invalid data for Mack's model applied analytically: at least one ratio must be included at each development period")
    message("Proceed with bootstrapping instead")
    
    coefs          <- rep(NaN, nc)
    parameter_se   <- rep(NaN, nc)
    LinkRatios     <- rep(NaN, nc)
    LinkRatios_SE   <- rep(NaN, nc)
    #sigma          <- rep(NaN, nc)
    Latest         <- rep(NaN, nc+1)
    #TotalLatest    <- NaN
    Reserves       <- rep(NaN, nc+1)
    TotalReserves  <- NaN
    Ultimates      <- rep(NaN, nc+1)
    #TotalUltimates <- NaN
    Reserves_SD    <- rep(NaN, nc+1)
    Reserves_CoV   <- rep(NaN, nc+1)
    TotalReserve_SD  <- NaN
    TotalReserve_CoV <- NaN
    
  } else {
    LRs <- LR_Tri(tridata)
    CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
    #CL_Result <- Tri_forecast(tridata, CL_facs)
    Mack_Resids <- Mack_Residuals(tridata, Mask)
    
    # allow for exclusions
    Weights_tmp = LRs$Weight*Mask
    
    Row_Triangle <- row(LRs$LRs)
    Column_Triangle <- col(LRs$LRs)
    
    Row <- as.vector(Row_Triangle[!is.na(LRs$LRs)])
    Column <- as.vector(Column_Triangle[!is.na(LRs$LRs)])
    Ratio <- as.vector(LRs$LRs[!is.na(LRs$LRs)])
    Weights <- as.vector(Weights_tmp[!is.na(LRs$LRs)])
    Design_tmp <- as.matrix(Column)
    Design_tmp <- as.factor(Design_tmp)
    Design <- model.matrix(~0+Design_tmp)
    
    y_var <- Ratio
    np <- nrow(LRs$LRs) # number of parameters
    #coef_start <- rep(0, np)
    coef_start <- log(CL_facs)
    parameter_se <- rep(0, np)
    
    # Fit GLM with Gaussian error structure and log link
    
    test <- 1
    num_iter <- 0
    error <- "NA"
    disp <- Mack_Resids$sqrtScale[Column]^2
    disp[disp==0] <- 0.00000001
    
    while (test > 0.00000001) {
      coef <- coef_start
      eta <- Design %*% coef
      lambda <- exp(eta)
      deta_dmu <- 1/lambda
      z <- eta + (y_var-lambda)*deta_dmu
      V <- 1 # variance function for Gaussian GLM
      W_vec <- Weights/(deta_dmu^2*V*disp) # since sigmas are not constant, bring into Weight vector here so parameter_se are correct later
      W_mat <- diag(as.vector(W_vec))
      inv_XWX <- solve(t(Design) %*% (W_mat %*% Design))
      XWz <- t(Design) %*% (W_mat %*% z)
      coef <- inv_XWX %*% XWz
      # create difference between start and end params and check convergence, then repeat:
      test <- sqrt(sum((coef-coef_start)^2))
      coef_start <- coef
      num_iter <- num_iter+1
      if (num_iter > 20) {
        error <- "Failure to Converge"
        test <- 0 #force a stop to prevent infinite loop
      }
    }
    
    coef_labels <- paste("gamma", 2:(length(coef)+1), sep=" ")
    dimnames(coef) <- list(coef_labels)
    coefs <- coef[,1]
    
    # Calculate standard error of parameters allowing for over-dispersion
    parameter_se <- (sqrt(diag(inv_XWX))) # simply sqrt of variance of linear predictor in this case
    
    LinkRatios <- exp(coef) # should be standard chain ladder LRs in this case
    LinkRatios_SE <- LinkRatios*parameter_se
    LinkRatios <- LinkRatios[,1]
    LinkRatios_SE <- LinkRatios_SE[,1]
    
    CL_Result <- Tri_forecast(tridata, LinkRatios)
    Latest <- CL_Result$Latest
    Reserves <- CL_Result$Reserves
    TotalReserves <- CL_Result$TotalReserve
    Ultimates <- CL_Result$Ultimates
    
    # Calculate SD of Reserves using recursive formulae from Appendix of England & Verrall (2006)
    Recursive_SD_Result <- Recursive_SD(tridata, LinkRatios_SE, Mack_Resids$sqrtScale, model="Mack", Mask=Mask)
    Reserves_SD <- Recursive_SD_Result$CL_Reserves_SD
    Reserves_CoV <- Recursive_SD_Result$CL_Reserves_CoV
    TotalReserve_SD <- Recursive_SD_Result$CL_TotalReserve_SD
    TotalReserve_CoV <- Recursive_SD_Result$CL_TotalReserve_CoV
  }
  
  result <- list()
  result$coefs <- coefs
  result$parameter_se <- parameter_se
  result$LinkRatios <- LinkRatios
  result$LinkRatios_SE <- LinkRatios_SE
  result$Latest <- Latest
  result$Reserves <- Reserves
  result$TotalReserves <- TotalReserves
  result$Ultimates <- Ultimates
  result$Reserves_SD <- Reserves_SD
  result$Reserves_CoV <- Reserves_CoV
  result$TotalReserve_SD <- TotalReserve_SD
  result$TotalReserve_CoV <- TotalReserve_CoV
  #result$GLMerror <- error
  
  return(result)
  
}

Mack_Residuals <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)){
  # Function to calculate residuals and sigma parameters for Mack's model
  # Various residual definitions are calculated, which are useful when bootstrapping
  n <- ncol(tridata)-1
  
  unscaled_resids <- matrix(NA,n,n)
  adj_unscaled_resids <- matrix(NA,n,n)
  scaled_resids <- matrix(NA,n,n)
  adj_scaled_resids <- matrix(NA,n,n)
  zeroavg_adj_scaled_resids <- matrix(NA,n,n)
  scale_tmp <- matrix(NA,n,n)
  
  n_jj <- rep(NA,n)
  bias <- rep(NA,n)
  sigma <- rep(NA,n)
  
  LRs <- LR_Tri(tridata)
  LRTriangle <- LRs$LRs
  LRWeights <- LRs$Weight
  FittedFactors <- CL_factors(LRTriangle, LRWeights, Mask)
  
  # set Mask=0 if LinkRatio=0 for counting n_j
  Mask_tmp <- Mask
  Mask_tmp[LRTriangle==0] <- 0
  
  # bias correction for each development period
  #(Needs modifying if curves are fitted to factors to allow for number of parameters)
  for (jj in 1:(n-1)){
    n_jj[jj] <- sum(Mask_tmp[1:(n-jj+1),jj])
    if (n_jj[jj]<=1){
      bias[jj] <- 0
    } else {
      bias[jj] <- sqrt(n_jj[jj]/(n_jj[jj]-1))
    }
  }
  bias[n] <- 1
  
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      unscaled_resids[ii,jj] <- sqrt(abs(Mask_tmp[ii,jj]*LRWeights[ii,jj]))*(LRTriangle[ii,jj]-FittedFactors[jj])
      adj_unscaled_resids[ii,jj] <- bias[jj]*unscaled_resids[ii,jj]
      scale_tmp[ii,jj] <- (adj_unscaled_resids[ii,jj])^2
    }
  }
  
  for (ii in 1:(n-1)){
    if(n_jj[ii]<=1) {
      if (ii==1) {
        sigma[ii] <- 0
      } 
      else {
        sigma[ii] <- sigma[ii-1]
      }
    }
    else {
      sigma[ii] <- sqrt(sum(scale_tmp[1:(n-ii+1),ii],na.rm=TRUE)/n_jj[ii])
    }
  }
  sigma[n] <- min(sigma[n-1], sigma[n-2])
  
  # set sigma to zero if cumulative dev factors are 1
  rev_cumprod <- rev(cumprod(rev(FittedFactors)))
  sigma[rev_cumprod == 1] <- 0
  
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      scaled_resids[ii,jj] <- safe_divide(unscaled_resids[ii,jj], sigma[jj], NA)
      adj_scaled_resids[ii,jj] <- safe_divide(adj_unscaled_resids[ii,jj], sigma[jj], NA)
    }
  }
  avg_resid <- sum(adj_scaled_resids,na.rm=TRUE)/(sum(n_jj,na.rm=TRUE)-1)
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      zeroavg_adj_scaled_resids[ii,jj] <- adj_scaled_resids[ii,jj]-avg_resid
    }
  }
  # exclude zero residuals
  unscaled_resids[unscaled_resids==0] <- NA
  adj_unscaled_resids[is.na(unscaled_resids)] <- NA
  scaled_resids[is.na(unscaled_resids)] <- NA
  adj_scaled_resids[is.na(unscaled_resids)] <- NA
  zeroavg_adj_scaled_resids[is.na(unscaled_resids)] <- NA
  
  
  result <- list()
  result$sigma <- sigma
  result$sqrtScale <- sigma
  result$unscaledresids <- unscaled_resids
  result$adjunscaledresids <- adj_unscaled_resids
  result$scaledresids <- scaled_resids
  result$adjscaledresids <- adj_scaled_resids
  result$zeroavgscaledresids <- zeroavg_adj_scaled_resids
  result$avgresid <- avg_resid
  return(result)
}

Mack_pseudo_data <- function(resids, sigma, avgRatio, weight) {
  # Function to calculate pseudo loss ratios when bootstrapping Mack's model
  nc <- ncol(resids)-1
  pseudo_ratios <- matrix(NA,nc,nc)
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      pseudo_ratios[ii,jj] <- resids[ii,jj] * (sigma[jj]/sqrt(abs(weight[ii,jj])))+avgRatio[jj]
    }
  }
  return(pseudo_ratios)
}

Mack_pseudo_data_Gamma <- function(sigma, avgRatio, weight) {
  # Function to calculate pseudo loss ratios when bootstrapping Mack's model
  # using parametric bootstrapping and Gamma distribution
  nc <- ncol(weight)
  pseudo_ratios <- matrix(NA,nc,nc)
  tol <- 1e-12
  
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      Mean <- avgRatio[jj]
      SD <- safe_divide(sigma[jj], sqrt(abs(weight[ii,jj])))
      if (Mean > tol) {
        if (SD < tol) {
          pseudo_ratios[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          pseudo_ratios[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        pseudo_ratios[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  return(pseudo_ratios)
}

Mack_pseudo_data_Lognormal <- function(sigma, avgRatio, weight) {
  # Function to calculate pseudo loss ratios when bootstrapping Mack's model
  # using parametric bootstrapping and Lognormal distribution
  nc <- ncol(weight)
  pseudo_ratios <- matrix(NA,nc,nc)
  tol <- 1e-12
  
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      Mean <- avgRatio[jj]
      SD <- safe_divide(sigma[jj], sqrt(abs(weight[ii,jj])))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        pseudo_ratios[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      }
      else {
        pseudo_ratios[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  return(pseudo_ratios)
}


Mack_Bstrap_forecast_NP <- function(tridata, factors, resids, sigma) {
  # Simulates Cumulatives using a non-parametric approach. Beware, values returned could be negative,
  # which doesn't make sense. Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  cumulatives <- tridata
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      cumulatives[ii,jj] <- cumulatives[ii,jj-1]*factors[jj-1]+resids[ii,jj]*sigma[jj-1]*sqrt(abs(cumulatives[ii,jj-1]))
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
}

Mack_Forecast_Gamma <-function(tridata, factors, sigma) {
  # Simulates Cumulatives from a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but beware, values returned could be negative,
  # which doesn't make sense. Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  cumulatives <- tridata
  tol <- 1e-12
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Mean <- cumulatives[ii,jj-1]*factors[jj-1]
      SD <- sigma[jj-1] * sqrt(abs(cumulatives[ii,jj-1]))
      if (Mean > tol) {
        if (SD < tol) {
          cumulatives[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          cumulatives[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        cumulatives[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
}

Mack_Forecast_Lognormal <-function(tridata, factors, sigma) {
  # Simulates Cumulatives from a Lognormal distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but beware, values returned could be negative,
  # which doesn't make sense. Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  cumulatives <- tridata
  tol <- 1e-12
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Mean <- cumulatives[ii,jj-1]*factors[jj-1]
      SD <- sigma[jj-1] * sqrt(abs(cumulatives[ii,jj-1]))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        cumulatives[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      }
      else {
        cumulatives[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
}

Main_Mack_Bstrap <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1), 
                             iterations=1000, seed = sample(1e6,1),
                             BootstrapDist = "NP", ForecastDist="Gamma", UserSigma=NULL) {
  # Main function for bootstrapping Macks model. Choice of non-parametric or Gamma forecast distributions
  # and non-parametric or Gamma distribution for pseudo-data when bootstrapping
  # Note that with Mack's model, cumulative claims are forecast
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  Mack_Resids <- Mack_Residuals(tridata, Mask)
  
  nc <- length(CL_facs)
  Pseudo_LRs <- matrix(NA, iterations, nc)
  Reserves <- matrix(NA, iterations, nc+1)
  Ultimates <- matrix(NA, iterations, nc+1)
  TotalReserve <- vector("numeric",iterations)
  Cumulatives <- array(0, c(iterations,(nc+1),(nc+1)))
  
  ResidsVector <- as.vector(Mack_Resids$zeroavgscaledresids)
  # no need to take out NAs
  ResidsVector <- sort(ResidsVector,TRUE)
  
  if (is.null(UserSigma)) {
    ForecastSigma <- Mack_Resids$sigma
  } else {
    ForecastSigma <- UserSigma
  }
  
  # set seed
  set.seed(seed)
  # Do bootstrapping and forecasting
  for (ii in 1:iterations) {
    # Not efficient to have if..else statement inside bootstrap loop. Fix later.
    if (BootstrapDist=="Gamma") {
      pseudo_ratios <- Mack_pseudo_data_Gamma(Mack_Resids$sigma, CL_facs, LRs$Weight)
    } else if (BootstrapDist=="Lognormal") {
      pseudo_ratios <- Mack_pseudo_data_Lognormal(Mack_Resids$sigma, CL_facs, LRs$Weight)
    } else {
      Resampled_Resids <- Resample_Resids(ResidsVector, nc+1)
      pseudo_ratios <- Mack_pseudo_data(Resampled_Resids, Mack_Resids$sigma, CL_facs, LRs$Weight)
    }

    Pseudo_LRs[ii,] <- CL_factors(pseudo_ratios, LRs$Weight, Mask)    
        
    # Not efficient to have if..else statement inside bootstrap loop. Fix later.
    if (ForecastDist=="Lognormal") {
      Forecast <- Mack_Forecast_Lognormal(tridata, Pseudo_LRs[ii,], ForecastSigma)
    } else if (ForecastDist=="Gamma") {
      Forecast <- Mack_Forecast_Gamma(tridata, Pseudo_LRs[ii,], ForecastSigma)      
    } else {
      Resampled_Resids <- Resample_Resids(ResidsVector, nc+1)
      Forecast <- Mack_Bstrap_forecast_NP(tridata, Pseudo_LRs[ii,],Resampled_Resids, ForecastSigma)
    }
    
    Reserves[ii,] <- Forecast$Reserves
    Ultimates[ii,] <- Forecast$Ultimates
    TotalReserve[ii] <- Forecast$TotalReserve
    Cumulatives[ii,,] <- as.matrix(Forecast$Cumulatives)
  }
  
  Avg_Reserve <- apply(Reserves,2,mean)
  SD_Reserve <- apply(Reserves,2,sd)
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  Avg_TotalReserve <- mean(TotalReserve)
  SD_TotalReserve <- sd(TotalReserve)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  result <- list()
  result$CL_facs <- CL_facs
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  result$Resids <- Mack_Resids
  result$PseudoLRs <- Pseudo_LRs
  result$Cumulatives <- Cumulatives
  result$Reserves <- Reserves
  result$Ultimates <- Ultimates
  result$TotalReserve <- TotalReserve
  result$Avg_Reserve <- Avg_Reserve
  result$SD_Reserve <- SD_Reserve
  result$Avg_TotalReserve <- Avg_TotalReserve
  result$SD_TotalReserve <- SD_TotalReserve
  result$CoV_Reserve <- CoV_Reserve
  result$CoV_TotalReserve <- CoV_TotalReserve
  result$iterations <- iterations
  result$finished <- TRUE
  
  return(result)
}


## Negative Binomial Analytic

NegBin_ChainLadder <- function(tridata, Scale="NonConstant", Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1)) {
  
  nc <- length(tridata)-1
  
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  #CL_Result <- Tri_forecast(tridata, CL_facs)
  
  if (any(CL_facs <= 1)) {
    message("Invalid data for NegBin model applied analytically: Dev factor less than or equal to one detected")
    message("Proceed with bootstrapping instead")
    
    coefs          <- rep(NaN, nc)
    parameter_se   <- rep(NaN, nc)
    LinkRatios     <- rep(NaN, nc)
    LinkRatios_SE   <- rep(NaN, nc)
    #sigma          <- rep(NaN, nc)
    Latest         <- rep(NaN, nc+1)
    #TotalLatest    <- NaN
    Reserves       <- rep(NaN, nc+1)
    TotalReserves  <- NaN
    Ultimates      <- rep(NaN, nc+1)
    #TotalUltimates <- NaN
    Reserves_SD    <- rep(NaN, nc+1)
    Reserves_CoV   <- rep(NaN, nc+1)
    TotalReserve_SD  <- NaN
    TotalReserve_CoV <- NaN
    
  } else {
    
    NegBin_Resids <- NegBin_Residuals(tridata, Scale=Scale, Mask=Mask)
    
    # allow for exclusions
    Weights_tmp = LRs$Weight*Mask
    
    Row_Triangle <- row(LRs$LRs)
    Column_Triangle <- col(LRs$LRs)
    
    Row <- as.vector(Row_Triangle[!is.na(LRs$LRs)])
    Column <- as.vector(Column_Triangle[!is.na(LRs$LRs)])
    Ratio <- as.vector(LRs$LRs[!is.na(LRs$LRs)])
    Weights <- as.vector(Weights_tmp[!is.na(LRs$LRs)])
    Design_tmp <- as.matrix(Column)
    Design_tmp <- as.factor(Design_tmp)
    Design <- model.matrix(~0+Design_tmp)
    
    y_var <- Ratio
    np <- nrow(LRs$LRs) # number of parameters
    #coef_start <- rep(0, np)
    coef_start <- log(log(CL_facs))
    parameter_se <- rep(0, np)
    
    # Fit GLM with Negative Binomial Error structure and log-log link
    # A log-log link is non-standard, so fit GLM by hand
    
    test <- 1
    num_iter <- 0
    error <- "NA"
    
    while (test > 0.00000001) {
      coef <- coef_start
      eta <- Design %*% coef
      lambda <- exp(exp(eta))
      deta_dmu <- 1/(lambda*log(lambda))
      z <- eta + (y_var-lambda)*deta_dmu
      V <- lambda*(lambda-1)
      # W_vec <- Weights/(deta_dmu^2*V*disp) # theoretically would use this with joint modelling over multiple iterations
      W_vec <- Weights/(deta_dmu^2*V)
      W_mat <- diag(as.vector(W_vec))
      inv_XWX <- solve(t(Design) %*% (W_mat %*% Design))
      XWz <- t(Design) %*% (W_mat %*% z)
      coef <- inv_XWX %*% XWz
      # create difference between start and end params and check convergence, then repeat:
      test <- sqrt(sum((coef-coef_start)^2))
      coef_start <- coef
      num_iter <- num_iter+1
      if (num_iter > 20) {
        error <- "Failure to Converge"
        test <- 0 #force a stop to prevent infinite loop
      }
    }
    
    # Calculate standard error of parameters allowing for over-dispersion
    disp <- NegBin_Resids$sqrtScale[Column]^2
    disp[disp==0] <- 0.00000001
    W_vec <- Weights/(deta_dmu^2*V*disp)
    W_mat <- diag(as.vector(W_vec))
    inv_XWX <- solve(t(Design) %*% (W_mat %*% Design))
    
    coef_labels <- paste("gamma", 2:(length(coef)+1), sep=" ")
    dimnames(coef) <- list(coef_labels)
    coefs <- coef[,1]
    parameter_se <- (sqrt(diag(inv_XWX))) # simply sqrt of variance of linear predictor in this case

    LinkRatios <- exp(exp(coef)) # should be standard chain ladder LRs in this case
    LinkRatios_SE <- LinkRatios*log(LinkRatios)*parameter_se
    LinkRatios <- LinkRatios[,1]
    LinkRatios_SE <- LinkRatios_SE[,1]
    
    CL_Result <- Tri_forecast(tridata, LinkRatios)
    Latest <- CL_Result$Latest
    Reserves <- CL_Result$Reserves
    TotalReserves <- CL_Result$TotalReserve
    Ultimates <- CL_Result$Ultimates
    
    # Calculate SD of Reserves using recursive formulae from Appendix of England & Verrall (2006)
    Recursive_SD_Result <- Recursive_SD(tridata, LinkRatios_SE, NegBin_Resids$sqrtScale, model="NegBin", Mask=Mask)
    Reserves_SD <- Recursive_SD_Result$CL_Reserves_SD
    Reserves_CoV <- Recursive_SD_Result$CL_Reserves_CoV
    TotalReserve_SD <- Recursive_SD_Result$CL_TotalReserve_SD
    TotalReserve_CoV <- Recursive_SD_Result$CL_TotalReserve_CoV
  }
  
  result <- list()
  result$coefs <- coefs
  result$parameter_se <- parameter_se
  result$LinkRatios <- LinkRatios
  result$LinkRatios_SE <- LinkRatios_SE
  result$Latest <- Latest
  result$Reserves <- Reserves
  result$TotalReserves <- TotalReserves
  result$Ultimates <- Ultimates
  result$Reserves_SD <- Reserves_SD
  result$Reserves_CoV <- Reserves_CoV
  result$TotalReserve_SD <- TotalReserve_SD
  result$TotalReserve_CoV <- TotalReserve_CoV
  #result$GLMerror <- error
  
  return(result)
  
}

## Negative Binomial Bootstrap

NegBin_Residuals <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1), Scale="NonConstant"){
  # Function to calculate residuals and rootphi parameters for Negative Binomial model
  # Various residual definitions are calculated, which are useful when bootstrapping
  n <- ncol(tridata)-1
  
  unscaled_resids <- matrix(NA,n,n)
  adj_unscaled_resids <- matrix(NA,n,n)
  scaled_resids <- matrix(NA,n,n)
  adj_scaled_resids <- matrix(NA,n,n)
  zeroavg_adj_scaled_resids <- matrix(NA,n,n)
  
  n_jj <- rep(NA,n)
  sqrtScale <- rep(NA,n)
  
  LRs <- LR_Tri(tridata)
  LRTriangle <- LRs$LRs
  LRWeights <- LRs$Weight
  FittedFactors <- CL_factors(LRTriangle, LRWeights, Mask)
  
  # set Mask=0 if LinkRatio=0 for counting n_j
  Mask_tmp <- Mask
  Mask_tmp[LRTriangle==0] <- 0
  
  for (jj in 1:n){
    n_jj[jj] <- sum(Mask_tmp[1:(n-jj+1),jj])
  }
  
  n_obs <- sum(n_jj)
  n_params <- n
  bias <- sqrt(n_obs/(n_obs-n_params))
  
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      unscaled_resids[ii,jj] <- safe_divide(sqrt(abs(Mask_tmp[ii,jj]*LRWeights[ii,jj]))*
        (LRTriangle[ii,jj]-FittedFactors[jj]), sqrt(abs(FittedFactors[jj]*(FittedFactors[jj]-1))), NA)
      adj_unscaled_resids[ii,jj] <- bias*unscaled_resids[ii,jj] 
    }
  }
  
  # exclude zero residuals
  unscaled_resids[unscaled_resids==0] <- NA
  adj_unscaled_resids[is.na(unscaled_resids)] <- NA
  
  if (Scale=="Constant") {
    sqrtScale <- rep(sqrt(sum(adj_unscaled_resids^2, na.rm=TRUE)/n_obs),n)
  } else {
    sqrtScale_tmp <- apply(adj_unscaled_resids^2,2,sum, na.rm=TRUE)
    for (ii in 1:(n-1)){
      if(n_jj[ii]<=1) {
        if (ii==1) {
          sqrtScale[ii] <- 0
        } else {
          sqrtScale[ii] <- sqrtScale[ii-1]
        }
      } else {
        sqrtScale[ii] <- sqrt(sqrtScale_tmp[ii]/n_jj[ii])
      }
    }
    sqrtScale[n] <- min(sqrtScale[n-1], sqrtScale[n-2])
  }
  
  # set sigma to zero if cumulative dev factors are 1
  rev_cumprod <- rev(cumprod(rev(FittedFactors)))
  sqrtScale[rev_cumprod == 1] <- 0
  
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      scaled_resids[ii,jj] <- safe_divide(unscaled_resids[ii,jj], sqrtScale[jj], NA)
      adj_scaled_resids[ii,jj] <- safe_divide(adj_unscaled_resids[ii,jj], sqrtScale[jj], NA)
    }
  }
  avg_resid <- sum(adj_scaled_resids,na.rm=TRUE)/(sum(n_jj,na.rm=TRUE)-1)
  for (ii in 1:n){
    for (jj in 1:(n-ii+1)){
      zeroavg_adj_scaled_resids[ii,jj] <- adj_scaled_resids[ii,jj]-avg_resid
    }
  }
  
  result <- list()
  result$sigma <- sqrtScale
  result$sqrtScale <- sqrtScale
  result$unscaledresids <- unscaled_resids
  result$adjunscaledresids <- adj_unscaled_resids
  result$scaledresids <- scaled_resids
  result$adjscaledresids <- adj_scaled_resids
  result$zeroavgscaledresids <- zeroavg_adj_scaled_resids
  result$avgresid <- avg_resid
  return(result)
}

NegBin_pseudo_data <- function(resids, sigma, avgRatio, weight) {
  # Function to calculate pseudo loss ratios when bootstrapping the Negative Binomial model
  nc <- ncol(resids)-1
  pseudo_ratios <- matrix(NA,nc,nc)
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      pseudo_ratios[ii,jj] <- resids[ii,jj] * (sigma[jj]*sqrt(abs(avgRatio[jj]*(avgRatio[jj]-1)))/sqrt(abs(weight[ii,jj])))+avgRatio[jj]
    }
  }
  return(pseudo_ratios)
}

NegBin_pseudo_data_Gamma <- function(sigma, avgRatio, weight) {
  # Function to calculate pseudo loss ratios when bootstrapping Mack's model
  # using parametric bootstrapping and Gamma distribution
  nc <- ncol(weight)
  pseudo_ratios <- matrix(NA,nc,nc)
  tol <- 1e-12
  
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      Mean <- avgRatio[jj]
      SD <- safe_divide(sigma[jj] * sqrt(abs(avgRatio[jj]*(avgRatio[jj]-1))), sqrt(abs(weight[ii,jj])))
      if (Mean > tol) {
        if (SD < tol) {
          pseudo_ratios[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          pseudo_ratios[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        pseudo_ratios[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  return(pseudo_ratios)
}

NegBin_pseudo_data_Lognormal <- function(sigma, avgRatio, weight) {
  # Function to calculate pseudo loss ratios when bootstrapping Mack's model
  # using parametric bootstrapping and Lognormal distribution
  nc <- ncol(weight)
  pseudo_ratios <- matrix(NA,nc,nc)
  tol <- 1e-12
  
  for (ii in 1:nc) {
    for (jj in 1:(nc-ii+1)) {
      Mean <- avgRatio[jj]
      SD <- safe_divide(sigma[jj] * sqrt(abs(avgRatio[jj]*(avgRatio[jj]-1))), sqrt(abs(weight[ii,jj])))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        pseudo_ratios[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      }
      else {
        pseudo_ratios[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  return(pseudo_ratios)
}

NegBin_Forecast_Gamma <-function(tridata, factors, sigma) {
  # Simulates Cumulatives from a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but beware, values returned could be negative,
  # which doesn't make sense. Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  cumulatives <- tridata
  tol <- 1e-12
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Mean <- cumulatives[ii,jj-1]*factors[jj-1]
      # force SD to be positive when factors are less than 1
      SD <- sigma[jj-1] * sqrt(abs(factors[jj-1]*(factors[jj-1]-1))*abs(cumulatives[ii,jj-1]))
      if (Mean > tol) {
        if (SD < tol) {
          cumulatives[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          cumulatives[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        cumulatives[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
}

NegBin_Forecast_Lognormal <-function(tridata, factors, sigma) {
  # Simulates Cumulatives from a Lognormal distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but beware, values returned could be negative,
  # which doesn't make sense. Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  cumulatives <- tridata
  tol <- 1e-12
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      Mean <- cumulatives[ii,jj-1]*factors[jj-1]
      # force SD to be positive when factors are less than 1
      SD <- sigma[jj-1] * sqrt(abs(factors[jj-1]*(factors[jj-1]-1))*abs(cumulatives[ii,jj-1]))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        cumulatives[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      }
      else {
        cumulatives[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
}

NegBin_Bstrap_forecast_NP <- function(tridata, factors, resids, sigma) {
  # Simulates Cumulatives using a non-parametric approach. Beware, values returned could be negative,
  # which doesn't make sense. Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  cumulatives <- tridata
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      cumulatives[ii,jj] <- cumulatives[ii,jj-1]*factors[jj-1]+resids[ii,jj]*sigma[jj-1]*sqrt(abs(cumulatives[ii,jj-1]*factors[jj-1]*(factors[jj-1]-1)))
    }
  }
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
}

Main_NegBin_Bstrap <- function(tridata, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1), 
                               Scale="NonConstant", iterations, seed = sample(1e6,1),
                               BootstrapDist = "NP", ForecastDist="Gamma", UserSqrtScale=NULL) {
  # Main function for bootstrapping Negative Binomial model with Gamma forecast distribution
  # Note that with the Negative Binomial model, cumulative claims are forecast
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  NegBin_Resids <- NegBin_Residuals(tridata, Mask, Scale=Scale)
  
  nc <- length(CL_facs)
  Pseudo_LRs <- matrix(NA, iterations, nc)
  Reserves <- matrix(NA, iterations, nc+1)
  Ultimates <- matrix(NA, iterations, nc+1)
  TotalReserve <- vector("numeric",iterations)
  Cumulatives <- array(0, c(iterations,(nc+1),(nc+1)))
  
  ResidsVector <- as.vector(NegBin_Resids$zeroavgscaledresids)
  # no need to take out NAs
  ResidsVector <- sort(ResidsVector,TRUE)
  
  if (is.null(UserSqrtScale)) {
    ForecastSigma <- NegBin_Resids$sigma
  } else {
    ForecastSigma <- UserSqrtScale
  }
  
  # set seed
  set.seed(seed)
  # Do bootstrapping and forecasting
  for (ii in 1:iterations) {
    # Not efficient to have if..else statement inside bootstrap loop. Fix later.
    if (BootstrapDist=="Gamma") {
      pseudo_ratios <- NegBin_pseudo_data_Gamma(NegBin_Resids$sigma, CL_facs, LRs$Weight)
    } else if (BootstrapDist=="Lognormal") {
      pseudo_ratios <- NegBin_pseudo_data_Lognormal(NegBin_Resids$sigma, CL_facs, LRs$Weight)
    } else {
      Resampled_Resids <- Resample_Resids(ResidsVector, nc+1)
      pseudo_ratios <- NegBin_pseudo_data(Resampled_Resids, NegBin_Resids$sigma, CL_facs, LRs$Weight)
    }

    Pseudo_LRs[ii,] <- CL_factors(pseudo_ratios, LRs$Weight, Mask)
    
    # Not efficient to have if..else statement inside bootstrap loop. Fix later.
    if (ForecastDist=="Lognormal") {
      Forecast <- NegBin_Forecast_Lognormal(tridata, Pseudo_LRs[ii,], ForecastSigma)
    } else if (ForecastDist=="Gamma") {
      Forecast <- NegBin_Forecast_Gamma(tridata, Pseudo_LRs[ii,], ForecastSigma)      
    } else {
      Resampled_Resids <- Resample_Resids(ResidsVector, nc+1)
      Forecast <- NegBin_Bstrap_forecast_NP(tridata, Pseudo_LRs[ii,],Resampled_Resids, ForecastSigma)
    }
    
    Reserves[ii,] <- Forecast$Reserves
    Ultimates[ii,] <- Forecast$Ultimates
    TotalReserve[ii] <- Forecast$TotalReserve
    Cumulatives[ii,,] <- as.matrix(Forecast$Cumulatives)
  }
  
  Avg_Reserve <- apply(Reserves,2,mean)
  SD_Reserve <- apply(Reserves,2,sd)
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  Avg_TotalReserve <- mean(TotalReserve)
  SD_TotalReserve <- sd(TotalReserve)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  result <- list()
  result$CL_facs <- CL_facs
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  result$Resids <- NegBin_Resids
  result$PseudoLRs <- Pseudo_LRs
  result$Cumulatives <- Cumulatives
  result$Reserves <- Reserves
  result$Ultimates <- Ultimates
  result$TotalReserve <- TotalReserve
  result$Avg_Reserve <- Avg_Reserve
  result$SD_Reserve <- SD_Reserve
  result$Avg_TotalReserve <- Avg_TotalReserve
  result$SD_TotalReserve <- SD_TotalReserve
  result$CoV_Reserve <- CoV_Reserve
  result$CoV_TotalReserve <- CoV_TotalReserve
  result$iterations <- iterations
  result$finished <- TRUE
  
  return(result)
}

# Risk measures
VAR <- function(x,p){
  # Function to calculate the value of X at a given percentile p. Note that the value returned is 
  # such that there is a probability p of values being less than X. For example, if there are 100 simulations
  # and VAR(x, 0.5) is required, then the value of the 51st ordered simulation is returned, since 50% of values
  # are below this value. Think about whether it would be better to take -InvPercentile(-x,1-p)
  x=sort(x)
  return(x[floor(length(x)*p+1)])
}
TVAR <- function(x,p){
  # Function to calculate Tail Value at Risk. If there are 100 simulations
  # and VAR(x, 0.5) is required, then the value of the average of ordered simulations 51 to 100
  # Think about whether the inpit is a profit or loss distribution (ie think about the sign)
  x=sort(x)
  return(mean(x[x>=VAR(x,p)]))
}
PHT <- function(x,rho){
  # Function to calculate Wang's proportional hazards transform given parameter rho.
  # This method uses a weighted average of simulations.
  rho <- 1/rho
  x=sort(x)
  n <- length(x)
  tSx <- vector("numeric", n)
  wt <- vector("numeric", n)
  
  tSx[1] <- (1-1/n)^rho
  wt[1] <- 1-tSx[1]
  for (ii in 2:n) {
    tSx[ii] <- (1-ii/n)^rho
    wt[ii] <- tSx[ii-1] - tSx[ii]
  }
  return(sum(wt * x))  
}

TVAR_with_wt <- function(x,p){
  # Function to calculate Tail Value at Risk. If there are 100 simulations
  # and VAR(x, 0.5) is required, then the value of the average of ordered simulations 51 to 100
  # Think about whether the inpit is a profit or loss distribution (ie think about the sign)
  x=sort(x)
  VAR_x <- VAR(x,p)
  wt <- rep(0, length(x))
  wt[x>=VAR_x] <- 1
  wt <- wt/sum(wt)
  result <- list()
  result$weight <- wt
  result$value <- mean(x[x>=VAR_x])
  return(result)
}

PHT_with_wt <- function(x,rho){
  # Function to calculate Wang's proportional hazards transform given parameter rho.
  # This method uses a weighted average of simulations.
  rho <- 1/rho
  x=sort(x)
  n <- length(x)
  tSx <- vector("numeric", n)
  wt <- vector("numeric", n)
  
  tSx[1] <- (1-1/n)^rho
  wt[1] <- 1-tSx[1]
  for (ii in 2:n) {
    tSx[ii] <- (1-ii/n)^rho
    wt[ii] <- tSx[ii-1] - tSx[ii]
  }
  result <- list()
  result$weight <- wt
  result$value <- sum(wt * x)
  return(result)  
}

# Cost-of-capital risk margin
Capital_Profile <- function(Basis) {
  # Function to calculate a "Capital profile" given a basis
  # Used to provide an input to the CoC_RM function
  fy <- length(Basis)
  Profile <- Basis/Basis[1]
  return(Profile)
}
CoC_RM <- function(Opening_Capital, Capital_Profile, CoC_Rate, Discount_Rate, Offset=0) {
  # Function to calculate a cost-of-capital risk margin as a sum of discounted costs-of-capital
  # Assumes a constant discount rate
  fy <- length(Capital_Profile)
  Capital <- vector("numeric", fy)
  CoC <- vector("numeric", fy)
  Disc_CoC <- vector("numeric", fy)
  
  t <- c(1:fy)
  Capital <- Opening_Capital*Capital_Profile
  CoC <- Capital * CoC_Rate
  Disc_CoC <- CoC/(1+Discount_Rate)^(t-1+min(Offset,1))
  RM <- sum(Disc_CoC)
  
  result <- list()
  result$Capital <- Capital
  result$CoC <- CoC
  result$Disc_CoC <- Disc_CoC
  result$RM <- RM
  return(result)
}

Future_Reserves <- function(Cumulatives) {
  # Function to calculate remaining undiscounted reserves at each future point in time
  # Assumes a standard triangle with no tail
  nc <- ncol(Cumulatives)
  Reserves <- matrix(NA, nc-1,nc)
  for (fy in 1:(nc-1)) {
    for (ii in 1:nc) {
      Reserves[fy,ii] <- Cumulatives[ii,nc] - Cumulatives[ii,min(nc-ii+fy,nc)]
    }
  }
  
  Future_Reserves <- apply(Reserves, 1, sum)
  return(Future_Reserves)
  
}

Disc_Reserves <- function(Incrementals, Disc_Rate, offset=0, periods=0) {
  # Function to calculate discounted reserves. If periods > 0, then remaining payments beyond the input periods ahead
  # are discounted back to that calendar period
  nc <- ncol(Incrementals)
  Disc_Incrementals <- matrix(NA, nc,nc)
  offset <- min(offset,1)
  if (periods<nc) {
    for (ii in min(2+periods,nc):nc) {
      t <- min(nc-ii+2+periods,nc)
      for (jj in t:nc) {
        Disc_Incrementals[ii,jj] <- Incrementals[ii,jj]/(1+Disc_Rate)^(jj-t+offset)
      }
    }
  }
  Disc_Reserves <- apply(Disc_Incrementals,1,sum, na.rm=TRUE)
  Disc_Total_Reserves <- sum(Disc_Reserves)
  
  result <- list()
  result$Disc_Reserves <- Disc_Reserves
  result$Disc_Total_Reserves <- Disc_Total_Reserves
  
  return(result)  
  
  
}

Disc_Future_Reserves <- function(Incrementals, Disc_rate, offset=0) {
  # Function to calculate remaining discounted reserves at each future point in time. Note that at 
  # each future time period, amounts are discounted back to that calendar period only.
  # Assumes a standard triangle with no tail
  nc <- ncol(Incrementals)
  Future_Reserves <- vector("numeric", nc-1)
  for (period in 1:(nc-1)) {
    Future_Reserves[period] <- Disc_Reserves(Incrementals, Disc_rate, offset, period-1)$Disc_Total_Reserves
  }
  
  return(Future_Reserves)
}

Bstrap_Disc_Reserves <- function(Cumulatives, Disc_rate, offset=0) {
  # Function to calculate discounted reserves and summary statistics following bootstrapping
  iterations <- dim(Cumulatives)[1]
  nc <- dim(Cumulatives)[2]
  Disc_Reserves_by_yr <- matrix(NA, iterations, nc)
  Disc_Total_Reserves <- vector("numeric", iterations)
  
  for (ii in 1:iterations) {
    Incremental_claims <- Incrementals(Cumulatives[ii,,])
    Disc_Reserves_tmp <- Disc_Reserves(Incremental_claims, Disc_rate, offset, 0)
    Disc_Reserves_by_yr[ii,] <- Disc_Reserves_tmp$Disc_Reserves
    Disc_Total_Reserves[ii] <- Disc_Reserves_tmp$Disc_Total_Reserves
  }
  
  Avg_Disc_Res <- apply(Disc_Reserves_by_yr,2,mean)
  SD_Disc_Res <- apply(Disc_Reserves_by_yr,2,sd)
  CoV_Disc_Res <- safe_divide(SD_Disc_Res, abs(Avg_Disc_Res))
  Avg_Disc_TotalRes <- mean(Disc_Total_Reserves)
  SD_Disc_TotalRes <- sd(Disc_Total_Reserves)
  Cov_Disc_TotalRes <- safe_divide(SD_Disc_TotalRes, abs(Avg_Disc_TotalRes))
  
  result <- list()
  result$Avg_Disc_Res <- Avg_Disc_Res
  result$SD_Disc_Res <- SD_Disc_Res
  result$CoV_Disc_Res <- CoV_Disc_Res
  result$Avg_Disc_TotalRes <- Avg_Disc_TotalRes
  result$SD_Disc_TotalRes <- SD_Disc_TotalRes
  result$Cov_Disc_TotalRes <- Cov_Disc_TotalRes
  result$Reserves <- Disc_Reserves_by_yr
  result$TotalReserve <- Disc_Total_Reserves
  result$finished = TRUE
  
  return(result)
}

Bstrap_Disc_Future_Reserves <- function(Cumulatives, Disc_rate, offset=0) {
  # Function to calculate remaining discounted reserves at each future point in time for each simulation following bootstrapping.
  # Note that at each future time period, amounts are discounted back to that calendar period only.
  # Assumes a standard triangle with no tail
  iterations <- dim(Cumulatives)[1]
  nc <- dim(Cumulatives)[2]
  Future_Reserves <- matrix(NA, iterations, nc-1)
  
  for (ii in 1:iterations) {
    Incremental_claims <- Incrementals(Cumulatives[ii,,])
    for (period in 1:(nc-1)) {
      Future_Reserves[ii,period] <- Disc_Reserves(Incremental_claims, Disc_rate, offset, period-1)$Disc_Total_Reserves
    }
  }
  
  Avg_Disc_Fut_Res <- apply(Future_Reserves,2,mean)
  SD_Disc_Fut_Res <- apply(Future_Reserves,2,sd)
  #  VAR_Disc_Fut_Res_995 <- apply(Future_Reserves,2,VAR, p=0.995)
  
  result <- list()
  result$FutureReserves <- Future_Reserves
  result$Avg_Disc_Fut_Res <- Avg_Disc_Fut_Res
  result$SD_Disc_Fut_Res <- SD_Disc_Fut_Res
  #  result$VAR_Disc_Fut_Res_995 <- VAR_Disc_Fut_Res_995 - Avg_Disc_Fut_Res
  result$finished <- TRUE
  
  return(result)
}


CDR_Full_Picture <- function(tridata, Forecast_Cumulatives, VAR_p=0.995, Mask=matrix(1,nrow(tridata)-1, ncol(tridata)-1),
                              Future_Periods=ncol(tridata)-1) {
  # Function to calculate the "Actuary-in-the-Box" Claims Development Result (CDR) for a sequence of one-year forecasts
  # and calculate the Avg and SD of the CDR.
  # This is useful for the one-year view of Solvency II, and links together the one-year and the lifetime views of risk.
  #
  # The function calculates the CDR for each future year by default. Use the Future_Periods argument to restrict the range
  # if required (eg Future_Periods=1 for 1-year view only)
  
  iterations <- dim(Forecast_Cumulatives)[1]
  
  # Get standard chain ladder result
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight, Mask)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  
  # For 1 yr view of risk and beyond
  nc <- length(CL_facs)
  Max_Future_Periods <- min(Future_Periods, nc)
  CDR <- array(0, c(Max_Future_Periods, iterations, (nc+1)))
  cumsum_CDR <- array(0, c(Max_Future_Periods, iterations, (nc+1)))
  TotalCDR <- matrix(NA, Max_Future_Periods, iterations)
  cumsum_TotalCDR <- matrix(NA, Max_Future_Periods, iterations)
  
  # Do one-yr view first
  for (ii in 1:iterations) {
    One_Yr_LRs <- LinkRatiofunction(Forecast_Cumulatives[ii,,], Mask, 1)
    One_Yr_Forecasts <- Tri_forecast(Forecast_Cumulatives[ii,,], One_Yr_LRs, 1)
    CDR[1,ii,] <- CL_Result$Ultimates - One_Yr_Forecasts$Ultimates
    TotalCDR[1,ii] <- sum(CDR[1,ii,1:(nc+1)])
    cumsum_CDR[1,ii,] <- CDR[1,ii,]
    cumsum_TotalCDR[1,ii] <- TotalCDR[1,ii]
    # Then do remaining years
    if (Max_Future_Periods > 1) {
      for (fy in 2:Max_Future_Periods){
        Last_Yr_Ultimates <- One_Yr_Forecasts$Ultimates
        One_Yr_LRs <- LinkRatiofunction(Forecast_Cumulatives[ii,,], Mask, fy)
        One_Yr_Forecasts <- Tri_forecast(Forecast_Cumulatives[ii,,], One_Yr_LRs, fy)
        CDR[fy,ii,] <- Last_Yr_Ultimates - One_Yr_Forecasts$Ultimates
        TotalCDR[fy,ii] <- sum(CDR[fy,ii,1:(nc+1)])
        cumsum_CDR[fy,ii,] <- cumsum_CDR[fy-1,ii,] + CDR[fy,ii,]
        cumsum_TotalCDR[fy,ii] <- cumsum_TotalCDR[fy-1,ii] + TotalCDR[fy,ii]
      }
    }
  }
  
  
  Avg_CDR <- matrix(NA, Max_Future_Periods, nc+1)
  SD_CDR <- matrix(NA, Max_Future_Periods, nc+1)
  CDR_VAR <- matrix(NA, Max_Future_Periods, nc+1)
  Avg_cumsum_CDR <- matrix(NA, Max_Future_Periods, nc+1)
  SD_cumsum_CDR <- matrix(NA, Max_Future_Periods, nc+1)
  cumsum_CDR_VAR <- matrix(NA, Max_Future_Periods, nc+1)
  
  for (fy in 1:Max_Future_Periods) {
    Avg_CDR[fy,] <- apply(CDR[fy,,],2,mean)
    SD_CDR[fy,] <- apply(CDR[fy,,],2,sd)
    CDR_VAR[fy,] <- -apply(CDR[fy,,],2,VAR, p=(1-VAR_p)) + Avg_CDR[fy,]
    Avg_cumsum_CDR[fy,] <- apply(cumsum_CDR[fy,,],2,mean)
    SD_cumsum_CDR[fy,] <- apply(cumsum_CDR[fy,,],2,sd)
    cumsum_CDR_VAR[fy,] <- -apply(cumsum_CDR[fy,,],2,VAR, p=(1-VAR_p)) + Avg_cumsum_CDR[fy,]
  }
  
  Avg_TotalCDR <- apply(TotalCDR,1,mean)
  SD_TotalCDR <- apply(TotalCDR,1,sd)
  TotalCDR_VAR <- -apply(TotalCDR,1,VAR, p=(1-VAR_p)) + Avg_TotalCDR
  Avg_cumsum_TotalCDR <- apply(cumsum_TotalCDR,1,mean)
  SD_cumsum_TotalCDR <- apply(cumsum_TotalCDR,1,sd)
  cumsum_TotalCDR_VAR <- -apply(cumsum_TotalCDR,1,VAR, p=(1-VAR_p)) + Avg_cumsum_TotalCDR
  
  result <- list()
  
  result$LRs <- LRs
  result$CL_facs <- CL_facs
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  
  result$CDR <- CDR
  result$TotalCDR <- TotalCDR
  result$Avg_CDR <- Avg_CDR
  result$SD_CDR <- SD_CDR
  result$CDR_VAR <- CDR_VAR
  result$Avg_TotalCDR <- Avg_TotalCDR
  result$SD_TotalCDR <- SD_TotalCDR
  result$TotalCDR_VAR <- TotalCDR_VAR
  result$cumsum_CDR <- cumsum_CDR
  result$cumsum_TotalCDR <- cumsum_TotalCDR
  result$SD_cumsum_CDR <- SD_cumsum_CDR
  result$cumsum_CDR_VAR <- cumsum_CDR_VAR
  result$Avg_cumsum_CDR <- Avg_cumsum_CDR
  result$Avg_cumsum_TotalCDR <- Avg_cumsum_TotalCDR
  result$SD_cumsum_TotalCDR <- SD_cumsum_TotalCDR
  result$cumsum_TotalCDR_VAR <- cumsum_TotalCDR_VAR
  result$finished=TRUE
  
  return(result)
}

CDR_CumSum <- function(CDR) {
  # Function to calculate reverse sum of CDRs from simulated results
  iterations <- dim(CDR)[2]
  fy <- dim(CDR)[1]
  CumSum_CDR <- matrix(NA, fy, iterations)
  for (ii in 1:iterations) {
    CumSum_CDR[,ii] <- cumsum(CDR[,ii])
  }
  
  SD_CumSum_CDR <- apply(CumSum_CDR,1,sd)
  #  VAR_CumSum_CDR_995 <- -apply(CumSum_CDR,1,VAR, p=0.005)
  
  result <- list()
  result$CumSum_CDR <- CumSum_CDR
  result$SD_CumSum_CDR <- SD_CumSum_CDR
  
  return(result)
}


CDR_Rev_Sum <- function(CDR) {
  # Function to calculate reverse sum of CDRs from simulated results
  iterations <- dim(CDR)[2]
  fy <- dim(CDR)[1]
  RevSum_CDR <- matrix(NA, fy, iterations)
  for (ii in 1:iterations) {
    RevSum_CDR[,ii] <- rev(cumsum(rev(CDR[,ii])))
  }
  
  Avg_RevSum_CDR <- apply(RevSum_CDR,1,mean)
  SD_RevSum_CDR <- apply(RevSum_CDR,1,sd)
  #  VAR_RevSum_CDR_995 <- -apply(RevSum_CDR,1,VAR, p=0.005)
  
  result <- list()
  result$RevSum_CDR <- RevSum_CDR
  result$Avg_RevSum_CDR <- Avg_RevSum_CDR
  result$SD_RevSum_CDR <- SD_RevSum_CDR
  result$finished <- TRUE
  
  return(result)
}

## MCMC models

## ODP MCMC

ODP_MCMC_Forecast_Gamma <- function(tridata, coefs, rootphi) {
  # Simulates Incrementals from a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but values returned could be negative.
  # Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  Row_Triangle <- row(tridata)
  Column_Triangle <- col(tridata)
  incrementals <- matrix(NA, nc, nc)
  tol <- 1e-12
  cumulatives <- tridata
  
  intercept <- coefs[1]
  row_coefs <- c(0, coefs[2:nc])
  col_coefs <- c(0, coefs[(nc+1):(2*nc-1)])
  
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      incrementals[ii,jj] <- exp(intercept+row_coefs[Row_Triangle[ii,jj]]+col_coefs[Column_Triangle[ii,jj]])
      Mean <- incrementals[ii,jj]
      SD <- rootphi[jj] * sqrt(abs(Mean))
      if (Mean > tol) {
        if (SD < tol) {
          incrementals[ii,jj] <- Mean
        }
        else {
          scale <- (SD^2)/Mean
          shape <- Mean/scale
          incrementals[ii,jj] <- rgamma(1, shape=shape, scale=scale)
        }
      }
      else {
        incrementals[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
      cumulatives[ii,jj] <- cumulatives[ii,jj-1] + incrementals[ii,jj]
    }
  }
  
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
  
}

ODP_MCMC_Forecast_Lognormal <- function(tridata, coefs, rootphi) {
  # Simulates Incrementals from a Gamma distribution. A Normal distribution is used if mean is negative,
  # which retains first and second moment properties, but values returned could be negative.
  # Values could be censored at a small positive number, but this could result in a bias.
  nc <- ncol(tridata)
  Row_Triangle <- row(tridata)
  Column_Triangle <- col(tridata)
  incrementals <- matrix(NA, nc, nc)
  tol <- 1e-12
  cumulatives <- tridata
  
  intercept <- coefs[1]
  row_coefs <- c(0, coefs[2:nc])
  col_coefs <- c(0, coefs[(nc+1):(2*nc-1)])
  
  for (ii in 2:nc) {
    for (jj in (nc-ii+2):nc) {
      incrementals[ii,jj] <- exp(intercept+row_coefs[Row_Triangle[ii,jj]]+col_coefs[Column_Triangle[ii,jj]])
      Mean <- incrementals[ii,jj]
      SD <- rootphi[jj] * sqrt(abs(Mean))
      if (Mean > tol) {
        sigma_normal <- sqrt(log(1+(SD/Mean)^2))
        mean_normal <- log(Mean) - 0.5*sigma_normal^2
        incrementals[ii,jj] <- rlnorm(1, meanlog=mean_normal, sdlog=sigma_normal)
      } else {
        incrementals[ii,jj] <- rnorm(1, mean=Mean, sd=SD)
      }
      cumulatives[ii,jj] <- cumulatives[ii,jj-1] + incrementals[ii,jj]
    }
  }
  
  Ultimates <- vector("numeric",nc)
  Reserves <- vector("numeric",nc)
  for (ii in 1:nc) {
    Ultimates[ii] <- cumulatives[ii,nc]
    Reserves[ii] <- Ultimates[ii] - cumulatives[ii,nc-ii+1]
  }
  TotalReserve <- sum(Reserves)
  
  result <- list()
  result$Cumulatives <- cumulatives
  result$Ultimates <- Ultimates
  result$Reserves <- Reserves
  result$TotalReserve <-TotalReserve
  return(result)
  
}

Main_ODP_MCMC <- function(tridata, Scale="NonConstant", iter, chains, seed = sample(1e6,1),
                          ForecastDist="Gamma", UserSqrtScale=NULL) {
  # Main function for MCMC version of ODP chain ladder model. Choice of constant or non-constant scale paremeters.
  # Gamma forecast distribution
  # Note that with the ODP model, incremental claims are forecast
  
  n <- length(tridata)
  nc <- n-1
  
  CL_facs <- LinkRatiofunction(tridata)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  ODP_Resids <- ODP_Residuals(tridata, Scale=Scale)
  
  Row_Triangle <- row(tridata)
  Column_Triangle <- col(tridata)
  
  Row <- as.vector(Row_Triangle[!is.na(tridata)])
  Column <- as.vector(Column_Triangle[!is.na(tridata)])
  IncClaims_Tri <- Incrementals(tridata)
  IncClaims <- as.vector(IncClaims_Tri[!is.na(tridata)])
  
  Row_fac <- as.factor(as.matrix(Row))
  Col_fac <- as.factor(as.matrix(Column))
  Design <- model.matrix(~Row_fac+Col_fac)
  
  data_for_stan <- list()
  data_for_stan$N <- length(IncClaims)
  data_for_stan$C <- 2*n-1
  data_for_stan$cl <- IncClaims
  data_for_stan$X <- Design
  data_for_stan$rootphi <- ODP_Resids$sqrtScale[Column]
  
  # set seed
  set.seed(seed)
  options(mc.cores=chains)
  
  MCMC_model <- stan_model('ODP_model.stan')
  
  fit <- sampling(MCMC_model,data=data_for_stan,chains=chains,iter=iter,seed=seed)
  
  #print(fit)
  #params <- extract(fit)
  params <- rstan::extract(fit)
  iterations <- length(params$coefs[,1])
  coefs <- params$coefs
  
  # Stan returns coefs in a different sort order even when a seed is set, 
  # so force an ordering for repeatability when forecasting
  coefs <- coefs[order(coefs[, 1]),]
  
  Reserves <- matrix(NA, iterations, n)
  Ultimates <- matrix(NA, iterations, n)
  TotalReserve <- vector("numeric",iterations)
  Cumulatives <- array(0, c(iterations,n,n))
  
  if (is.null(UserSqrtScale)) {
    ForecastSigma <- ODP_Resids$sqrtScale
  } else {
    ForecastSigma <- UserSqrtScale
  }
  
  if (ForecastDist=="NP") {
    message("ForecastDist=='NP' is not valid for MCMC methods: using 'Gamma' option instead")
    ForecastDist <- "Gamma"
  }
  
  # Do forecasting
  for (ii in 1:iterations) {
    coefs_tmp <- coefs[ii,]
    
    if (ForecastDist=="Lognormal") {
      Forecast <- ODP_MCMC_Forecast_Lognormal(tridata, coefs_tmp, ForecastSigma)
    } else if (ForecastDist=="Gamma") {
      Forecast <- ODP_MCMC_Forecast_Gamma(tridata, coefs_tmp, ForecastSigma)      
    }
    
    Reserves[ii,] <- Forecast$Reserves
    Ultimates[ii,] <- Forecast$Ultimates
    TotalReserve[ii] <- Forecast$TotalReserve
    Cumulatives[ii,,] <- as.matrix(Forecast$Cumulatives)
  }
  
  Avg_Reserve <- apply(Reserves,2,mean)
  SD_Reserve <- apply(Reserves,2,sd)
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  Avg_TotalReserve <- mean(TotalReserve)
  SD_TotalReserve <- sd(TotalReserve)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  result <- list()
  result$CL_facs <- CL_facs
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  result$ODPResids <- ODP_Resids
  #  result$PseudoLRs <- Pseudo_LRs
  result$Cumulatives <- Cumulatives
  result$Reserves <- Reserves
  result$Ultimates <- Ultimates
  result$TotalReserve <- TotalReserve
  result$Avg_Reserve <- Avg_Reserve
  result$SD_Reserve <- SD_Reserve
  result$Avg_TotalReserve <- Avg_TotalReserve
  result$SD_TotalReserve <- SD_TotalReserve
  result$CoV_Reserve <- CoV_Reserve
  result$CoV_TotalReserve <- CoV_TotalReserve
  result$MCMCfit <- fit
  result$iterations <- iterations
  result$finished <- TRUE
  
  return(result)
}

## Mack's model MCMC

Main_Mack_MCMC <- function(tridata, iter, chains, seed = sample(1e6,1), ForecastDist="Gamma", UserSigma=NULL) {
  ## Just passing in simulated LRs for now
  # Main function for MCMC version of Mack's model with Gamma forecast distribution
  # Note that with Mack's model, cumulative claims are forecast
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  Mack_Resids <- Mack_Residuals(tridata)
  
  Row_Triangle <- row(LRs$LRs)
  Column_Triangle <- col(LRs$LRs)
  sigmas <- Mack_Resids$sigma
  
  Row <- as.vector(Row_Triangle[!is.na(LRs$LRs)])
  Column <- as.vector(Column_Triangle[!is.na(LRs$LRs)])
  Ratio <- as.vector(LRs$LRs[!is.na(LRs$LRs)])
  Weights <- as.vector(LRs$Weight[!is.na(LRs$LRs)])
  Design_tmp <- as.factor(as.matrix(Column))
  #Design_tmp <- as.factor(Design_tmp)
  Design <- model.matrix(~0+Design_tmp)
  
  data_for_stan <- list()
  data_for_stan$N <- length(Ratio)
  data_for_stan$C <- ncol(LRs$LRs)
  data_for_stan$f <- Ratio
  data_for_stan$w <- Weights
  data_for_stan$col <- Column
  data_for_stan$X <- Design
  data_for_stan$sigmas <- sigmas
  
  # set seed
  set.seed(seed)
  #parallel::detectCores()  
  options(mc.cores=chains)
  
  MCMC_model <- stan_model('Macks_model.stan')
  
  fit <- sampling(MCMC_model,data=data_for_stan,chains=chains,iter=iter,seed=seed)    
  
  #print(fit)
  #params <- extract(fit)
  params <- rstan::extract(fit)
  coefs <- params$coefs
  
  # Stan returns coefs in a different sort order even when a seed is set, 
  # so force an ordering for repeatability when forecasting
  coefs <- coefs[order(coefs[, 1]),]
  
  Pseudo_LRs <- exp(coefs)  #log link
  iterations <- length(Pseudo_LRs[,1])
  
  nc <- length(CL_facs)
  #  Pseudo_LRs <- matrix(NA, iterations, nc)
  Reserves <- matrix(NA, iterations, nc+1)
  Ultimates <- matrix(NA, iterations, nc+1)
  TotalReserve <- vector("numeric",iterations)
  Cumulatives <- array(0, c(iterations,(nc+1),(nc+1)))
  
  if (is.null(UserSigma)) {
    ForecastSigma <- sigmas
  } else {
    ForecastSigma <- UserSigma
  }
  
  if (ForecastDist=="NP") {
    message("ForecastDist=='NP' is not valid for MCMC methods: using 'Gamma' option instead")
    ForecastDist <- "Gamma"
  }
  
  # Do forecasting
  for (ii in 1:iterations) {
    if (ForecastDist=="Lognormal") {
      Forecast <- Mack_Forecast_Lognormal(tridata, Pseudo_LRs[ii,], ForecastSigma)
    } else if (ForecastDist=="Gamma") {
      Forecast <- Mack_Forecast_Gamma(tridata, Pseudo_LRs[ii,], ForecastSigma)
    }
    Reserves[ii,] <- Forecast$Reserves
    Ultimates[ii,] <- Forecast$Ultimates
    TotalReserve[ii] <- Forecast$TotalReserve
    Cumulatives[ii,,] <- as.matrix(Forecast$Cumulatives)
  }
  
  Avg_Reserve <- apply(Reserves,2,mean)
  SD_Reserve <- apply(Reserves,2,sd)
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  Avg_TotalReserve <- mean(TotalReserve)
  SD_TotalReserve <- sd(TotalReserve)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  result <- list()
  result$CL_facs <- CL_facs
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  result$PseudoLRs <- Pseudo_LRs
  result$Cumulatives <- Cumulatives
  result$Reserves <- Reserves
  result$Ultimates <- Ultimates
  result$TotalReserve <- TotalReserve
  result$Avg_Reserve <- Avg_Reserve
  result$SD_Reserve <- SD_Reserve
  result$Avg_TotalReserve <- Avg_TotalReserve
  result$SD_TotalReserve <- SD_TotalReserve
  result$CoV_Reserve <- CoV_Reserve
  result$CoV_TotalReserve <- CoV_TotalReserve
  result$MCMCfit <- fit
  result$iterations <- iterations
  result$finished <- TRUE
  
  return(result)
}

## Negative Binomial MCMC

Main_NegBin_MCMC <- function(tridata, Scale="NonConstant", iter, chains, seed = sample(1e6,1),
                             ForecastDist="Gamma", UserSqrtScale=NULL) {
  # Main function for MCMC version of Mack's model with Gamma forecast distribution
  # Note that with Mack's model, cumulative claims are forecast
  LRs <- LR_Tri(tridata)
  CL_facs <- CL_factors(LRs$LRs, LRs$Weight)
  CL_Result <- Tri_forecast(tridata, CL_facs)
  NegBin_Resids <- NegBin_Residuals(tridata, Scale=Scale)
  
  Row_Triangle <- row(LRs$LRs)
  Column_Triangle <- col(LRs$LRs)
  
  Row <- as.vector(Row_Triangle[!is.na(LRs$LRs)])
  Column <- as.vector(Column_Triangle[!is.na(LRs$LRs)])
  Ratio <- as.vector(LRs$LRs[!is.na(LRs$LRs)])
  Weights <- as.vector(LRs$Weight[!is.na(LRs$LRs)])
  Design_tmp <- as.factor(as.matrix(Column))
  #Design_tmp <- as.factor(Design_tmp)
  Design <- model.matrix(~0+Design_tmp)
  
  data_for_stan <- list()
  data_for_stan$N <- length(Ratio)
  data_for_stan$C <- ncol(LRs$LRs)
  data_for_stan$f <- Ratio
  data_for_stan$w <- Weights
  #  data_for_stan$col <- Column
  data_for_stan$X <- Design
  data_for_stan$rootphi <- NegBin_Resids$sqrtScale[Column]
  
  # set seed
  set.seed(seed)
  #parallel::detectCores()  
  options(mc.cores=chains)
  
  MCMC_model <- stan_model('NegBin_model.stan')
  
  fit <- sampling(MCMC_model,data=data_for_stan,chains=chains,iter=iter,seed=seed)    
  
  #print(fit)
  #params <- extract(fit)
  params <- rstan::extract(fit)
  coefs <- params$coefs
  
  # Stan returns coefs in a different sort order even when a seed is set, 
  # so force an ordering for repeatability when forecasting
  coefs <- coefs[order(coefs[, 1]),]
  
  Pseudo_LRs <- exp(exp(coefs))  #log-log link
  iterations <- length(Pseudo_LRs[,1])
  
  nc <- length(CL_facs)
  #  Pseudo_LRs <- matrix(NA, iterations, nc)
  Reserves <- matrix(NA, iterations, nc+1)
  Ultimates <- matrix(NA, iterations, nc+1)
  TotalReserve <- vector("numeric",iterations)
  Cumulatives <- array(0, c(iterations,(nc+1),(nc+1)))
  
  if (is.null(UserSqrtScale)) {
    ForecastSigma <- NegBin_Resids$sqrtScale
  } else {
    ForecastSigma <- UserSqrtScale
  }
  
  if (ForecastDist=="NP") {
    message("ForecastDist=='NP' is not valid for MCMC methods: using 'Gamma' option instead")
    ForecastDist <- "Gamma"
  }
  
  # Do forecasting
  for (ii in 1:iterations) {
    if (ForecastDist=="Lognormal") {
      Forecast <- NegBin_Forecast_Lognormal(tridata, Pseudo_LRs[ii,], ForecastSigma)
    } else if (ForecastDist=="Gamma") {
      Forecast <- NegBin_Forecast_Gamma(tridata, Pseudo_LRs[ii,], ForecastSigma)      
    }
    Reserves[ii,] <- Forecast$Reserves
    Ultimates[ii,] <- Forecast$Ultimates
    TotalReserve[ii] <- Forecast$TotalReserve
    Cumulatives[ii,,] <- as.matrix(Forecast$Cumulatives)
  }
  
  Avg_Reserve <- apply(Reserves,2,mean)
  SD_Reserve <- apply(Reserves,2,sd)
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  Avg_TotalReserve <- mean(TotalReserve)
  SD_TotalReserve <- sd(TotalReserve)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  result <- list()
  result$CL_facs <- CL_facs
  result$Latest <- CL_Result$Latest
  result$CL_Reserves <- CL_Result$Reserves
  result$CL_Ultimates <- CL_Result$Ultimates
  result$CL_Cumulatives <- CL_Result$Cumulatives
  result$NegBinResids <- NegBin_Resids
  result$PseudoLRs <- Pseudo_LRs
  result$Cumulatives <- Cumulatives
  result$Reserves <- Reserves
  result$Ultimates <- Ultimates
  result$TotalReserve <- TotalReserve
  result$Avg_Reserve <- Avg_Reserve
  result$SD_Reserve <- SD_Reserve
  result$Avg_TotalReserve <- Avg_TotalReserve
  result$SD_TotalReserve <- SD_TotalReserve
  result$CoV_Reserve <- CoV_Reserve
  result$CoV_TotalReserve <- CoV_TotalReserve
  result$MCMCfit <- fit
  result$iterations <- iterations
  result$finished <- TRUE
  
  return(result)
}

Run_MCMC <- function(tridata, method, replications=2000, chains=1, seed=sample(1e6,1), 
                     ForecastDist="Gamma", UserSqrtScale=NULL) {
  # Control function for bootstrapping
  if (method=="Mack") {
    #MCMC_model <<- stan_model('Macks model in Stan using R.stan') # super-assignment needed
    result <- Main_Mack_MCMC(tridata, iter=replications, chains=chains, seed=seed,
                             ForecastDist, UserSqrtScale)
  }
  else if (method=="ODPNonConstant") {
    #MCMC_model <<- stan_model('ODP model in Stan using R.stan') # super-assignment needed
    result <- Main_ODP_MCMC(tridata, Scale="NonConstant", iter=replications, chains=chains, seed=seed,
                            ForecastDist, UserSqrtScale)
  }
  else if (method=="ODPConstant") {
    #MCMC_model <<- stan_model('ODP model in Stan using R.stan') # super-assignment needed
    result <- Main_ODP_MCMC(tridata, Scale="Constant", iter=replications, chains=chains, seed=seed,
                            ForecastDist, UserSqrtScale)
  }
  else if (method=="NegBinNonConstant") {
    #MCMC_model <<- stan_model('NegBin model in Stan using R.stan') # super-assignment needed
    result <- Main_NegBin_MCMC(tridata, Scale="NonConstant", iter=replications, chains=chains, seed=seed,
                               ForecastDist, UserSqrtScale)
  }
  else if (method=="NegBinConstant") {
    #MCMC_model <<- stan_model('NegBin model in Stan using R.stan') # super-assignment needed
    result <- Main_NegBin_MCMC(tridata, Scale="Constant", iter=replications, chains=chains, seed=seed,
                               ForecastDist, UserSqrtScale)
  }
  return(result)
}

## GRAPHS and TABLES

# Data graph
plotTriangleGraph <- function(tridata) {
  Triangle_zero <- cbind(0,as.matrix(tridata))
  Claims_vector <- as.vector(Triangle_zero)
  op <- as.vector(row(Triangle_zero))
  op <- op[!is.na(Claims_vector)]
  dp <- as.vector(col(Triangle_zero)-1)
  dp <- dp[!is.na(Claims_vector)]
  Claims_vector <- Claims_vector[!is.na(Claims_vector)]
  Claims_Graph_data <- tibble(op,dp,Claims_vector)
  
  ggplot(Claims_Graph_data, aes(x=dp, y=Claims_vector, colour=factor(op))) +
    geom_line(size=1) +
    ggtitle("Claim Amounts by Development Period") +
    labs(y="Claim Amounts",x="Development Period", colour="Origin Period") +
    scale_x_continuous(expand = c(0,0), breaks = sort(unique(dp))) +
    scale_y_continuous(labels = scales::comma, expand = expand_scale(mult = c(0, .05))) +
    theme_bw()
}

# Link ratio graph
plotLRsGraph <- function(tridata) {
  Triangle <- as.matrix(tridata)
  Claims_vector <- as.vector(Triangle)
  op <- as.vector(row(Triangle))
  op <- op[!is.na(Claims_vector)]
  dp <- as.vector(col(Triangle))
  dp <- dp[!is.na(Claims_vector)]
  Claims_vector <- Claims_vector[!is.na(Claims_vector)]
  Claims_Graph_data <- tibble(op,dp,Claims_vector)
  
  ggplot(Claims_Graph_data, aes(x=dp, y=Claims_vector, colour=factor(op))) +
    geom_line(size=1) +
    ggtitle("Link Ratios by Development Period") +
    labs(y="Link Ratio",x="Development Period", colour="Origin Period") +
    scale_x_continuous(expand = c(0,0), breaks = sort(unique(dp))) +
    scale_y_continuous(labels = scales::comma, expand = expand_scale(mult = c(0, .05))) +
    theme_bw()
}


# Development graph

plotDevelopmentGraph <- function(Cumulatives, OY) {
  
  Cum_data <- Cumulatives[, OY, ]
  ndp <- ncol(Cum_data)
  dp <- 0:ndp
  
  Cum_data <- cbind(0, Cum_data)
  
  percentiles <- seq(1, 99, by = 2)
  perc_mat <- sapply(percentiles, function(p) apply(Cum_data, 2, quantile, p/100))
  colnames(perc_mat) <- percentiles
  
  perc_df <- data.frame(
    dp = dp,
    perc_mat,
    check.names = FALSE
  )
  
  pairs <- data.frame(
    lower = percentiles[percentiles < 50],
    upper = 100 - percentiles[percentiles < 50]
  )
  
  pairs$alpha <- {
    dist <- abs(pairs$lower - 50) / 50
    0.05 + 0.6 * (1 - dist)^2
  }
  
  p <- ggplot() +
    labs(
      title = paste("Cumulative Amounts: Origin Period =", OY),
      x = "Development Period",
      y = "Cumulative Amounts"
    ) +
    theme_bw()
  
  for (i in seq_len(nrow(pairs))) {
    lo <- as.character(pairs$lower[i])
    hi <- as.character(pairs$upper[i])
    
    p <- p +
      geom_ribbon(
        data = perc_df,
        aes(
          x = dp,
          ymin = .data[[lo]],
          ymax = .data[[hi]]
        ),
        fill = "blue",
        alpha = pairs$alpha[i]
      )
  }
  
  Cum_mean <- apply(Cum_data, 2, mean)
  Cum_q10  <- apply(Cum_data, 2, quantile, 0.10)
  Cum_q25  <- apply(Cum_data, 2, quantile, 0.25)
  Cum_q75  <- apply(Cum_data, 2, quantile, 0.75)
  Cum_q90  <- apply(Cum_data, 2, quantile, 0.90)
  
  stats_df <- data.frame(
    dp = dp,
    Mean = Cum_mean,
    Q10 = Cum_q10,
    Q25 = Cum_q25,
    Q75 = Cum_q75,
    Q90 = Cum_q90
  )
  
  p <- p +
    geom_line(data = stats_df, aes(dp, Q10), colour = "black", linetype = "dotted", size = 1) +
    geom_line(data = stats_df, aes(dp, Q90), colour = "black", linetype = "dotted", size = 1) +
    geom_line(data = stats_df, aes(dp, Q25), colour = "black", linetype = "dashed", size = 1.2) +
    geom_line(data = stats_df, aes(dp, Q75), colour = "black", linetype = "dashed", size = 1.2) +
    geom_line(data = stats_df, aes(dp, Mean), colour = "green2", size = 1.2) +
    scale_x_continuous(
      breaks = dp,
      limits = c(0, ndp),
      expand = c(0, 0)
    )
  
  p
}





# Histograms
plotHistogram_Total <- function(Reserves) {
  Reserves_df <- as.data.frame(Reserves)
  Mean_Reserves <- mean(Reserves)
  VAR_Reserves_25 <- VAR(Reserves, 0.25)
  VAR_Reserves_50 <- VAR(Reserves, 0.50)
  VAR_Reserves_75 <- VAR(Reserves, 0.75)
  Histo_theme <- theme(plot.title = element_text(hjust=0.5),legend.title=element_blank())
  print(ggplot(Reserves_df, aes(Reserves)) +
          geom_histogram(bins=50, col="black", fill="white") +
          labs(title=paste("Reserves Density: Total"), x="Reserves", y="Frequency") +
          geom_vline(aes(xintercept = Mean_Reserves, colour="1"), size=1) +
          geom_vline(aes(xintercept = VAR_Reserves_25, colour="2"),size=1) +
          geom_vline(aes(xintercept = VAR_Reserves_50, colour="3"),size=1) +
          geom_vline(aes(xintercept = VAR_Reserves_75, colour="4"), size=1) +
          scale_color_hue(labels = c(paste("Mean",format(round(Mean_Reserves), big.mark=",")),
                                     paste("25th pctile",format(round(VAR_Reserves_25), big.mark=",")),
                                     paste("Median",format(round(VAR_Reserves_50), big.mark=",")),
                                     paste("75th pctile", format(round(VAR_Reserves_75), big.mark=",")))) +
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + # removes lower margin but retains small uppper margin
          Histo_theme)
}


plotHistogram_by_yr <- function(reserves_by_yr) {
  for (ii in 2:length(reserves_by_yr[1,])) {
    Reserves <- reserves_by_yr[,ii]
    Reserves_df <- as.data.frame(Reserves)
    Mean_Reserves <- mean(Reserves)
    VAR_Reserves_25 <- VAR(Reserves, 0.25)
    VAR_Reserves_50 <- VAR(Reserves, 0.50)
    VAR_Reserves_75 <- VAR(Reserves, 0.75)
    Histo_theme <- theme(plot.title = element_text(hjust=0.5),legend.title=element_blank())
    print(ggplot(Reserves_df, aes(Reserves)) +
            geom_histogram(bins=50, col="black", fill="white") +
            labs(title=paste("Reserves Density: Origin Period = ",ii), x="Discounted Reserves", y="Frequency") +
            geom_vline(aes(xintercept = Mean_Reserves, colour="1"), size=1) +
            geom_vline(aes(xintercept = VAR_Reserves_25, colour="2"),size=1) +
            geom_vline(aes(xintercept = VAR_Reserves_50, colour="3"),size=1) +
            geom_vline(aes(xintercept = VAR_Reserves_75, colour="4"), size=1) +
            scale_color_hue(labels = c(paste("Mean",format(round(Mean_Reserves), big.mark=",")),
                                       paste("25th pctile",format(round(VAR_Reserves_25), big.mark=",")),
                                       paste("Median",format(round(VAR_Reserves_50), big.mark=",")),
                                       paste("75th pctile", format(round(VAR_Reserves_75), big.mark=",")))) +
            scale_x_continuous(labels = scales::comma) +
            scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + # removes lower margin but retains small uppper margin
            Histo_theme)
  }
}


# Residuals graphs
plotResiduals <- function(Resids, resid_type="zeroavgscaledresids", bstrap_method="Mack") {
  if (resid_type == "unscaledresids") {
    g_resids <- round(Resids$unscaledresids,3)
  } else if (resid_type == "adjunscaledresids"){
    g_resids <- round(Resids$adjunscaledresids,3)
  } else if (resid_type == "scaledresids"){
    g_resids <- round(Resids$scaledresids,3)
  } else if (resid_type == "adjscaledresids"){
    g_resids <- round(Resids$adjscaledresids,3)
  } else if (resid_type == "zeroavgscaledresids"){
    g_resids <- round(Resids$zeroavgscaledresids,3)
  }
  g_resids_vector <- as.vector(g_resids)
  op <- as.vector(row(g_resids))
  op <- op[!is.na(g_resids_vector)]
  dp <- as.vector(col(g_resids))
  dp <- dp[!is.na(g_resids_vector)]
  g_resids_vector <- g_resids_vector[!is.na(g_resids_vector)]
  Resids_Graph_data <- tibble(op,dp,cp=op+dp-1,g_resids_vector )
  
  mean_by_op <- lm(g_resids_vector ~ factor(op), data=Resids_Graph_data)$fitted.values
  mean_by_dp <- lm(g_resids_vector ~ factor(dp), data=Resids_Graph_data)$fitted.values
  mean_by_cp <- lm(g_resids_vector ~ factor(cp), data=Resids_Graph_data)$fitted.values
  scale_by_dp <- rep(NA, length(g_resids_vector))
  #scale_by_dp <- Resids$sqrtScale[dp]
  scale_by_dp <- Resids$sqrtScale
  
  res_tmp <- g_resids_vector - min(g_resids_vector)
  scale_tmp <- Resids$sqrtScale - min(Resids$sqrtScale)
  z <- max(scale_tmp) / max(res_tmp)
  scale_tmp <- if (!(bstrap_method %in% c("ODPConstant","NegBinConstant"))) {scale_tmp/z + min(g_resids_vector)} else {scale_tmp}
  scale_by_dp <- scale_tmp[dp]
  #scale_by_dp <- scale_tmp
  
  info <- if (resid_type == "unscaledresids") { "Unscaled"}
  else if (resid_type == "adjunscaledresids"){ "Adjusted Unscaled" }
  else if (resid_type == "scaledresids"){ "Scaled" }
  else if (resid_type == "adjscaledresids"){ "Adjusted Scaled" }
  else if (resid_type == "zeroavgscaledresids"){"Zero-Average Adjusted Scaled" }

  # Residuals by selected time period

  #  if (resids_gph_x == "op") {
  gph <- ggplot(Resids_Graph_data, aes(x=op, y=g_resids_vector))
  gph <- gph + geom_point(size=3, colour="blue", shape="cross")
  gph <- gph + geom_hline(yintercept = 0, linetype="dashed")
  #    if (show_average==TRUE) {
  gph <- gph + geom_line(aes(y=mean_by_op, colour="Average"), size=1)
  #    }
  gph <- gph + ggtitle(paste( info, "Residuals by Origin Period"))
  gph <- gph + labs(y=paste( info, "Residuals"),x="Origin Period")
  gph <- gph + scale_x_continuous(breaks=c(1:length(g_resids[1,])))
  #      scale_y_continuous(breaks = seq(-2, +2, by = 0.5)) +
  gph <- gph + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + #+ Resids_theme
    scale_colour_manual(values = "green") #+ Resids_theme
  print(gph)
  #  } else if (resids_gph_x == "dp") {
  info2 <- if (bstrap_method == "Mack") { "Mack's alpha"} else { "Sqrt(Scale)" }
  gph <- ggplot(Resids_Graph_data, aes(x=dp, y=g_resids_vector))
  gph <- gph + geom_point(size=3, colour="blue", shape="cross")
  gph <- gph + geom_hline(yintercept = 0, linetype="dashed")
  #    if (show_average==TRUE) {
  #gph <- gph + geom_line(aes(y=mean_by_dp), colour="green", size=1)
  gph <- gph + geom_line(aes(y=mean_by_dp, colour="Average"), size=1)
  #    }
  #    if (show_scale == TRUE) {
  #gph <- gph + geom_line(aes(y=scale_by_dp, colour="Sqrt(Scale)"), colour= "blue", size=1)
  gph <- gph + geom_line(aes(y=scale_by_dp, colour= info2), size=1)
  gph <- if (!(bstrap_method %in% c("ODPConstant","NegBinConstant"))) {
    gph + scale_y_continuous(sec.axis = sec_axis(~ (. -min(g_resids_vector))*z, name = info2))
  } else {
    gph + scale_y_continuous(sec.axis = sec_axis(~. +Resids$sqrtScale[1] , name = info2))
  }
  #    }
  # scale_colour_manual(values = c("green", "blue"))
  gph <- gph + ggtitle(paste( info, "Residuals by Development Period"))
  gph <- gph + labs(y=paste( info, "Residuals"),x="Development Period")
  gph <- gph + scale_x_continuous(breaks=c(1:length(g_resids[1,])))
  gph <- gph +   scale_color_hue(labels = c("Average", info2))
  gph <- gph + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + #+ Resids_theme
    scale_colour_manual(values = c("green", "blue"))
  print(gph)
  # } else if (resids_gph_x == "cp") {
  gph <- ggplot(Resids_Graph_data, aes(x=cp, y=g_resids_vector))
  gph <- gph + geom_point(size=3, colour="blue", shape="cross")
  gph <- gph + geom_hline(yintercept = 0, linetype="dashed")
  #    if (show_average==TRUE) {
  gph <- gph + geom_line(aes(y=mean_by_cp, colour="Average"), size=1)
  #    }
  gph <- gph + ggtitle(paste( info, "Residuals by Calendar Period"))
  gph <- gph + labs(y=paste( info, "Residuals"),x="Calendar Period")
  gph <- gph + scale_x_continuous(breaks=c(1:length(g_resids[1,])))
  gph <- gph + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + #+ Resids_theme
    scale_colour_manual(values = "green") #+ Resids_theme
  print(gph)
  #  }
}


ShowSummaryStats <- function(Stochastic_Results, Output = "Reserves") {
  # Function for calculating summary statistics from bootstrap results
  
  if (Output == "Reserves") {
    ReserveSims <- cbind(Stochastic_Results$Reserves, Stochastic_Results$TotalReserve)
  } else if (Output == "Ultimates") {
    ReserveSims <- cbind(
      Stochastic_Results$Ultimates,
      Stochastic_Results$TotalReserve + sum(Stochastic_Results$Latest)
    )
  }
  
  # Calculate summary statistics
  mean_reserves <- apply(ReserveSims, 2, mean)
  std_reserves <- apply(ReserveSims, 2, sd)
  cov_reserves <- safe_divide(std_reserves, abs(mean_reserves))  # Coefficient of variation
  min_reserves <- apply(ReserveSims, 2, min)
  max_reserves <- apply(ReserveSims, 2, max)
  
  percentiles <- c(0.5, 1, 5, 10, 25, 50, 75, 90, 95, 99, 99.5)
  percentile_results <- sapply(percentiles, function(p) {
    apply(ReserveSims, 2, quantile, probs = p / 100)
  })
  percentile_results <- t(percentile_results)
  
  # Create column labels
  num_cols <- ncol(ReserveSims)
  col_labels <- c(paste0("OP ", seq_len(num_cols - 1)), "Total")
  
  # Combine all rows: Mean, SD, CoV%, Min, percentiles, Max
  all_stats <- rbind(
    mean_reserves,
    std_reserves,
    cov_reserves * 100,  # Convert to percentage
    min_reserves,
    percentile_results,
    max_reserves
  )
  
  row_labels <- c(
    "Mean",
    "Standard Deviation",
    "Coefficient of Variation (%)",
    "Minimum",
    "0.5th Percentile",
    "1st Percentile",
    "5th Percentile",
    "10th Percentile",
    "25th Percentile",
    "50th Percentile (Median)",
    "75th Percentile",
    "90th Percentile",
    "95th Percentile",
    "99th Percentile",
    "99.5th Percentile",
    "Maximum"
  )
  
  colnames(all_stats) <- col_labels
  rownames(all_stats) <- row_labels
  
  table_df <- data.frame(all_stats)
  
  df_formatted <- table_df %>%
    mutate(across(where(is.numeric), ~ ifelse(
      row_number() == 3,
      sprintf("%.1f", .),  # 2 decimal places for row 3
      sprintf("%.0f", .)   # 0 decimal places for others
    )))
  
  print(paste("Simulation Summary Statistics", Output, sep = " - "))
  print(df_formatted)
  
  return(invisible(all_stats))
}


## Sensitivities using Mack's method applied analytically

Sensitivities <- function(Triangle) {
  # Function to identify influential link ratios associated with a claims triangle
  # using Mack's model applied analytically and excluding each ratio in turn
  
  n <- ncol(Triangle) - 1
  Mask <- matrix(1, nrow = n, ncol = n)
  
  # Initialize matrices with NaN
  S_Reserves <- matrix(NA, nrow = n, ncol = n)
  S_Reserves_diff <- matrix(NA, nrow = n, ncol = n)
  S_Reserves_absdiff <- matrix(NA, nrow = n, ncol = n)
  S_Reserves_rank <- matrix(NA, nrow = n, ncol = n)
  S_ReservesSD <- matrix(NA, nrow = n, ncol = n)
  S_ReservesSD_diff <- matrix(NA, nrow = n, ncol = n)
  S_ReservesSD_absdiff <- matrix(NA, nrow = n, ncol = n)
  S_ReservesSD_rank <- matrix(NA, nrow = n, ncol = n)
  S_ReservesCoV <- matrix(NA, nrow = n, ncol = n)
  S_ReservesCoV_diff <- matrix(NA, nrow = n, ncol = n)
  S_ReservesCoV_absdiff <- matrix(NA, nrow = n, ncol = n)
  S_ReservesCoV_rank <- matrix(NA, nrow = n, ncol = n)
  
  # Get base case results
  Mack_Analytic_Result_Base <- Mack_ChainLadder(Triangle, Mask)
  
  # Sensitivity analysis: exclude each link ratio in turn
  for (i in 1:n) {
    for (j in 1:(n - i + 1)) {
      if (i == 1 && j == n) {
        next  # Skip the last ratio
      }
      
      Mask_tmp <- Mask
      Mask_tmp[i, j] <- 0
      Analytic_Result_tmp <- Mack_ChainLadder(Triangle, Mask = Mask_tmp)
      
      # Store reserves results
      S_Reserves[i, j] <- Analytic_Result_tmp$TotalReserves
      S_Reserves_diff[i, j] <- Analytic_Result_tmp$TotalReserves - Mack_Analytic_Result_Base$TotalReserves
      S_Reserves_absdiff[i, j] <- abs(S_Reserves_diff[i, j])
      
      # Store SD results
      S_ReservesSD[i, j] <- Analytic_Result_tmp$TotalReserve_SD
      S_ReservesSD_diff[i, j] <- Analytic_Result_tmp$TotalReserve_SD - Mack_Analytic_Result_Base$TotalReserve_SD
      S_ReservesSD_absdiff[i, j] <- abs(S_ReservesSD_diff[i, j])
      
      # Store CoV results
      S_ReservesCoV[i, j] <- Analytic_Result_tmp$TotalReserve_CoV
      S_ReservesCoV_diff[i, j] <- Analytic_Result_tmp$TotalReserve_CoV - Mack_Analytic_Result_Base$TotalReserve_CoV
      S_ReservesCoV_absdiff[i, j] <- abs(S_ReservesCoV_diff[i, j])
    }
  }
  
  # Rank the differences in ascending order
  S_Reserves_rank <- rank(as.vector(S_Reserves_diff))
  S_Reserves_rank <- matrix(S_Reserves_rank, nrow = nrow(S_Reserves_diff), ncol = ncol(S_Reserves_diff))
  S_Reserves_rank[is.na(S_Reserves_diff)] <- NA
  
  S_ReservesSD_rank <- rank(as.vector(S_ReservesSD_diff))
  S_ReservesSD_rank <- matrix(S_ReservesSD_rank, nrow = nrow(S_ReservesSD_diff), ncol = ncol(S_ReservesSD_diff))
  S_ReservesSD_rank[is.na(S_ReservesSD_diff)] <- NA
  
  S_ReservesCoV_rank <- rank(as.vector(S_ReservesCoV_diff))
  S_ReservesCoV_rank <- matrix(S_ReservesCoV_rank, nrow = nrow(S_ReservesCoV_diff), ncol = ncol(S_ReservesCoV_diff))
  S_ReservesCoV_rank[is.na(S_ReservesCoV_diff)] <- NA
  
  # Return results as list
  result <- list(
    Reserves_Base = round(Mack_Analytic_Result_Base$TotalReserves,0),
    ReservesSD_Base = round(Mack_Analytic_Result_Base$TotalReserve_SD,0),
    ReservesCoV_Base = round(Mack_Analytic_Result_Base$TotalReserve_CoV,2),
    S_Reserves = round(S_Reserves,0),
    S_Reserves_diff = round(S_Reserves_diff,1),
    S_Reserves_absdiff = round(S_Reserves_absdiff,1),
    S_Reserves_rank = S_Reserves_rank,
    S_ReservesSD = round(S_ReservesSD,0),
    S_ReservesSD_diff = round(S_ReservesSD_diff,1),
    S_ReservesSD_absdiff = round(S_ReservesSD_absdiff,1),
    S_ReservesSD_rank = S_ReservesSD_rank,
    S_ReservesCoV = round(S_ReservesCoV,3),
    S_ReservesCoV_diff = round(S_ReservesCoV_diff,3),
    S_ReservesCoV_absdiff = round(S_ReservesCoV_absdiff,3),
    S_ReservesCoV_rank = S_ReservesCoV_rank
  )
  
  return(result)
}


## Scaling simulated results to a given target

# Calculate scaled reserves given target ultimates and scaling method
Scaled_Results <- function(Simul_Results, Target_Ultimates, Scaling_Method) {
  
  num_iterations <- nrow(Simul_Results$Reserves)
  num_origin_periods <- ncol(Simul_Results$Reserves)
  
  # Calculate target reserves and differences
  Target_Reserves <- Target_Ultimates - Simul_Results$Latest
  Target_Diff <- Target_Reserves - Simul_Results$Avg_Reserve
  
  # Calculate target multiplier (handling zero division)
  Target_Multiplier <- numeric(num_origin_periods)
  for (i in 1:num_origin_periods) {
    if (Simul_Results$Avg_Reserve[i] == 0) {
      Target_Multiplier[i] <- 0
    } else {
      Target_Multiplier[i] <- safe_divide(Target_Reserves[i], Simul_Results$Avg_Reserve[i])
    }
  }
  
  # Apply selected scaling method for each origin period
  ScaledReserves <- matrix(0, nrow = num_iterations, ncol = num_origin_periods)
  
  for (i in 1:num_iterations) {
    for (j in 1:num_origin_periods) {
      if (Scaling_Method[j] == 'Additive') {
        ScaledReserves[i, j] <- Simul_Results$Reserves[i, j] + Target_Diff[j]
      } else if (Scaling_Method[j] == 'Multiplicative') {
        ScaledReserves[i, j] <- Simul_Results$Reserves[i, j] * Target_Multiplier[j]
      } else {
        stop(paste("Unknown scaling method '", Scaling_Method[j], 
                   "' for origin period ", j, sep = ""))
      }
    }
  }
  
  # Calculate summary statistics
  Latest <- Simul_Results$Latest
  Reserves <- ScaledReserves
  TotalReserve <- rowSums(Reserves)
  Avg_Reserve <- apply(Reserves, 2, mean, na.rm = TRUE)
  SD_Reserve <- apply(Reserves, 2, sd, na.rm = TRUE)
  
  # CoV_Reserve: NaN for first element, then SD/Mean for remaining
  CoV_Reserve <- safe_divide(SD_Reserve, abs(Avg_Reserve))
  
  Avg_TotalReserve <- mean(TotalReserve, na.rm = TRUE)
  SD_TotalReserve <- sd(TotalReserve, na.rm = TRUE)
  CoV_TotalReserve <- safe_divide(SD_TotalReserve, abs(Avg_TotalReserve))
  
  # Return results as a list
  result <- list(
    Latest = Latest,
    Reserves = Reserves,
    TotalReserve = TotalReserve,
    Avg_Reserve = Avg_Reserve,
    SD_Reserve = SD_Reserve,
    Avg_TotalReserve = Avg_TotalReserve,
    SD_TotalReserve = SD_TotalReserve,
    CoV_Reserve = CoV_Reserve,
    CoV_TotalReserve = CoV_TotalReserve
  )
  
  return(result)
}


## Wrapper for calculating residuals

Calc_Residuals <- function(triangle, method, Mask=matrix(1,nrow(triangle)-1, ncol(triangle)-1)) {
  
  # Determine residuals based on method
  Resids <- switch(method,
                   
       "Mack" = {
         Mack_Residuals(triangle, Mask = Mask)
       },
       
       "ODPConstant" = {
         Scale <- "Constant"
         ODP_Residuals(triangle, Scale = Scale, Mask = Mask)
       },
       
       "ODPNonConstant" = {
         Scale <- "NonConstant"
         ODP_Residuals(triangle, Scale = Scale, Mask = Mask)
       },
       
       "NegBinConstant" = {
         Scale <- "Constant"
         NegBin_Residuals(triangle, Scale = Scale, Mask = Mask)
       },
       
       "NegBinNonConstant" = {
         Scale <- "NonConstant"
         NegBin_Residuals(triangle, Scale = Scale, Mask = Mask)
       },
       
       stop("Unknown method specified")
  )
  
  # Return selected components
  result <- list(
    unscaledresids = Resids$unscaledresids,
    adjunscaledresids = Resids$adjunscaledresids,
    scaledresids = Resids$scaledresids,
    adjscaledresids = Resids$adjscaledresids,
    zeroavgscaledresids = Resids$zeroavgscaledresids,
    sqrtScale = Resids$sqrtScale,
    avgresid = Resids$avgresid
  )
  
  return(result)
}
