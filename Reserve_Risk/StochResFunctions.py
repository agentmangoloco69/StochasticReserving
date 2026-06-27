# version 1.0.0
#
# Stochastic reserving functions in Python to support reproduction of the tables in:

# England, Verrall & Wüthrich (2018/2019). On the lifetime and one-year views of reserve risk, 
# with application to IFRS 17 and Solvency II risk margins.
# Insurance: Mathematics and Economics (2019) https://doi.org/10.1016/j.insmatheco.2018.12.002
# (Pre-print version available from SSRN (2018) https://ssrn.com/abstract=3141239)

# and:

# England & Verrall (2006). Predictive distributions of outstanding liabilities in general insurance,
# Annals of Actuarial Science, 1, II, 221-270. https://doi.org/10.1017/S1748499500000142

# This code is provided by Peter England on behalf of EMC Actuarial and Analytics Ltd as an educational resource.

# NOTE: Functions for the MCMC methods in England & Verrall (2006) are included in a separate Python file,
# StochResFunctions_MCMC.py, to avoid the need to install the additional packages required for MCMC methods.
# Those packages are not straightforward to install and are not required for users only interested in 
# bootstrapping and analytic (closed form) methods.

# Dependent packages
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

## Functions for plots and tables

def npNaN(a):
    # Function to return an array of the same shape as the input, but with all values set to NaN
    a = np.empty(a)
    a[:] = np.nan
    return a

def safe_divide(numerator, denominator, fill_value=0.0):
   # Element-wise division of arrays, returning fill_value where denominator is zero or missing.
    
    numer = np.asarray(numerator)
    denom = np.asarray(denominator)
    
    # Initialize output with fill_value
    out = np.full(np.broadcast_shapes(numer.shape, denom.shape), fill_value, dtype=float)
    
    # Suppress divide-by-zero and invalid warnings (we handle them explicitly)
    with np.errstate(divide='ignore', invalid='ignore'):
        # Divide only where denom is not zero AND not NaN
        mask = (denom != 0) & ~np.isnan(denom)
        np.divide(numer, denom, out=out, where=mask)
    
    # Return scalar if inputs were scalar
    if out.shape == ():
        return float(out)
    return out

def replace_nan_with_zeros(tridata):
    # replace missing values in north west triangle with zeros
    result = tridata.copy()

    # Determine the mask for the upper triangle
    upper_mask = np.fliplr(np.triu(np.ones_like(result, dtype=bool), k=0))

    # Find NaNs in the upper triangle
    nan_mask = np.isnan(result) & upper_mask

    # Replace NaNs with zeros
    result[nan_mask] = 0

    return result


def table_plot(data, row_labels = None, col_labels = None, title = None):
    # Function for plotting formatted table
    fig, ax1 = plt.subplots(figsize=(20, 2 + len(data) / 2.5))

    data = data.tolist()

    for i in range(len(data)):
        data[i] = [str(x)+' ' for x in data[i]]
        data[i] = [w.replace('nan ', ' ') for w in data[i]]
        data[i] = [w.replace('.0 ', ' ') for w in data[i]]

    rcolors = np.full(len(data), 'linen')
    ccolors = np.full(len(data[0]), 'lavender')

    table = ax1.table(cellText=data,
                  cellLoc='center',
                  rowLabels=row_labels,
                  rowColours=rcolors,
                  rowLoc='center',
                  colColours=ccolors,
                  colLabels=col_labels,
                  loc='center')
    table.scale(1, 2)
    table.set_fontsize(16)
    ax1.axis('off')

    ax1.set_title(title, weight='bold', size=28, color='k')

    #plt.savefig(title+".png", dpi=200, bbox_inches='tight')

    plt.show()


def line_plot(data, title = None, xlabel = None, ylabel = None, LegendTitle = None, withZeroPoint = False):
    # Function for line graphs

    fig, ax1 = plt.subplots(figsize=(20, 2 + len(data) / 2.5))

    for i in range(len(data)):

        plotdata = data[i]

        if withZeroPoint == True:
            xmin = 0
            plotdata = [0, *plotdata] # Add (0,0) point

        else:
            xmin = 1

        ax1.plot(range(xmin, len(plotdata)+xmin), plotdata, label=str(i+1))

    
    ax1.legend(title = LegendTitle)
    ax1.set_title(title, weight='bold', size=28, color='k')
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel)
    ax1.grid(True, which='both')

    #plt.savefig(title+".png", dpi=200, bbox_inches='tight')

    plt.show()


def scatter_plot(tridata, title=None, xlabel=None, ylabel=None,
                 rowData=True, calendarData=False, sigma=None, showAverage=True):
    # Function for residual scatter plots

    fig, ax1 = plt.subplots(figsize=(20, 2 + len(tridata) / 2.5))
    data = tridata

    ax1.axhline(y=0, linestyle='--', color='k')

    legend_lines = []
    legend_labels = []

    # =========================
    # CALENDAR PERIOD (FIXED)
    # =========================
    if calendarData:
        bins = {}

        # Group by calendar period (i + j)
        for i in range(len(data)):
            for j in range(len(data[i])):
                val = data[i][j]
                k = i + j  # 0-based calendar index
                bins.setdefault(k, []).append(val)

        max_k = max(bins.keys()) + 1
        data_avg = npNaN(max_k)

        # Plot: ALL points at same x per calendar period
        for k in bins:
            yvals = bins[k]
            xvals = [k + 1] * len(yvals)  # <-- key fix
            ax1.scatter(xvals, yvals, c='darkblue', marker='x', linewidths=1)

            valid = [x for x in yvals if not np.isnan(float(x))]
            data_avg[k] = np.nanmean(valid) if valid else np.nan

        sigma = None  # not meaningful here

    # =========================
    # ROW / COLUMN MODES
    # =========================
    else:
        series = []

        for i in range(len(data)):
            if rowData:  # rows (origin)
                scatterline = data[i]
            else:  # columns (development)
                scatterline = [data[j][i] for j in range(len(data)) if i < len(data[j])]
                sigma = None

            series.append(scatterline)

        max_len = max(len(s) for s in series)
        bins = [[] for _ in range(max_len)]

        for s in series:
            ax1.scatter(range(1, len(s) + 1), s,
                        c='darkblue', marker='x', linewidths=1)
            for j, val in enumerate(s):
                bins[j].append(val)

        data_avg = npNaN(max_len)

        for i in range(len(data_avg)):
            vals = [x for x in bins[i] if not np.isnan(float(x))]
            data_avg[i] = np.nanmean(vals) if vals else np.nan

    # =========================
    # AVERAGE LINE
    # =========================
    if showAverage:
        l1, = ax1.plot(range(1, len(data_avg) + 1), data_avg,
                       linewidth=2.5, color='lime')
        legend_lines.append(l1)
        legend_labels.append("Average")

    # =========================
    # SIGMA (SECOND AXIS)
    # =========================
    ax2 = ax1.twinx()
    if sigma is not None:
        l2, = ax2.plot(range(1, len(sigma) + 1), sigma,
                       linewidth=2.5, color='mediumblue')
        legend_lines.append(l2)
        legend_labels.append("SqrtScale")

    if legend_labels:
        plt.legend(legend_lines, legend_labels)

    ax1.set_title(title, weight='bold', size=28, color='k')
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel)
    ax1.grid(True, which='both')

    plt.show()


def fan_plot(dev_gph_year, Cum_tri):
    # Function for reserve development (fan) graphs
    # Improved for large numbers of simulations using percentile coloring with filled bands
    
    ndp = len(Cum_tri[0, 0, :])
    Cum_data = Cum_tri[:, dev_gph_year, :]
    
    Cum_mean = np.nanmean(Cum_data, 0)
    
    # Calculate specific percentiles for reference lines
    Cum_q10 = np.nanpercentile(Cum_data, 10, 0)
    Cum_q25 = np.nanpercentile(Cum_data, 25, 0)
    Cum_q75 = np.nanpercentile(Cum_data, 75, 0)
    Cum_q90 = np.nanpercentile(Cum_data, 90, 0)
    
    x = np.arange(1, ndp+1)
    
    fig, ax = plt.subplots(figsize=(20, 2 + len(Cum_tri[0]) / 2.5))
    
    # Calculate percentiles from 1 to 99 in steps of 2
    percentiles = np.arange(1, 100, 2)  # 1, 3, 5, ..., 99
    
    # Calculate percentile lines once
    perc_lines = {}
    for perc in percentiles:
        perc_lines[perc] = np.nanpercentile(Cum_data, perc, 0)
    
    # Plot filled bands between percentile pairs, working outward from median
    for i, perc in enumerate(sorted(percentiles)):
        # For each percentile, pair it with its mirror (e.g., 5th with 95th)
        mirror_perc = 100 - perc
        
        # Skip if we've already plotted this pair
        if mirror_perc < perc:
            continue
        
        lower_line = perc_lines[perc]
        upper_line = perc_lines[mirror_perc]
        
        # Distance from median determines opacity - closer to median = darker
        distance_from_median = abs(perc - 50)
        normalized_distance = distance_from_median / 50
        alpha = 0.05 + 0.6 * (1 - normalized_distance)**2.0  # Very light at extremes (0.05), darker in middle (0.6)
        
        ax.fill_between(x, lower_line, upper_line, color='blue', alpha=alpha)
    
    # Plot percentile reference lines with different styles
    # 10th and 90th percentiles as dashed lines (lighter tails)
    ax.plot(x, Cum_q10, color='black', linestyle='dotted', linewidth=2.0)
    ax.plot(x, Cum_q90, color='black', linestyle='dotted', linewidth=2.0)
    
    # 25th and 75th percentiles as dotted lines (inner quartiles)
    ax.plot(x, Cum_q25, color='black', linestyle='dashed', linewidth=2.5)
    ax.plot(x, Cum_q75, color='black', linestyle='dashed', linewidth=2.5)
    
    # Plot mean in green on top
    ax.plot(x, Cum_mean, color='lime', linestyle='solid', linewidth=2.5, label='Mean')
    
    ax.set_title("Origin Period: " + str(dev_gph_year+1), weight='bold', size=28, color='k')
    ax.set_xlabel("Development Period")
    ax.set_ylabel("Cumulative Claims")
    ax.set_xticks(range(1, ndp+1))
    ax.grid(True, alpha=0.3)
    
    plt.show()

## General Functions

def triangleUpper(array):
    # Function to create an upper triangle of the same shape as the input, with values in the upper triangle and NaN elsewhere
    nc = len(array[0])
    nr = len(array)
    newArray = npNaN((nr,nc))
    for i in range(nr):
        for j in range(nc-i):
            newArray[i][j] = array[i][j]
    return newArray

def Cumulatives(Incrementals):
    # Function to convert an incremental triangle to a cumulative triangle

    Cumulatives = np.cumsum(Incrementals, axis=1) # Create cumulative array by summing incremental data across rows

    return Cumulatives

def Incrementals(Cumulatives):
    # Function to convert a cumulative triangle to an incremental triangle
    nc = len(Cumulatives[0])
    Incrementals = npNaN(Cumulatives.shape)
    for i in range(nc):
        Incrementals[i][0] = Cumulatives[i][0]
        for j in range(1, nc):
            if np.isnan(Cumulatives[i][j]) == False:
                Incrementals[i][j] = np.nansum((Cumulatives[i][j], -1*Cumulatives[i][j-1]))
    return Incrementals

def LR_Tri(tridata):
    # Function to obtain a triangle of link ratios and associated weights from a cumulative triangle
    n = len(tridata)

    LinkRatioTri = npNaN(tuple(np.subtract(tridata.shape, (0,1))))

    for i in range(n):
        # perform elementwise division but return 0 where the denominator is zero
        numer = tridata[i, 1:].astype(float, copy=False)
        denom = tridata[i, :-1].astype(float, copy=False)
        out = np.zeros_like(numer, dtype=float)
        with np.errstate(divide='ignore', invalid='ignore'):
            np.divide(numer, denom, out=out, where=(denom != 0))
        # if the denominator is missing, set the result to missing (np.nan)
        out[np.isnan(numer)] = np.nan
        LinkRatioTri[i, :] = out
    
    LinkRatioTri[np.isinf(LinkRatioTri)] = np.nan
    LinkRatioTri = np.array(LinkRatioTri[~np.all(np.isnan(LinkRatioTri), axis=1)], dtype = float)
    Weight = np.where(~np.isnan(LinkRatioTri), tridata[:len(LinkRatioTri),:-1], np.nan)

    result = {"Ratios": LinkRatioTri, "Weights": Weight}
    return result
    
def CL_factors(LRTriangle, LRWeights, Mask=None):
    # Function to obtain volume weighted chain ladder factors given a triangle of link ratios and associated weights
    n = len(LRTriangle[0])
    if Mask is None:
        Mask = np.ones_like(LRTriangle)
    LinkRatio = npNaN(n)
    #LinkRatio = []
    LRWeights_tmp = LRWeights*Mask
    LinkRatio_tmp = LRTriangle*LRWeights_tmp
    for i in range(n):
        #LinkRatio.append(np.nansum(LinkRatio_tmp[:, i])/np.nansum(LRWeights_tmp[:, i]))
        LinkRatio[i] = np.nansum(LinkRatio_tmp[:, i])/np.nansum(LRWeights_tmp[:, i])

    LinkRatio[np.isnan(LinkRatio)] = 1.0
    return LinkRatio

def LinkRatioMethod(tridata, Mask=None, pseudoLRs=None):
    # Function to give fitted link ratios (development factors) using development factor methods
    # eg vol-wtd averages, simple averages, curve fitting etc
    # the function is used when calculating residuals and when bootstrapping using the various bootstrap models
    if Mask is None:
        Mask = np.ones((len(tridata[0])-1, len(tridata[0])-1))

    # Link Ratio triangle and associated weights
    # pseudoLRs are used during the bootstrap process for Mack's model and Over-dispersed Negative Binomial model
    LRs = LR_Tri(tridata)
    LR_Weights = LRs["Weights"] 
    if pseudoLRs is None:
        LR_Triangle = LRs["Ratios"]
    else:
        LR_Triangle = pseudoLRs

    # Volume weighted chain ladder factors, allowing for exclusions
    LRM_facs = CL_factors(LR_Triangle, LR_Weights, Mask)

    # Could use other averages, eg simple average

    # Could then fit a curve to the CL_facs (or triangle of ratios) here if desired
    # Would then need to add number of parameters in development direction to result
    # to allow bias correction factors to be modified for residuals later

    result = {"LR_Triangle": LR_Triangle,
              "LR_Weights": LR_Weights,
              "LRM_Factors": LRM_facs}

    return result

def LinkRatioMethod_forecast(tridata, factors):
    # Function to give forecasts using development factor methods, given a cumulative triangle of data and 
    # a set of development factors
    nc = len(tridata[0])
    nr = len(tridata)

    cumulatives = npNaN(tridata.shape)
    for i in range(nr):
        cumulatives[i][-i-1] = tridata[i][-i-1]
        for j in range(nc-i,nc):
            cumulatives[i][j] = cumulatives[i][j-1]*factors[j-1] # Across, multiply previous entry by relevant CL factors
        cumulatives[i][-i-1] = float('nan')

    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)
    #complete_forecast[cumulatives == 0 and tridata == 0] = float('nan')

    Ultimates = complete_forecast[:, -1] 
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest
    TotalReserves = np.nansum(Reserves)

    result = {"Cumulatives":cumulatives,
              "Latest":Latest,
              "Reserves": Reserves,
              "Ultimates": Ultimates, 
              "TotalLatest": np.nansum(Latest),
              "TotalReserves":np.nansum(Reserves),
              "TotalUltimates": np.nansum(Ultimates),
              "CompleteForecast": complete_forecast}

    return result

def arrayRound_and_format(array, dp):
    # Function to round an array to a specified number of decimal places and format with commas, 
    # while leaving NaN values as blank
    newArray = np.zeros(array.shape, dtype = object)
    for i in range(len(array)):
        for j in range(len(array[0])):
            if np.isnan(array[i,j]) == False:
                newArray[i,j] = round(array[i,j],dp)
                newArray[i,j] = f'{float(newArray[i,j]):,}'
            else:
                newArray[i,j] = ""
     
    return newArray

def Rowsum(matrix, nc):
    # Function to sum blocks of increasing size along leading diagonal of a matrix
    # requires matrix to be a numpy array
    Rowsum = np.zeros(nc+1)
    Vsum = np.zeros(nc)
    counter = 1
    for i in range(nc-1):
        Rtot = 0
        counter = counter+i
        for j in range(i+1):
            Vsum[i+1] += matrix[counter+j-1][counter+j-1]
            Rtot += np.sum(matrix[counter+j-1][counter-1:counter+j])
        Rowsum[i+1] = 2*Rtot - Vsum[i+1]

    Rowsum[nc] = np.sum(matrix)
    return Rowsum

def Resample_Resids(residsvector, nc):
    # Function to resample residuals for bootstrapping, given a vector of residuals and the number of development periods

    randoms = np.random.choice(residsvector, size = (nc,nc))

    return randoms


def Disc_Reserves(Incrementals, Disc_Rate, offset=0, periods=0):
    # Function to calculate discounted reserves. If periods > 0, then remaining payments beyond the input periods ahead
    # are discounted back to that calendar period. This is useful for cost-of-capital risk margin calculations.
    nc = len(Incrementals[0])
    Disc_Incrementals = npNaN((nc, nc))
    offset = min(offset, 1)
    
    if periods < nc:
        for i in range(min(2+periods, nc)-1, nc):  # Convert R's 1-based to Python's 0-based
            t = min(nc-i+1+periods, nc)
            for j in range(t-1, nc):  # Convert R's 1-based to Python's 0-based
                Disc_Incrementals[i][j] = Incrementals[i][j] / ((1+Disc_Rate)**(j-t+1+offset))
    
    Disc_Reserves = np.nansum(Disc_Incrementals, 1)
    Disc_Total_Reserves = np.nansum(Disc_Reserves)
    
    result = {"Disc_Reserves": Disc_Reserves,
              "Disc_Total_Reserves": Disc_Total_Reserves}
    return result

def Bstrap_Disc_Reserves(Cumulatives, Disc_rate, offset):
    # Function for discounting simulated future payments following bootstrapping
    iterations = len(Cumulatives)
    nc = len(Cumulatives[0][0])

    Disc_Reserves_by_yr = npNaN((iterations, nc))
    Disc_TotalReserve = npNaN(iterations)

    for i in range(iterations):
        Incremental_claims = Incrementals(Cumulatives[i])
        Disc_Reserves_tmp = Disc_Reserves(Incremental_claims, Disc_rate, offset)

        Disc_Reserves_by_yr[i] = Disc_Reserves_tmp["Disc_Reserves"]
        Disc_TotalReserve[i] = Disc_Reserves_tmp["Disc_Total_Reserves"]

    Avg_Disc_Res = np.nanmean(Disc_Reserves_by_yr, axis = 0)
    SD_Disc_Res = np.nanstd(Disc_Reserves_by_yr, axis = 0)
    CoV_Disc_Res = abs(safe_divide(SD_Disc_Res, Avg_Disc_Res))

    Avg_Disc_TotalRes = np.nanmean(Disc_TotalReserve)
    SD_Disc_TotalRes = np.nanstd(Disc_TotalReserve)
    Cov_Disc_TotalRes = abs(safe_divide(SD_Disc_TotalRes, Avg_Disc_TotalRes))
    
    result = {"Avg_Disc_Res": Avg_Disc_Res, 
          "SD_Disc_Res": SD_Disc_Res, 
          "CoV_Disc_Res": CoV_Disc_Res, 
          "Avg_Disc_TotalRes": Avg_Disc_TotalRes, 
          "SD_Disc_TotalRes": SD_Disc_TotalRes, 
          "Cov_Disc_TotalRes": Cov_Disc_TotalRes, 
          "Reserves": Disc_Reserves_by_yr, 
          "TotalReserve": Disc_TotalReserve}

    return result

def Disc_Future_Reserves(Incrementals, Disc_rate, offset=0):
    # Function to calculate remaining discounted reserves at each future point in time.
    # Note that at each future time period, amounts are discounted back to that calendar period only.
    # Assumes a standard triangle with no tail
    nc = len(Incrementals[0])
    Future_Reserves = npNaN(nc-1)
    
    for period in range(nc-1):
        Future_Reserves[period] = Disc_Reserves(Incrementals, Disc_rate, offset, period)["Disc_Total_Reserves"]
    
    return Future_Reserves

def Bstrap_Disc_Future_Reserves(Cumulatives, Disc_rate, offset=0):
    # Function to calculate remaining discounted reserves at each future point in time for each simulation following bootstrapping.
    # Note that at each future time period, amounts are discounted back to that calendar period only.
    # Assumes a standard triangle with no tail
    iterations = len(Cumulatives)
    nc = len(Cumulatives[0][0])
    Future_Reserves = npNaN((iterations, nc-1))
    
    for ii in range(iterations):
        Incremental_claims = Incrementals(Cumulatives[ii])
        for period in range(nc-1):
            Future_Reserves[ii, period] = Disc_Reserves(Incremental_claims, Disc_rate, offset, period)["Disc_Total_Reserves"]
    
    Avg_Disc_Fut_Res = np.nanmean(Future_Reserves, axis=0)
    SD_Disc_Fut_Res = np.nanstd(Future_Reserves, axis=0)
    
    result = {"FutureReserves": Future_Reserves,
              "Avg_Disc_Fut_Res": Avg_Disc_Fut_Res,
              "SD_Disc_Fut_Res": SD_Disc_Fut_Res,
              "finished": True}
    
    return result


def CDR_Full_Picture(tridata, Forecast_Cumulatives, VAR_p=0.995, Mask=None, Future_Periods=None):
# Function to calculate the "Actuary-in-the-Box" Claims Development Result (CDR) for a sequence of one-year forecasts
# and calculate the Avg and SD of the CDR.
# This is useful for the one-year view of Solvency II, and links together the one-year and the lifetime views of risk
#
# The function calculates the CDR for each future year by default. Use the Future_Periods argument to restrict the range
# if required (eg Future_Periods=1 for 1-year view only)

  if Mask is None:
    Mask = np.ones((len(tridata[0])-1, len(tridata[0])-1))
  
  iterations = len(Forecast_Cumulatives) 
    
  LRM = LinkRatioMethod(tridata, Mask)
  CL_facs = LRM["LRM_Factors"]

  CL_Result = LinkRatioMethod_forecast(tridata, CL_facs)

  nc = len(CL_facs)

  if Future_Periods is None:
    Max_Future_Periods = nc
  else:
    Max_Future_Periods = min(Future_Periods, nc)

  CDR = npNaN((Max_Future_Periods, iterations, (nc+1)))
  cumsum_CDR = npNaN((Max_Future_Periods, iterations, (nc+1)))

  TotalCDR = npNaN((Max_Future_Periods, iterations))
  cumsum_TotalCDR = npNaN((Max_Future_Periods, iterations))
  
  # Add an extra row to Mask since one-year forecast now has a link ratio in final year etc
  Mask = np.vstack((Mask, np.ones((1, len(Mask[0])))))

  for i in range(iterations):

    One_Yr_Forecasts = CL_Result["CompleteForecast"]

    for fy in range(Max_Future_Periods):

      Last_Yr_Ultimates = One_Yr_Forecasts[:,-1]

      One_Yr_Claims = np.triu(np.fliplr(Forecast_Cumulatives[i,:,:]), k=-fy-1)
      One_Yr_Claims[np.tril_indices(One_Yr_Claims.shape[0], -fy-2)] = np.nan

      One_Yr_Claims = np.fliplr(One_Yr_Claims)

      One_Yr_LRs =  LinkRatioMethod(One_Yr_Claims, Mask)["LRM_Factors"]

      One_Yr_Claims_Array = One_Yr_Claims

      for j in range(fy+1):
          One_Yr_Claims_Array = np.delete(One_Yr_Claims_Array, 0, 0) # deletes the desired row 
          One_Yr_Claims_Array = np.delete(One_Yr_Claims_Array, 0, 1) # deletes the desired column at index

      One_Yr_Forecasts = LinkRatioMethod_forecast(One_Yr_Claims_Array, One_Yr_LRs[fy+1:])["CompleteForecast"]

      One_Yr_Forecasts = np.hstack((*[One_Yr_Claims[:,i][fy+1:].reshape(-1, 1) for i in range(fy+1)], One_Yr_Forecasts)) # re-add prev cols

      One_Yr_Forecasts = np.vstack((*[One_Yr_Claims[i] for i in range(fy+1)], One_Yr_Forecasts)) #re-add prev rows

      CDR[fy,i,:] = np.hstack((npNaN(fy+1),(Last_Yr_Ultimates - One_Yr_Forecasts[:,-1])[fy+1:]))

  TotalCDR = np.nansum(CDR, 2)
  
  cumsum_CDR = np.nancumsum(CDR, 0)
  cumsum_TotalCDR = np.nansum(cumsum_CDR, 2)
    
  Avg_CDR = np.zeros((Max_Future_Periods, nc+1))
  SD_CDR = np.zeros((Max_Future_Periods, nc+1))
  CDR_VAR = np.zeros((Max_Future_Periods, nc+1))

  Avg_cumsum_CDR = npNaN((Max_Future_Periods, nc+1))
  SD_cumsum_CDR = npNaN((Max_Future_Periods, nc+1))
  cumsum_CDR_VAR = npNaN((Max_Future_Periods, nc+1))

  for fy in range(Max_Future_Periods):

    Avg_CDR[fy,:] = np.nanmean(CDR[fy,:,:], 0)
    SD_CDR[fy,:] = np.nanstd(CDR[fy,:,:], 0)
    SD_CDR[np.isnan(SD_CDR)] = 0
    #CDR_VAR[fy,:] = np.array([-VAR(CDR[fy,:,j], 1-VAR_p) for j in range(CDR[fy,:,:].shape[1])])
    CDR_VAR[fy,:] = np.array([-VAR(CDR[fy,:,j], 1-VAR_p) for j in range(CDR[fy,:,:].shape[1])]) + Avg_CDR[fy,:]

    Avg_cumsum_CDR[fy,:] = np.nanmean(cumsum_CDR[fy,:,:], 0)
    SD_cumsum_CDR[fy,:] = np.nanstd(cumsum_CDR[fy,:,:], 0)
    #cumsum_CDR_VAR[fy,:] = np.array([-VAR(cumsum_CDR[fy,:,j], 1-VAR_p) for j in range(cumsum_CDR[fy,:,:].shape[1])])
    cumsum_CDR_VAR[fy,:] = np.array([-VAR(cumsum_CDR[fy,:,j], 1-VAR_p) for j in range(cumsum_CDR[fy,:,:].shape[1])]) + Avg_cumsum_CDR[fy,:]

  Avg_TotalCDR = np.nanmean(TotalCDR, 1)
  SD_TotalCDR = np.nanstd(TotalCDR, 1)
  #TotalCDR_VAR = np.array([-VAR(TotalCDR[j,:], 1-VAR_p) for j in range(TotalCDR.shape[0])])
  TotalCDR_VAR = np.array([-VAR(TotalCDR[j,:], 1-VAR_p) for j in range(TotalCDR.shape[0])]) + Avg_TotalCDR

  Avg_cumsum_TotalCDR = np.nanmean(cumsum_TotalCDR, 1)
  SD_cumsum_TotalCDR = np.nanstd(cumsum_TotalCDR, 1)
  #cumsum_TotalCDR_VAR = np.array([-VAR(cumsum_TotalCDR[j,:], 1-VAR_p) for j in range(cumsum_TotalCDR.shape[0])])
  cumsum_TotalCDR_VAR = np.array([-VAR(cumsum_TotalCDR[j,:], 1-VAR_p) for j in range(cumsum_TotalCDR.shape[0])]) + Avg_cumsum_TotalCDR

  result = {"CDR": CDR, 
          "TotalCDR": TotalCDR, 
          "Avg_CDR": Avg_CDR, 
          "SD_CDR": SD_CDR, 
          "CDR_VAR": CDR_VAR,
          "Avg_TotalCDR": Avg_TotalCDR, 
          "SD_TotalCDR": SD_TotalCDR,
          "TotalCDR_VAR": TotalCDR_VAR,
          "cumsum_CDR": cumsum_CDR, 
          "cumsum_TotalCDR": cumsum_TotalCDR, 
          "Avg_cumsum_CDR": Avg_cumsum_CDR, 
          "SD_cumsum_CDR": SD_cumsum_CDR,
          "cumsum_CDR_VAR": cumsum_CDR_VAR,
          "Avg_cumsum_TotalCDR": Avg_cumsum_TotalCDR, 
          "SD_cumsum_TotalCDR": SD_cumsum_TotalCDR,
          "cumsum_TotalCDR_VAR": cumsum_TotalCDR_VAR
          }
  
  return result


def CDR_Rev_Sum(CDR):
    # Function to calculate reverse sum of CDRs from simulated results
    iterations = CDR.shape[1]
    fy = CDR.shape[0]
    RevSum_CDR = npNaN((fy, iterations))
    
    for ii in range(iterations):
        RevSum_CDR[:, ii] = np.flip(np.cumsum(np.flip(CDR[:, ii])))
    
    Avg_RevSum_CDR = np.nanmean(RevSum_CDR, axis=1)
    SD_RevSum_CDR = np.nanstd(RevSum_CDR, axis=1)
    
    result = {"RevSum_CDR": RevSum_CDR,
              "Avg_RevSum_CDR": Avg_RevSum_CDR,
              "SD_RevSum_CDR": SD_RevSum_CDR,
              "finished": True}
    
    return result

def Capital_Profile(Basis):
    # Function to calculate a "Capital profile" given a basis
    # Used to provide an input to the CoC_RM function
    Profile = Basis / Basis[0]
    return Profile

def CoC_RM(Opening_Capital, Capital_Profile, CoC_Rate, Discount_Rate, Offset=0):
    # Function to calculate a cost-of-capital risk margin as a sum of discounted costs-of-capital
    # Assumes a constant discount rate
    fy = len(Capital_Profile)
    Capital = np.zeros(fy)
    CoC = np.zeros(fy)
    Disc_CoC = np.zeros(fy)
    
    t = np.arange(1, fy+1)
    Capital = Opening_Capital * Capital_Profile
    CoC = Capital * CoC_Rate
    Disc_CoC = CoC / ((1 + Discount_Rate) ** (t - 1 + min(Offset, 1)))
    RM = np.sum(Disc_CoC)
    
    result = {"Capital": Capital,
              "CoC": CoC,
              "Disc_CoC": Disc_CoC,
              "RM": RM}
    return result

## Risk Measures
def VAR(data,p):
  # Function to calculate the value of X at a given percentile p. Note that the value returned is 
  # such that there is a probability p of values being less than X. For example, if there are 100 simulations
  # and VAR(x, 0.5) is required, then the value of the 51st ordered simulation is returned, since 50% of values
  # are below this value. Think about whether it would be better to take -InvPercentile(-x,1-p)
  x=np.sort(data)
  return x[int(np.floor(len(x)*p+1))]

def TVAR(data,p):
  # Function to calculate Tail Value at Risk. If there are 100 simulations
  # and VAR(x, 0.5) is required, then the value of the average of ordered simulations 51 to 100
  # Think about whether the inpit is a profit or loss distribution (ie think about the sign)
  x= np.sort(data)
  return np.nanmean(x[x>=VAR(x,p)])

def PHT(data,param):
  # Function to calculate Wang's proportional hazards transform given parameter rho.
  # This method uses a weighted average of simulations.
  rho = 1/param
  x= np.sort(data)
  n = len(x)
  tSx = npNaN(n)
  wt = npNaN(n)

  tSx[0] = (1-(1/n))**rho
  wt[0] = 1-tSx[0]

  for i in range(1, n):
    tSx[i] = (1-((i+1)/n))**rho
    wt[i] = tSx[i-1] - tSx[i]

  return np.nansum(wt * x)  


## Functions for Mack's model

def Mack_ChainLadder(Triangle, Mask=None):
# Function to calculate analytic standard deviations of the forecasts (RMSEPs) using Mack's model 
# fitted as weighted Gaussian GLM then using recursive formulae from Appendix of England & Verrall (2002)
# for the prediction errors (RMSEPs).

    nc = len(Triangle[0])-1

    if Mask is None:
        Mask = np.ones((nc, nc))

    n_j = npNaN(nc)
    for j in range(nc):
        n_j[j] = np.sum(Mask[:nc-j,j])

    if (n_j < 1).any():
        print("Invalid data for Mack's model applied analytically: at least one ratio must be included at each development period")
        print("Proceed with bootstrapping instead")
        coefs=npNaN(nc)
        parameter_se=npNaN(nc)
        LinkRatios=npNaN(nc) 
        LinkRatiosSE=npNaN(nc) 
        sigma=npNaN(nc) 
        Latest=npNaN(nc)
        TotalLatest=np.nan
        Reserves=npNaN(nc)
        TotalReserves=np.nan
        Ultimates=npNaN(nc)
        TotalUltimates=None
        Reserves_SD=npNaN(nc)
        Reserves_CoV=npNaN(nc)
        TotalReserve_SD=np.nan
        TotalReserve_CoV=np.nan
    else:

        # calculate sigmas for pure chain ladder model
        LRTriangle = LR_Tri(Triangle)["Ratios"]
        LRWeights = LR_Tri(Triangle)["Weights"]
        CL_facs = CL_factors(LRTriangle, LRWeights, Mask) # used for starting values in GLM
        Mack_Resids = Mack_Residuals(Triangle, Mask)
        sigma = Mack_Resids["sigma"]

        # allow for exclusions
        LRWeights_tmp = LRWeights*Mask
        #LinkRatio_tmp = LRTriangle*LRWeights_tmp

        # create design matrix without intercept
        row_matrix = np.indices((nc,nc))[0] + 1
        col_matrix = np.indices((nc,nc))[1] + 1

        Row = row_matrix[~np.isnan(LRTriangle)]
        Column = col_matrix[~np.isnan(LRTriangle)]
        Ratio = LRTriangle[~np.isnan(LRTriangle)]
        Weights = LRWeights_tmp[~np.isnan(LRTriangle)]

        nr = len(Row)
        Design = np.zeros((nr,nc))
        for i in range(nr):
            for j in range(nc):
                if Column[i]==j+1:
                    Design[i,j]=1

        y_var = np.copy(Ratio)
        #coef_start = np.zeros(nc)
        coef_start = np.log(CL_facs)
        test = 1
        num_iter = 1
        error = "Converged"
        disp = sigma[Column-1]**2
        disp[disp==0] = 0.0000000001

        #start loop
        while test > 0.00000001:
            coefs = coef_start
            eta = np.matmul(Design, coefs)
            Lambda = np.exp(eta)
            deta_dmu = 1/Lambda
            z = eta + (y_var-Lambda) * deta_dmu
            V = 1 # variance function for Gaussian GLM
            W_vec = Weights/(deta_dmu**2*V*disp) # since sigmas are not constant, bring into Weight vector here so parameter_se are correct later
            W_mat = np.diag(W_vec)
            inv_XWX = np.linalg.inv(np.matmul(Design.T, np.matmul(W_mat, Design))) # inv(X_transpose*W*X)
            XWz = np.matmul(Design.T, np.matmul(W_mat, z))
            coefs = np.matmul(inv_XWX, XWz)
            # create difference between start and end params and check convergence, then repeat:
            test = np.sqrt(np.sum((coefs-coef_start)**2))
            coef_start = coefs
            num_iter = num_iter + 1
            if num_iter > 20:
                error = "Failure to converge"
                test = 0 #force a stop to prevent infinite loop

        #print(error, "in", num_iter, "iterations")

        # Calculate standard error of parameters allowing for over-dispersion
        parameter_se = np.sqrt(np.diag(inv_XWX))

        LinkRatios = np.exp(coefs)
        LinkRatiosSE = LinkRatios * parameter_se

        CL_Result = LinkRatioMethod_forecast(Triangle, LinkRatios)
        
        Recursive_SD_Result = Recursive_SD(Triangle, LinkRatiosSE, sigma, model='Mack', Mask=Mask)

        Latest = CL_Result["Latest"]
        Ultimates = CL_Result["Ultimates"]
        Reserves = CL_Result["Reserves"]
        TotalReserves = CL_Result["TotalReserves"]
        TotalLatest = sum(Latest)
        TotalUltimates = sum(Ultimates)

        Reserves_SD = Recursive_SD_Result["Reserves_SD"]
        Reserves_CoV = Recursive_SD_Result["Reserves_CoV"]
        TotalReserve_SD = Recursive_SD_Result["TotalReserve_SD"]
        TotalReserve_CoV = Recursive_SD_Result["TotalReserve_CoV"]

    result = {"coefs": coefs, 
              "parameter_se": parameter_se, 
              "LinkRatios": LinkRatios, 
              "LinkRatiosSE": LinkRatiosSE, 
              "sigma": sigma, 
              "Latest": Latest,
              "TotalLatest": TotalLatest,
              "Reserves": Reserves,
              "TotalReserves": TotalReserves,
              "Ultimates": Ultimates, 
              "TotalUltimates": TotalUltimates,
              "Reserves_SD": Reserves_SD, 
              "Reserves_CoV": Reserves_CoV, 
              "TotalReserve_SD": TotalReserve_SD, 
              "TotalReserve_CoV": TotalReserve_CoV}
    return result

def Recursive_SD(Triangle, LinkRatiosSE, sqrtScale, model="Mack", Mask=None):
    # Function for evaluating the recursive formulae from Appendix of England & Verrall (2002)
    # valid for models where the development factors are independent (ie no curve)
    if Mask is None:
        Mask = np.ones((len(Triangle[0])-1, len(Triangle[0])-1))
    
    LRs = LR_Tri(Triangle)
    LRTriangle = LRs["Ratios"]
    LRWeights = LRs["Weights"]
    CL_facs = CL_factors(LRTriangle, LRWeights, Mask=Mask)
    CL_Result = LinkRatioMethod_forecast(Triangle, CL_facs)
    Cum_CL_facs = np.cumprod(CL_facs[::-1])[::-1] # reverse cumulative product of reverse of dev factors

    nparams = len(CL_facs)
    #print(nparams)
    #print(Cum_CL_facs)
    var_factors = np.zeros(nparams)
    var_factors[nparams-1] = LinkRatiosSE[nparams-1]**2
    for i in range(nparams-2, -1, -1):
        var_factors[i] = CL_facs[i]**2*var_factors[i+1]+Cum_CL_facs[i+1]**2*LinkRatiosSE[i]**2+LinkRatiosSE[i]**2*var_factors[i+1]

    ReverseLatest = CL_Result["Latest"][1:][::-1] #exclude value for year 1 then reverse Latest vector
    Est_Error_tmp = ReverseLatest**2 * var_factors
    Est_Error_Cov = np.zeros((nparams,nparams))
    for i in range(nparams-1):
        for j in range(i+1, nparams):
            Est_Error_Cov[i][j] = ReverseLatest[j]*var_factors[j]*ReverseLatest[j-i-1]*np.prod(CL_facs[(j-i-1):j])

    Est_Err_OP = np.sqrt(np.concatenate(([0], Est_Error_tmp[::-1])))
    Est_Err_Total = np.sqrt(np.sum(Est_Error_tmp)+2*np.sum(Est_Error_Cov))

    dispersion = sqrtScale**2
    if model=='Mack':
        Process_Err_var = dispersion
    elif model=="NegBin":
        Process_Err_var = dispersion*CL_facs*(CL_facs-1)

    Cum_CL_facs_Squared = np.concatenate((Cum_CL_facs**2, [1]))
    Process_Err_tmp = np.zeros((nparams, nparams))
    for i in range(nparams):
        for j in range(nparams-i-1,nparams):
            Process_Err_tmp[i][j] = Process_Err_var[j] * CL_Result["CompleteForecast"][i+1][j] * Cum_CL_facs_Squared[j+1]

    Process_Err_OP = np.sqrt(np.concatenate(([0], np.sum(Process_Err_tmp, axis=1))))
    Process_Err_Total = np.sqrt(np.sum(Process_Err_tmp))


    Latest = CL_Result["Latest"]
    Reserves = CL_Result["Reserves"]
    Ultimates = CL_Result["Ultimates"]
    Reserves_SD = np.sqrt(Process_Err_OP**2 + Est_Err_OP**2)
    Reserves_CoV = abs(safe_divide(Reserves_SD, Reserves))
    TotalReserve = CL_Result["TotalReserves"]
    TotalReserve_SD = np.sqrt(Process_Err_Total**2 + Est_Err_Total**2)
    TotalReserve_CoV = abs(safe_divide(TotalReserve_SD, TotalReserve))

    result = {"Latest": Latest, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Ultimates": Ultimates, 
              "Reserves_SD": Reserves_SD, 
              "Reserves_CoV": Reserves_CoV, 
              "TotalReserve_SD": TotalReserve_SD, 
              "TotalReserve_CoV": TotalReserve_CoV}
    return result


def Mack_Residuals(tridata, Mask=None):
    # Function to calculate residuals and Mack's sigma associated with Mack's model.
    # Note that for an n by n triangle, there is an n-1 by n-1 set of residuals since
    # Mack's model is really a model of the link ratios
    n = len(tridata[0])-1
    if Mask is None:
        Mask = np.ones((n, n))
        
    unscaled_resids = npNaN((n,n))
    scaled_resids = npNaN((n,n))
    adj_unscaled_resids = npNaN((n,n))
    adj_scaled_resids = npNaN((n,n))
    scale_tmp = npNaN((n,n))
    zeroavg_adj_scaled_resids = npNaN((n,n))

    n_j = npNaN(n)
    bias = npNaN(n)
    sigma = npNaN(n)
    
    # fitted Link Ratio Method
    LRM = LinkRatioMethod(tridata, Mask)
    LRTriangle = LRM["LR_Triangle"]
    LRWeights = LRM["LR_Weights"]
    FittedFactors = LRM["LRM_Factors"]

    # set Mask=0 if LinkRatio=0 for counting n_j
    Mask_tmp = Mask.copy()
    Mask_tmp[LRTriangle==0] = 0

    # bias correction for each development period
    #(Needs modifying if curves are fitted to factors to allow for number of parameters)
    for j in range(n-1):
        n_j[j] = np.sum(Mask_tmp[:n-j,j])
        if n_j[j] <= 1:
            bias[j] = 0
        else:
            bias[j] = np.sqrt(n_j[j]/(n_j[j]-1))
    bias[n-1] = 1

    # first calculate unscaled residuals
    for i in range(n):
        for j in range(n):
            unscaled_resids[i][j] = np.sqrt(abs(Mask_tmp[i][j]*LRWeights[i][j]))*(LRTriangle[i][j]-FittedFactors[j])
            adj_unscaled_resids[i][j] = bias[j]*unscaled_resids[i][j]
            scale_tmp[i][j] = adj_unscaled_resids[i][j]**2

    # calulate sigma values as sqrt(average bias-adjusted unscaled residuals squared) in each development period
    for i in range(n-1):
        if n_j[i] <= 1:
            if i == 0:
                sigma[i] = 0
            else:
                sigma[i] = sigma[i-1]
        else:
            sigma[i] = np.sqrt((np.nansum([scale_tmp[j][i] for j in range(n-i)]))/n_j[i])
    # set final sigma value as minimum of previous two values
    sigma[n-1] = min(sigma[n-2],sigma[n-3])

    # set sigma to zero if cumulative dev factors are 1
    rev_cumprod = np.cumprod(FittedFactors[::-1])[::-1]
    sigma[rev_cumprod==1] = 0

    # calculate scaled residuals and bias-adjusted scaled residuals, 
    # then force mean to be zero by subtracting the average residual (usually a very small adjustment)
    for i in range(n):
        for j in range(n-i):
            scaled_resids[i][j] = safe_divide(unscaled_resids[i][j], sigma[j], np.nan)
            adj_scaled_resids[i][j] = safe_divide(adj_unscaled_resids[i][j], sigma[j], np.nan)

    avg_resid = np.nansum(adj_scaled_resids)/(np.nansum(n_j)-1)

    for i in range(n):
        for j in range(n-i):
            zeroavg_adj_scaled_resids[i][j] = adj_scaled_resids[i][j] - avg_resid

    unscaled_resids[unscaled_resids == 0] = float('nan')
    adj_unscaled_resids[np.isnan(unscaled_resids)] = float('nan')
    scaled_resids[np.isnan(unscaled_resids)] = float('nan')
    adj_scaled_resids[np.isnan(unscaled_resids)] = float('nan')
    zeroavg_adj_scaled_resids[np.isnan(unscaled_resids)] = float('nan')

    result = {"unscaled_resids": unscaled_resids,
              "adj_unscaled_resids": adj_unscaled_resids,
              "scaled_resids": scaled_resids, 
              "adj_scaled_resids": adj_scaled_resids,
              "zeroavg_adj_scaled_resids": zeroavg_adj_scaled_resids,
              "sigma": sigma,
              "sqrtScale": sigma,
              "avg_resid": avg_resid}

    return result

def Mack_pseudo_data_NP(resids, sigma, avgRatio, weight):
    # Function for calculating pseudo-ratios when bootstrapping Mack's model using
    # non-parametric bootstrapping
    nc = len(resids[0])-1

    pseudo_ratios = npNaN((nc,nc))
    for i in range(nc):
        for j in range(nc-i):
            pseudo_ratios[i][j] = resids[i][j] * (sigma[j]/np.sqrt(abs(weight[i][j])))+avgRatio[j]

    return pseudo_ratios

def Mack_pseudo_data_Gamma(sigma, avgRatio, weight, **kwargsData):
    # Function for calculating pseudo-ratios when bootstrapping Mack's model using
    # parametric bootstrapping with a Gamma distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(weight[0])

    pseudo_ratios = npNaN((nc,nc))

    tol = 1e-12
    for i in range(nc):
        for j in range(nc-i):
            Mean = avgRatio[j]
            SD = safe_divide(sigma[j], np.sqrt(abs(weight[i][j])))
            if Mean > tol:
                if SD < tol:
                    pseudo_ratios[i][j] = Mean
                else:
                    scale = (SD**2)/Mean
                    shape = Mean/scale
                    pseudo_ratios[i][j] = np.random.gamma(shape=shape, scale=scale)
            else:
                pseudo_ratios[i][j] = np.random.normal(loc=Mean, scale=SD)

    return pseudo_ratios

def Mack_pseudo_data_Lognormal(sigma, avgRatio, weight, **kwargsData):
    # Function for calculating pseudo-ratios when bootstrapping Mack's model using
    # parametric bootstrapping with a Lognormal distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative,
    nc = len(weight[0])

    pseudo_ratios = npNaN((nc,nc))

    tol = 1e-12
    for i in range(nc):
        for j in range(nc-i):
            Mean = avgRatio[j]
            SD = safe_divide(sigma[j], np.sqrt(abs(weight[i][j])))
            if Mean > tol:
                sigma_normal = np.sqrt(np.log(1 + (SD/Mean)**2))
                mean_normal = np.log(Mean) - 0.5 * sigma_normal**2
                pseudo_ratios[i][j] = np.random.lognormal(mean=mean_normal, sigma=sigma_normal)
            else:
                pseudo_ratios[i][j] = np.random.normal(loc=Mean, scale=SD)

    return pseudo_ratios

def Mack_Bstrap_Forecast_NP(tridata, factors, resids, sigma, **kwargs):
    # Function for calculating cumulative forecasts when bootstrapping Mack's model using a non-parametric approach
    nc = len(tridata[0])

    cumulatives = npNaN(tridata.shape)

    for i in range(nc):
        cumulatives[i, nc-i-1] = tridata[i, nc-i-1]
        for j in range(nc-i, nc):
            cumulatives[i, j] = cumulatives[i, j-1]*factors[j-1]+resids[i, j]*sigma[j-1]*(np.sqrt(abs(cumulatives[i, j-1])))
        cumulatives[i, nc-i-1] = np.nan


    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)

    Ultimates = complete_forecast[:,-1]
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest

    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}
    
    return result


def Mack_Forecast_Gamma(tridata, factors, sigma, **kwargs):
    # Function for calculating cumulative forecasts when bootstrapping Mack's model using a parametric approach and
    # a Gamma distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(tridata[0])

    cumulatives = npNaN(tridata.shape)

    tol = 1e-12

    for i in range(nc):

        cumulatives[i,nc-i-1] = tridata[i,nc-i-1]

        for j in range(nc-i, nc):
            Mean = cumulatives[i,j-1]*factors[j-1]
            SD = sigma[j-1] * np.sqrt(abs(cumulatives[i,j-1]))

            if Mean > tol:
                if SD < tol:
                    cumulatives[i,j] = Mean
                else:
                    scale = (SD**2)/Mean
                    shape = Mean/scale
                    cumulatives[i,j] = np.random.gamma(shape=shape, scale=scale)

            else:
                cumulatives[i,j] = np.random.normal(loc=Mean, scale=SD)

        cumulatives[i, nc-i-1] = np.nan

    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)

    Ultimates = complete_forecast[:,-1]
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest
    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}
    
    return result

def Mack_Forecast_Lognormal(tridata, factors, sigma, **kwargs):
    # Function for calculating cumulative forecasts when bootstrapping Mack's model using a parametric approach and
    # a Lognormal distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(tridata[0])

    cumulatives = npNaN(tridata.shape)

    tol = 1e-12

    for i in range(nc):

        cumulatives[i,nc-i-1] = tridata[i,nc-i-1]

        for j in range(nc-i, nc):
            Mean = cumulatives[i,j-1]*factors[j-1]
            SD = sigma[j-1] * np.sqrt(abs(cumulatives[i,j-1]))

            if Mean > tol:
                sigma_normal = np.sqrt(np.log(1 + (SD/Mean)**2))
                mean_normal = np.log(Mean) - 0.5 * sigma_normal**2
                cumulatives[i,j] = np.random.lognormal(mean=mean_normal, sigma=sigma_normal)

            else:
                cumulatives[i,j] = np.random.normal(loc=Mean, scale=SD)

        cumulatives[i, nc-i-1] = np.nan

    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)

    Ultimates = complete_forecast[:,-1]
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest
    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}
    
    return result


def Main_Mack_Bstrap(tridata, Mask=None, iterations=1000, seed = np.random.randint(1, 1000000),
                      BootstrapDist="NonParametric", ForecastDist="Gamma", UserSigma=None):
    # Main function for bootstrapping Mack's model given input settings.
    if Mask is None:
        Mask = np.ones((len(tridata[0])-1, len(tridata[0])-1))
    
    np.random.seed(seed)

    LRM = LinkRatioMethod(tridata, Mask)
    LRWeights = LRM["LR_Weights"]
    CL_facs = LRM["LRM_Factors"]
    CL_Result = LinkRatioMethod_forecast(tridata, CL_facs)
    Mack_Resids =  Mack_Residuals(tridata, Mask)

    nc = len(CL_facs)
    ZeroAvg_Mack_Resids = Mack_Resids["zeroavg_adj_scaled_resids"]
    sigma = Mack_Resids["sigma"]

    Pseudo_LRs = npNaN((iterations, nc))
    Reserves = npNaN((iterations, nc+1))
    Ultimates = npNaN((iterations, nc+1))
    TotalReserve = npNaN(iterations)
    Cumulatives = npNaN((iterations, nc+1, nc+1))
    Complete_Cumulatives = npNaN((iterations, nc+1, nc+1))

    ResidsVector = []
    for i in range(len(ZeroAvg_Mack_Resids)):
        ResidsVector = [*ResidsVector, *ZeroAvg_Mack_Resids[i]]

    ResidsVector = [x for x in ResidsVector if (np.isnan(float(x)) == False)]

    ResidsVector.sort()

    if BootstrapDist=="Gamma":
        Mack_pseudo_data = Mack_pseudo_data_Gamma
    elif BootstrapDist=="Lognormal":
        Mack_pseudo_data = Mack_pseudo_data_Lognormal
    else:
        Mack_pseudo_data = Mack_pseudo_data_NP

    if ForecastDist=="Gamma":
        Mack_Bstrap_forecast = Mack_Forecast_Gamma
    elif ForecastDist=="Lognormal":
        Mack_Bstrap_forecast = Mack_Forecast_Lognormal
    else:
        Mack_Bstrap_forecast = Mack_Bstrap_Forecast_NP

    if UserSigma is None:
        ForecastSigma = sigma
    else:
        ForecastSigma = UserSigma

    kwargsData = {'sigma': sigma, 'avgRatio': CL_facs, 'weight': LRWeights}
    kwargs = {'tridata': tridata, 'sigma': ForecastSigma}

    for i in range(iterations):
        Resampled_Resids = Resample_Resids(ResidsVector, nc+1)

        kwargsData.update({'resids': Resampled_Resids})

        pseudo_ratios = Mack_pseudo_data(**kwargsData)
        Pseudo_LRs[i] = LinkRatioMethod(tridata, Mask=Mask, pseudoLRs=pseudo_ratios)["LRM_Factors"] # gives chain ladder factors with exclusions
        
        kwargs.update({'factors': Pseudo_LRs[i], 'resids': Resampled_Resids})

        Forecast = Mack_Bstrap_forecast(**kwargs)

        Reserves[i] = Forecast["Reserves"]
        Ultimates[i] = Forecast["Ultimates"]
        TotalReserve[i] = Forecast["TotalReserve"]
        Cumulatives[i] = Forecast["Cumulatives"]
        Complete_Cumulatives[i] = Forecast["Complete_Forecast"]

    Avg_Reserve = np.nanmean(Reserves, axis = 0)
    SD_Reserve = np.nanstd(Reserves, axis = 0)

    CoV_Reserve = abs(safe_divide(SD_Reserve, Avg_Reserve))

    Avg_TotalReserve = np.nanmean(TotalReserve)

    SD_TotalReserve = np.nanstd(TotalReserve)

    CoV_TotalReserve = abs(safe_divide(SD_TotalReserve, Avg_TotalReserve))

    Latest = CL_Result["Latest"]
    CL_Ultimates = CL_Result["Ultimates"]
    CL_Reserves = CL_Result["Reserves"]
    CL_Cumulatives = CL_Result["Cumulatives"]

    finished = True

    result = {"CL_facs": CL_facs, 
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
              "iterations": iterations, 
              "finished": finished,

              "Complete_Cumulatives": Complete_Cumulatives}

    return result


## Functions for the Over-dispersed Poisson model

def ODP_ChainLadder(Triangle, Scale='NonConstant', Mask=None):
# Function to calculate analytic standard deviations of the forecasts (RMSEPs) using the 
# Over-dispersed Poisson model fitted as a GLM. 
# Note: ODP GLM reserves will not match vol-wtd chain ladder reserves when a Mask with zero values is used.
    
    nc = len(Triangle)
    
    if Mask is None:
        Mask = np.ones((nc-1, nc-1))
    
    Indicators = np.ones((nc,nc))
    # construct indicators from link ratio Mask using same method as for residuals
    # By convention, if ratios between development periods 1-2 are excluded, then
    # both observations at development periods 1 and 2 are excluded, otherwise
    # just the observation associated with the position of the numerator of the link ratio is excluded

    for i in range(nc-1):
        for j in range(nc-i):
            if j<=1:
                Indicators[i,j]=Mask[i,0]
            else:
                Indicators[i,j]=Mask[i,j-1]

    row_matrix = np.indices((nc,nc))[0] + 1
    col_matrix = np.indices((nc,nc))[1] + 1

    # create data frame for glm function
    Inc_Triangle = Incrementals(Triangle)

    # Test for sum(incrementals) by dev period <= 0, which implies a development factor <= 1,
    # which invalidates the ODP model
    Inc_Triangle_tmp = Inc_Triangle * Indicators
    col_sums = np.nansum(Inc_Triangle_tmp, axis=0)
    if (col_sums <= 0).any():
        print("Invalid data for ODP model applied analytically: Dev factor less than or equal to one detected")
        print("Proceed with bootstrapping instead")
        coefs=npNaN(2*nc-1)
        parameter_se=npNaN(2*nc-1)
        glmScale=npNaN(nc)
        Latest=npNaN(nc)
        TotalLatest=np.nan
        Reserves=npNaN(nc)
        TotalReserves=np.nan
        Ultimates=npNaN(nc)
        TotalUltimates=None
        Reserves_SD=npNaN(nc)
        Reserves_CoV=npNaN(nc)
        TotalReserve_SD=np.nan
        TotalReserve_CoV=np.nan
        Forecasts=None
        full_design=None
        past_design=None
        future_design=None
    else:
        Inc_Vector = Inc_Triangle.flatten()
        Indicators_Vector = Indicators.flatten()
        Row = row_matrix.flatten()
        Column = col_matrix.flatten()
        Weights = np.where(~np.isnan(Inc_Vector),1,0)
        Weights = Weights * Indicators_Vector

        glmData = pd.DataFrame({'y': Inc_Vector, 'row': Row, 'col': Column, 'weights': Weights})
        # replace nans with 0 so full design matrix is available later
        glmData['y'] = glmData['y'].fillna(0)

        model = smf.glm(formula='y~C(row)+C(col)', data=glmData, family=sm.families.Poisson(),freq_weights=glmData['weights'])
        result = model.fit()
        #print(result.summary())

        # obtain coefficents and fitted values
        coefs = result.params
        fv = result.fittedvalues
        fv_past = fv[~np.isnan(Inc_Vector)]
        fv_fut = fv[np.isnan(Inc_Vector)]

        # obtain design matrix for past and future observations
        full_design = model.exog
        past_design = full_design[~np.isnan(Inc_Vector)]
        future_design = full_design[np.isnan(Inc_Vector)]

        # obtain scaled covariance matrix of parameter estimates
        ODPRes = ODP_Residuals(Triangle, Scale=Scale, Mask=Mask)
        glmScale = ODPRes["sqrtScale"]**2
        disp = np.ones(len(fv_past))
        disp = glmScale[Column[~np.isnan(Inc_Vector)]-1]
        disp[disp==0] = 0.0000000001
        Wmat = np.diag(fv_past/disp)
        Sigma = np.linalg.inv(np.matmul(past_design.T, np.matmul(Wmat, past_design))) # inv(X_transpose*W*X)

        # scaled standard error of parameters
        parameter_se = np.sqrt(np.diag(Sigma))

        # Calculate prediction errors for future claims and reserves
        diag_fv_fut = np.diag(fv_fut)
        disp_fut = glmScale[Column[np.isnan(Inc_Vector)]-1]
        diag_disp_fut = np.diag(disp_fut)
        cov1 = np.matmul(future_design,np.matmul(Sigma,future_design.T)) # covariance matrix of linear predictors
        cov2 = np.matmul(diag_fv_fut,np.matmul(cov1,diag_fv_fut)) # covariance matrix of fitted values (for estimation error)
        cov3 = cov2 + diag_disp_fut * diag_fv_fut # adding process error along diagonal component
        # origin period prediction variances are then just the appropriate sum of elements
        # take square root for prediction errors (RMSEPs)

        Latest = np.diag(np.fliplr(Triangle))
        TotalLatest = sum(Latest)
        Reserves = Rowsum(diag_fv_fut, nc)[:-1]
        TotalReserves = sum(Reserves)
        Ultimates = Latest + Reserves
        TotalUltimates = sum(Ultimates)
        Reserves_SD_tmp = np.sqrt(Rowsum(cov3, nc))
        Reserves_SD = Reserves_SD_tmp[:-1]
        Reserves_CoV = abs(safe_divide(Reserves_SD, Reserves))
        TotalReserve_SD = Reserves_SD_tmp[-1]
        TotalReserve_CoV = abs(safe_divide(TotalReserve_SD, TotalReserves))
        Forecasts = fv_fut

        if (Mask == 0).any():
            print("Note: ODP reserves will not match vol-wtd chain ladder reserves when a Mask with zero values is used")
        
    result = {"coefs": coefs, 
              "parameter_se": parameter_se,
              "glmScale": glmScale,
              "Latest": Latest,
              "TotalLatest": TotalLatest,
              "Reserves": Reserves,
              "TotalReserves": TotalReserves,
              "Ultimates": Ultimates,
              "TotalUltimates": TotalUltimates,
              "Reserves_SD": Reserves_SD, 
              "Reserves_CoV": Reserves_CoV, 
              "TotalReserve_SD": TotalReserve_SD,
              "TotalReserve_CoV": TotalReserve_CoV,
              "Forecasts": Forecasts,
              "Full_Design": full_design,
              "Past_Design": past_design,
              "Future_Design": future_design}
    return result

def ODP_Residuals(tridata, Scale="NonConstant", Mask=None):
    # Function to calculate residuals and the square root of the scale parameters (sqrtScale) associated with the
    # ODP model.

    n = len(tridata[0])
    if Mask is None:
        Mask = np.ones((n-1, n-1))
    
    n_j = npNaN(n)
    Indicators = np.ones((n,n))
    obs_incremental=npNaN((n,n))
    fitted_cumulative=npNaN((n,n))
    fitted_incremental=npNaN((n,n))
    unscaled_resids = npNaN((n,n))
    scaled_resids = npNaN((n,n))
    adj_unscaled_resids = npNaN((n,n))
    adj_scaled_resids = npNaN((n,n))
    zeroavg_adj_scaled_resids = npNaN((n,n))
    scale_tmp = npNaN((n,n))
    sqrtScale = npNaN(n)
    
    # fitted Link Ratio Method
    LRM = LinkRatioMethod(tridata, Mask)
    LRTriangle = LRM["LR_Triangle"]
    #LRWeights = LRM["LR_Weights"]
    FittedFactors = LRM["LRM_Factors"]

    # set Mask=0 if LinkRatio=0 for counting n_j
    Mask_tmp = Mask.copy()
    Mask_tmp[LRTriangle==0] = 0

    for i in range(n-1):
        fitted_cumulative[i][n-i-1]=tridata[i][n-i-1]
        for j in range(n-i-2,-1,-1):
            fitted_cumulative[i][j]=fitted_cumulative[i][j+1]/FittedFactors[j]

    fitted_cumulative[n-1,0]=tridata[n-1,0]

    fitted_incremental=Incrementals(fitted_cumulative)
    obs_incremental=Incrementals(tridata)

    # construct residual indicators from link ratio Mask
    # By convention, if ratios between development periods 1-2 are excluded, then
    # both ODP residuals at development periods 1 and 2 are excluded, otherwise
    # just the residual associated with the position of the numerator of the link ratio is excluded

    for i in range(n-1):
        for j in range(n-i):
            if j<=1:
                Indicators[i,j]=Mask_tmp[i,0]
            else:
                Indicators[i,j]=Mask_tmp[i,j-1]

    for j in range(n):
        n_j[j]=np.nansum(Indicators[:n-j,j])
    
    # bias correction factor
    #(Needs modifying if curves are fitted to factors to allow for number of parameters)
    n_obs=np.sum(n_j)
    n_params=2*n-1
    bias=np.sqrt(n_obs/(n_obs-n_params))

    for i in range(n):
        for j in range(0,n-i):
            if (fitted_incremental[i,j]==0):
                unscaled_resids[i][j]=0
            else:
                unscaled_resids[i][j]=Indicators[i][j]*(obs_incremental[i][j]-fitted_incremental[i][j])/np.sqrt(abs(fitted_incremental[i][j]))
            adj_unscaled_resids[i][j]=bias*unscaled_resids[i][j]
            scale_tmp[i][j] = adj_unscaled_resids[i][j]**2

    # exclude zero residuals
    unscaled_resids[unscaled_resids == 0] = float('nan')
    adj_unscaled_resids[np.isnan(unscaled_resids)] = float('nan')

    if Scale=="Constant":
        sqrtScale=np.repeat(np.sqrt(np.nansum(scale_tmp)/n_obs),n)
    else:
        for i in range(n-1):
            if n_j[i] <= 1:
                if i == 0:
                    sqrtScale[i] = 0
                else:
                    sqrtScale[i] = sqrtScale[i-1]
            else:
                sqrtScale[i] = np.sqrt((np.nansum([scale_tmp[j][i] for j in range(n-i)]))/(n_j[i]))
        sqrtScale[n-1] = min(sqrtScale[n-2],sqrtScale[n-3])
    
    # set sqrtScale to zero if cumulative dev factors are 1
    rev_cumprod = np.cumprod(FittedFactors[::-1])[::-1]
    for i in range(n):
        if i==0:
            if (rev_cumprod[0]==1):
                sqrtScale[0] = 0
        else:
            if (rev_cumprod[i-1]==1):
                sqrtScale[i] = 0

    for i in range(n):
        for j in range(n-i):
            scaled_resids[i][j] = safe_divide(unscaled_resids[i][j], sqrtScale[j], np.nan)
            adj_scaled_resids[i][j] = safe_divide(adj_unscaled_resids[i][j], sqrtScale[j], np.nan)

    # exclude zero residuals from calculation of average
    avg_resid = np.nansum(adj_scaled_resids)/(n_obs-2)

    for i in range(n):
        for j in range(n-i):
            zeroavg_adj_scaled_resids[i][j] = adj_scaled_resids[i][j] - avg_resid

    zeroavg_adj_scaled_resids[0,n-1] = float('nan')
    zeroavg_adj_scaled_resids[n-1,0] = float('nan')

    result = {"unscaled_resids": unscaled_resids,
              "adj_unscaled_resids": adj_unscaled_resids,
              "scaled_resids": scaled_resids, 
              "adj_scaled_resids": adj_scaled_resids,
              "zeroavg_adj_scaled_resids": zeroavg_adj_scaled_resids, 
              "sqrtScale": sqrtScale, 
              "avg_resid": avg_resid,
              "fitted_cumulative": fitted_cumulative, 
              "fitted_incremental": fitted_incremental}

    return result

def ODP_pseudo_data_NP(resids, sqrtScale, Fitted_Incremental):
    # Function to calculate pseudo incrementals when bootstrapping the ODP model using
    # a non-parametric approach. Values returned could be negative, which is a disadvantage of the non-parametric approach.
    nc = len(resids[0])
    pseudo_incrementals = npNaN((nc,nc))
    for i in range(nc):
        for j in range(nc-i):
            pseudo_incrementals[i][j] = resids[i][j] * sqrtScale[j]*np.sqrt(abs(Fitted_Incremental[i][j]))+Fitted_Incremental[i][j]
    return pseudo_incrementals

def ODP_pseudo_data_Gamma(sqrtScale, Fitted_Incremental, **kwargsData):
    # Function to calculate pseudo incrementals when bootstrapping the ODP model using
    # parametric bootstrapping with a Gamma distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(Fitted_Incremental[0])
    pseudo_incrementals = npNaN((nc,nc))

    tol = 1e-12

    for i in range(nc):
        for j in range(nc-i):
            Mean = Fitted_Incremental[i][j]
            SD = sqrtScale[j]*np.sqrt(abs(Mean))
            if Mean > tol:
                if SD < tol:
                    pseudo_incrementals[i][j] = Mean
                else:
                    scale = (SD**2)/Mean
                    shape = Mean/scale
                    pseudo_incrementals[i][j] = np.random.gamma(shape=shape, scale=scale)
            else:
                pseudo_incrementals[i][j] = np.random.normal(loc=Mean, scale=SD)
    return pseudo_incrementals

def ODP_pseudo_data_Lognormal(sqrtScale, Fitted_Incremental, **kwargsData):
    # Function to calculate pseudo incrementals when bootstrapping the ODP model using
    # parametric bootstrapping with a Lognormal distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(Fitted_Incremental[0])
    pseudo_incrementals = npNaN((nc,nc))

    tol = 1e-12

    for i in range(nc):
        for j in range(nc-i):
            Mean = Fitted_Incremental[i][j]
            SD = sqrtScale[j]*np.sqrt(abs(Mean))
            if Mean > tol:
                sigma_normal = np.sqrt(np.log(1 + (SD/Mean)**2))
                mean_normal = np.log(Mean) - 0.5 * sigma_normal**2
                pseudo_incrementals[i][j] = np.random.lognormal(mean=mean_normal, sigma=sigma_normal)
            else:
                pseudo_incrementals[i][j] = np.random.normal(loc=Mean, scale=SD)
    return pseudo_incrementals

def ODP_Bstrap_forecast_NP(tridata, factors, resids, Pseudo_Cumulatives, sqrtScale, **kwargs):
  # Simulates Incrementals using a non-parametric approach. Values returned could be negative, which 
  # is a disadvantage of the non-parametric approach.
  # Values could be censored at a small positive number, but this could result in a bias.
    nc = len(tridata[0])
    incrementals = npNaN(tridata.shape)
    cumulatives=np.copy(tridata)

    for i in range(1,nc):
        for j in range(nc-i,nc):
            Pseudo_Cumulatives[i, j] = Pseudo_Cumulatives[i,j-1]*factors[j-1]
            incrementals[i,j] = Pseudo_Cumulatives[i,j] - Pseudo_Cumulatives[i,j-1]
            incrementals[i,j] = resids[i,j] * sqrtScale[j] * np.sqrt(abs(incrementals[i,j])) + incrementals[i,j]
            cumulatives[i,j] = cumulatives[i,j-1] + incrementals[i,j]

    complete_forecast = cumulatives

    Ultimates = complete_forecast[:,-1]
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest
    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}

    
    return result

def ODP_Bstrap_Forecast_Gamma(tridata, factors, Pseudo_Cumulatives, sqrtScale, **kwargs):
    # Function for calculating incremental forecasts when bootstrapping the ODP model using a parametric approach and
    # a Gamma distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(tridata[0])
    incrementals = npNaN(tridata.shape)
    cumulatives=np.copy(tridata)
    tol = 1e-12

    for i in range(1,nc):
        for j in range(nc-i,nc):
            Pseudo_Cumulatives[i, j] = Pseudo_Cumulatives[i,j-1]*factors[j-1]
            incrementals[i,j] = Pseudo_Cumulatives[i,j] - Pseudo_Cumulatives[i,j-1]
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
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest
    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}

    
    return result

def ODP_Bstrap_Forecast_Lognormal(tridata, factors, Pseudo_Cumulatives, sqrtScale, **kwargs):
    # Function for calculating incremental forecasts when bootstrapping the ODP model using a parametric approach and
    # a Lognormal distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(tridata[0])
    incrementals = npNaN(tridata.shape)
    cumulatives=np.copy(tridata)
    tol = 1e-12

    for i in range(1,nc):
        for j in range(nc-i,nc):
            Pseudo_Cumulatives[i, j] = Pseudo_Cumulatives[i,j-1]*factors[j-1]
            incrementals[i,j] = Pseudo_Cumulatives[i,j] - Pseudo_Cumulatives[i,j-1]
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
    Latest = np.diag(np.fliplr(tridata))
    Reserves = Ultimates - Latest
    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}

    
    return result


def Main_ODP_Bstrap(tridata, Mask=None, iterations=1000, seed = np.random.randint(1, 1000000), Scale="NonConstant", 
                    BootstrapDist="NonParametric", ForecastDist="Gamma", UserSqrtScale=None):
    # Main function for bootstrapping the ODP model given input settings.

    np.random.seed(seed)

    n=len(tridata[0])
    nc=n-1
    if Mask is None:
        Mask = np.ones((nc, nc))
    
    LRM = LinkRatioMethod(tridata, Mask)
    CL_facs = LRM["LRM_Factors"]
    CL_Result = LinkRatioMethod_forecast(tridata, CL_facs)
    ODP_Resids =  ODP_Residuals(tridata, Scale, Mask)

    Pseudo_Incrementals = npNaN((n,n))
    Pseudo_Cumulatives = npNaN((n,n))
    Pseudo_LRs = npNaN((iterations, nc))
    Reserves = npNaN((iterations, nc+1))
    Ultimates = npNaN((iterations, nc+1))
    TotalReserve = npNaN(iterations)
    Cumulatives = npNaN((iterations, nc+1, nc+1))
    Complete_Cumulatives = npNaN((iterations, nc+1, nc+1))

    ZeroAvg_Resids=ODP_Resids["zeroavg_adj_scaled_resids"]
    ResidsVector = np.reshape(ZeroAvg_Resids,-1)
    ResidsVector = ResidsVector[~np.isnan(ResidsVector)]
    ResidsVector.sort()
    sqrtScale = ODP_Resids["sqrtScale"]
    Fitted_Incremental = ODP_Resids["fitted_incremental"]

    if BootstrapDist=="Gamma":
        ODP_pseudo_data = ODP_pseudo_data_Gamma 
    elif BootstrapDist=="Lognormal":
        ODP_pseudo_data = ODP_pseudo_data_Lognormal
    else:
        ODP_pseudo_data = ODP_pseudo_data_NP

    if ForecastDist=="Gamma":
        Bstrap_forecast = ODP_Bstrap_Forecast_Gamma
    elif ForecastDist=="Lognormal":
        Bstrap_forecast = ODP_Bstrap_Forecast_Lognormal
    else:
        Bstrap_forecast = ODP_Bstrap_forecast_NP

    if UserSqrtScale is None:
        ForecastSqrtScale = sqrtScale
    else:
        ForecastSqrtScale = UserSqrtScale

    kwargsData = {'Fitted_Incremental': Fitted_Incremental, 'sqrtScale': sqrtScale}
    kwargs = {'tridata': tridata, 'sqrtScale': ForecastSqrtScale}

    for i in range(iterations):
        Resampled_Resids = Resample_Resids(ResidsVector, n)
        
        kwargsData.update({'resids': Resampled_Resids})

        Pseudo_Incrementals = ODP_pseudo_data(**kwargsData)
        Pseudo_Cumulatives = np.cumsum(Pseudo_Incrementals, axis=1)
        Pseudo_LRs[i] = LinkRatioMethod(Pseudo_Cumulatives, Mask=Mask)["LRM_Factors"] # gives chain ladder factors with exclusions

        kwargs.update({'factors': Pseudo_LRs[i], 'resids': Resampled_Resids, 'Pseudo_Cumulatives': Pseudo_Cumulatives})

        Forecast = Bstrap_forecast(**kwargs)

        Reserves[i] = Forecast["Reserves"]
        Ultimates[i] = Forecast["Ultimates"]
        TotalReserve[i] = Forecast["TotalReserve"]
        Cumulatives[i] = Forecast["Cumulatives"]
        Complete_Cumulatives[i] = Forecast["Complete_Forecast"]

    Avg_Reserve = np.nanmean(Reserves, axis = 0)
    SD_Reserve = np.nanstd(Reserves, axis = 0)

    CoV_Reserve = abs(safe_divide(SD_Reserve, Avg_Reserve))
    
    Avg_TotalReserve = np.nanmean(TotalReserve)

    SD_TotalReserve = np.nanstd(TotalReserve)

    CoV_TotalReserve = abs(safe_divide(SD_TotalReserve, Avg_TotalReserve))

    Latest = CL_Result["Latest"]
    CL_Ultimates = CL_Result["Ultimates"]
    CL_Reserves = CL_Result["Reserves"]
    CL_Cumulatives = CL_Result["Cumulatives"]

    finished = True

    result = {"CL_facs": CL_facs, 
              "Latest": Latest, 
              "CL_Reserves": CL_Reserves, 
              "CL_Ultimates": CL_Ultimates, 
              "CL_Cumulatives": CL_Cumulatives, 
              "ODP_Resids": ODP_Resids, 
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
              "iterations": iterations, 
              "finished": finished, 
              
              "Complete_Cumulatives": Complete_Cumulatives}

    return result


## Functions for the Over-dispersed Negative Binomial model

def NegBin_Residuals(tridata, Scale="NonConstant", Mask=None):
    # Function to calculate residuals and the square root of the GLM scale parameters (sqrtScale) associated with 
    # the over-dispersed Negative Binomial model.
    # Note that for an n by n triangle, there is an n-1 by n-1 set of residuals since
    # the Negative Binomial model is a model of the link ratios.

    n = len(tridata[0])-1
    if Mask is None:
        Mask = np.ones((n, n))

    unscaled_resids = npNaN((n,n))
    scaled_resids = npNaN((n,n))
    adj_unscaled_resids = npNaN((n,n))
    adj_scaled_resids = npNaN((n,n))
    scale_tmp = npNaN((n,n))
    zeroavg_adj_scaled_resids = npNaN((n,n))

    n_j = npNaN(n)
    sqrtScale = npNaN(n)
    
    # fitted Link Ratio Method
    LRM = LinkRatioMethod(tridata, Mask)
    LRTriangle = LRM["LR_Triangle"]
    LRWeights = LRM["LR_Weights"]
    FittedFactors = LRM["LRM_Factors"]

    # set Mask=0 if LinkRatio=0 for counting n_j
    Mask_tmp = Mask.copy()
    Mask_tmp[LRTriangle==0] = 0

    for j in range(n):
        n_j[j] = np.nansum(Mask_tmp[:n-j,j])

    # bias correction factor
    #(Needs modifying if curves are fitted to factors to allow for number of parameters)
    num_obs = np.nansum(n_j)
    n_params = n
    bias = np.sqrt(num_obs/(num_obs-n_params))

    for i in range(n):
        for j in range(n):
            unscaled_resids[i][j] = safe_divide(np.sqrt(abs(Mask_tmp[i][j]*LRWeights[i][j]))*(LRTriangle[i][j]-FittedFactors[j]), np.sqrt(abs(FittedFactors[j]*(FittedFactors[j]-1))), np.nan)
            adj_unscaled_resids[i][j] = bias*unscaled_resids[i][j]
            scale_tmp[i][j] = adj_unscaled_resids[i][j]**2

    # exclude zero residuals
    unscaled_resids[unscaled_resids == 0] = float('nan')
    adj_unscaled_resids[np.isnan(unscaled_resids)] = float('nan')

    if Scale=="Constant":
        sqrtScale = np.repeat(np.sqrt(np.nansum(scale_tmp)/num_obs),n)
    elif Scale=="NonConstant":
        for i in range(n-1):
            if n_j[i] <= 1:
                if i == 0:
                    sqrtScale[i] = 0
                else:
                    sqrtScale[i] = sqrtScale[i-1]
            else:
                sqrtScale[i] = np.sqrt((np.nansum([scale_tmp[j][i] for j in range(n-i)]))/n_j[i])
        sqrtScale[n-1] = min(sqrtScale[n-2],sqrtScale[n-3])

    # set sqrtScale to zero if cumulative dev factors are 1
    rev_cumprod = np.cumprod(FittedFactors[::-1])[::-1]
    sqrtScale[rev_cumprod==1] = 0

    for i in range(n):
        for j in range(n-i):
            scaled_resids[i][j] = safe_divide(unscaled_resids[i][j], sqrtScale[j], np.nan)
            adj_scaled_resids[i][j] = safe_divide(adj_unscaled_resids[i][j], sqrtScale[j], np.nan)

    avg_resid = np.nansum(adj_scaled_resids)/(num_obs-1)

    for i in range(n):
        for j in range(n-i):
            zeroavg_adj_scaled_resids[i][j] = adj_scaled_resids[i][j] - avg_resid

    result = {"unscaled_resids": unscaled_resids,
              "adj_unscaled_resids": adj_unscaled_resids,
              "scaled_resids": scaled_resids, 
              "adj_scaled_resids": adj_scaled_resids,
              "zeroavg_adj_scaled_resids": zeroavg_adj_scaled_resids,
              "sqrtScale": sqrtScale,
              "avg_resid": avg_resid}

    return result


def NegBin_ChainLadder(Triangle, Scale="NonConstant", Mask=None):
# Function to calculate analytic standard deviations of the forecasts (RMSEPs) using the Negative Binomial model 
# fitted as a GLM then using recursive formulae from Appendix of England & Verrall (2002)
# for the prediction errors (RMSEPs).

    nc = len(Triangle[0])-1

    if Mask is None:
        Mask = np.ones((nc, nc))

    # calculate sigmas for pure chain ladder model
    LRTriangle = LR_Tri(Triangle)["Ratios"]
    LRWeights = LR_Tri(Triangle)["Weights"]
    CL_facs = CL_factors(LRTriangle, LRWeights, Mask)

    if (CL_facs <= 1).any():
        print("Invalid data for NegBin model applied analytically: Dev factor less than or equal to one detected")
        print("Proceed with bootstrapping instead")
        coefs=npNaN(nc)
        parameter_se=npNaN(nc)
        LinkRatios=npNaN(nc) 
        LinkRatiosSE=npNaN(nc) 
        sigma=npNaN(nc) 
        Latest=npNaN(nc)
        TotalLatest=np.nan
        Reserves=npNaN(nc)
        TotalReserves=np.nan
        Ultimates=npNaN(nc)
        TotalUltimates=None
        Reserves_SD=npNaN(nc)
        Reserves_CoV=npNaN(nc)
        TotalReserve_SD=np.nan
        TotalReserve_CoV=np.nan
    else:
        NegBin_Resids = NegBin_Residuals(Triangle, Scale=Scale, Mask=Mask)
        sigma = NegBin_Resids["sqrtScale"]

        # allow for exclusions
        LRWeights_tmp = LRWeights*Mask

        # create design matrix without intercept

        row_matrix = np.indices((nc,nc))[0] + 1
        col_matrix = np.indices((nc,nc))[1] + 1

        Row = row_matrix[~np.isnan(LRTriangle)]
        Column = col_matrix[~np.isnan(LRTriangle)]
        Ratio = LRTriangle[~np.isnan(LRTriangle)]
        Weights = LRWeights_tmp[~np.isnan(LRTriangle)]

        nr = len(Row)
        Design = np.zeros((nr,nc))
        for i in range(nr):
            for j in range(nc):
                if Column[i]==j+1:
                    Design[i,j]=1

        y_var = np.copy(Ratio)
        #coef_start = np.zeros(nc)
        coef_start = np.log(np.log(CL_facs))
        test = 1
        num_iter = 1
        error = "Converged"
        disp = sigma[Column-1]**2
        disp[disp==0] = 0.0000000001

        #start loop
        while test > 0.00000001:
            coefs = coef_start
            #print(coefs)
            eta = np.matmul(Design, coefs)
            Lambda = np.exp(np.exp(eta)) # log-log link to force link ratios to be greater than 1
            deta_dmu = 1/(Lambda*np.log(Lambda))
            z = eta + (y_var-Lambda) * deta_dmu
            V = Lambda*(Lambda-1) # variance function for Gaussian GLM
            W_vec = Weights/(deta_dmu**2*V*disp) # since sigmas are not constant, bring into Weight vector here so parameter_se are correct later
            W_mat = np.diag(W_vec)
            inv_XWX = np.linalg.inv(np.matmul(Design.T, np.matmul(W_mat, Design))) # inv(X_transpose*W*X)
            XWz = np.matmul(Design.T, np.matmul(W_mat, z))
            coefs = np.matmul(inv_XWX, XWz)
            # create difference between start and end params and check convergence, then repeat:
            test = np.sqrt(np.sum((coefs-coef_start)**2))
            coef_start = coefs
            num_iter = num_iter + 1
            if num_iter > 20:
                error = "Failure to converge"
                test = 0 #force a stop to prevent infinite loop

        #print(error, "in", num_iter, "iterations")

        # Calculate standard error of parameters allowing for over-dispersion
        parameter_se = np.sqrt(np.diag(inv_XWX))

        LinkRatios = np.exp(np.exp(coefs)) # should be standard chain ladder LRs in this case
        LinkRatiosSE = LinkRatios * np.log(LinkRatios) * parameter_se

        CL_Result = LinkRatioMethod_forecast(Triangle, LinkRatios)
        
        Recursive_SD_Result = Recursive_SD(Triangle, LinkRatiosSE, sigma, model="NegBin", Mask=Mask)

        Latest = CL_Result["Latest"]
        Ultimates = CL_Result["Ultimates"]
        Reserves = CL_Result["Reserves"]
        TotalReserves = CL_Result["TotalReserves"]
        TotalLatest = sum(Latest)
        TotalUltimates = sum(Ultimates)

        Reserves_SD = Recursive_SD_Result["Reserves_SD"]
        Reserves_CoV = Recursive_SD_Result["Reserves_CoV"]
        TotalReserve_SD = Recursive_SD_Result["TotalReserve_SD"]
        TotalReserve_CoV = Recursive_SD_Result["TotalReserve_CoV"]

    result = {"coefs": coefs, 
              "parameter_se": parameter_se, 
              "LinkRatios": LinkRatios, 
              "LinkRatiosSE": LinkRatiosSE, 
              "sigma": sigma, 
              "Latest": Latest,
              "TotalLatest": TotalLatest,
              "Reserves": Reserves,
              "TotalReserves": TotalReserves,
              "Ultimates": Ultimates, 
              "TotalUltimates": TotalUltimates,
              "Reserves_SD": Reserves_SD, 
              "Reserves_CoV": Reserves_CoV, 
              "TotalReserve_SD": TotalReserve_SD, 
              "TotalReserve_CoV": TotalReserve_CoV}
    return result

def NegBin_pseudo_data_NP(resids, sigma, avgRatio, weight):
    # Function for calculating pseudo-ratios when bootstrapping the Negative Binomial model using
    # non-parametric bootstrapping
    nc = len(resids[0])-1

    pseudo_ratios = npNaN((nc,nc))
    for i in range(nc):
        for j in range(nc-i):
            pseudo_ratios[i][j] = resids[i][j] * (sigma[j]*np.sqrt(abs(avgRatio[j]*(avgRatio[j]-1)))/np.sqrt(abs(weight[i][j])))+avgRatio[j]
 
    return pseudo_ratios

def NegBin_pseudo_data_Gamma(sigma, avgRatio, weight, **kwargsData):
    # Function for calculating pseudo-ratios when bootstrapping the Negative Binomial model using
    # parametric bootstrapping with a Gamma distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(weight[0])

    pseudo_ratios = npNaN((nc,nc))

    tol = 1e-12
    for i in range(nc):
        for j in range(nc-i):
            Mean = avgRatio[j]
            SD = safe_divide(sigma[j] * np.sqrt(abs(avgRatio[j]*(avgRatio[j]-1))), np.sqrt(abs(weight[i][j])))
            if Mean > tol:
                if SD < tol:
                    pseudo_ratios[i][j] = Mean
                else:
                    scale = (SD**2)/Mean
                    shape = Mean/scale
                    pseudo_ratios[i][j] = np.random.gamma(shape=shape, scale=scale)
            else:
                pseudo_ratios[i][j] = np.random.normal(loc=Mean, scale=SD)

    return pseudo_ratios

def NegBin_pseudo_data_Lognormal(sigma, avgRatio, weight, **kwargsData):
    # Function for calculating pseudo-ratios when bootstrapping the Negative Binomial model using
    # parametric bootstrapping with a Lognormal distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(weight[0])
 
    pseudo_ratios = npNaN((nc,nc))

    tol = 1e-12
    for i in range(nc):
        for j in range(nc-i):
            Mean = avgRatio[j]
            SD = safe_divide(sigma[j] * np.sqrt(abs(avgRatio[j]*(avgRatio[j]-1))), np.sqrt(abs(weight[i][j])))
            if Mean > tol:
                sigma_normal = np.sqrt(np.log(1 + (SD/Mean)**2))
                mean_normal = np.log(Mean) - 0.5 * sigma_normal**2
                pseudo_ratios[i][j] = np.random.lognormal(mean=mean_normal, sigma=sigma_normal)
            else:
                pseudo_ratios[i][j] = np.random.normal(loc=Mean, scale=SD)

    return pseudo_ratios        

def NegBin_Bstrap_Forecast_NP(tridata, factors, resids, sigma, **kwargs):
    # Function for calculating cumulative forecasts when bootstrapping the Negative Binomial model using
    # a non-parametric approach
    nc = len(tridata[0])

    cumulatives = npNaN(tridata.shape)

    for i in range(nc):
        cumulatives[i, nc-i-1] = tridata[i, nc-i-1]
        for j in range(nc-i, nc):
            cumulatives[i, j] = cumulatives[i, j-1]*factors[j-1]+resids[i, j]*sigma[j-1]*np.sqrt(abs(cumulatives[i, j-1]*factors[j-1]*(factors[j-1]-1)))
        cumulatives[i, nc-i-1] = np.nan


    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)

    Ultimates = complete_forecast[:,-1]

    Latest = np.diag(np.fliplr(tridata))

    Reserves = Ultimates - Latest


    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}
    
    return result


def NegBin_Forecast_Gamma(tridata, factors, sigma, **kwargs):
    # Function for calculating cumulative forecasts when bootstrapping the Negative Binomial model using 
    # a parametric approach and a Gamma distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.

    nc = len(tridata[0])

    cumulatives = npNaN(tridata.shape)

    tol = 1e-12

    for i in range(nc):

        cumulatives[i,nc-i-1] = tridata[i,nc-i-1]

        for j in range(nc-i, nc):
            Mean = cumulatives[i,j-1]*factors[j-1]
            SD = sigma[j-1] * np.sqrt(abs(factors[j-1]*(factors[j-1]-1))*abs(cumulatives[i,j-1]))

            if Mean > tol:
                if SD < tol:
                    cumulatives[i,j] = Mean
                else:
                    scale = (SD**2)/Mean
                    shape = Mean/scale
                    cumulatives[i,j] = np.random.gamma(shape=shape, scale=scale)

            else:
                cumulatives[i,j] = np.random.normal(loc=Mean, scale=SD)

        cumulatives[i, nc-i-1] = np.nan

    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)

    Ultimates = complete_forecast[:,-1]

    Latest = np.diag(np.fliplr(tridata))

    Reserves = Ultimates - Latest

    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}
    
    return result

def NegBin_Forecast_Lognormal(tridata, factors, sigma, **kwargs):
    # Function for calculating cumulative forecasts when bootstrapping the Negative Binomial model using 
    # a parametric approach and a Lognormal distribution. A Normal distribution is used if mean is negative,
    # which retains first and second moment properties, but beware, values returned could be negative.
    nc = len(tridata[0])

    cumulatives = npNaN(tridata.shape)

    tol = 1e-12

    for i in range(nc):

        cumulatives[i,nc-i-1] = tridata[i,nc-i-1]

        for j in range(nc-i, nc):
            Mean = cumulatives[i,j-1]*factors[j-1]
            SD = sigma[j-1] * np.sqrt(abs(factors[j-1]*(factors[j-1]-1))*abs(cumulatives[i,j-1]))

            if Mean > tol:
                sigma_normal = np.sqrt(np.log(1 + (SD/Mean)**2))
                mean_normal = np.log(Mean) - 0.5 * sigma_normal**2
                cumulatives[i,j] = np.random.lognormal(mean=mean_normal, sigma=sigma_normal)

            else:
                cumulatives[i,j] = np.random.normal(loc=Mean, scale=SD)

        cumulatives[i, nc-i-1] = np.nan

    complete_forecast = np.nansum(np.dstack((cumulatives,tridata)),2)

    Ultimates = complete_forecast[:,-1]

    Latest = np.diag(np.fliplr(tridata))

    Reserves = Ultimates - Latest

    TotalReserve = np.nansum(Reserves)

    result = {"Cumulatives": cumulatives, 
              "Ultimates": Ultimates, 
              "Reserves": Reserves, 
              "TotalReserve": TotalReserve, 
              "Complete_Forecast": complete_forecast}
    
    return result


def Main_NegBin_Bstrap(tridata, Mask=None, iterations=1000, seed = np.random.randint(1, 1000000), Scale="NonConstant", 
                       BootstrapDist="NonParametric", ForecastDist="Gamma", UserSqrtScale=None):
    # Main function for bootstrapping the Negative Binomial model given input settings.
    if Mask is None:
        Mask = np.ones((len(tridata[0])-1, len(tridata[0])-1))

    np.random.seed(seed)

    LRM = LinkRatioMethod(tridata, Mask)
    LRWeights = LRM["LR_Weights"]
    CL_facs = LRM["LRM_Factors"]
    CL_Result = LinkRatioMethod_forecast(tridata, CL_facs)
    NegBin_Resids =  NegBin_Residuals(tridata, Scale=Scale, Mask=Mask)

    nc = len(CL_facs)
    ZeroAvg_Resids = NegBin_Resids["zeroavg_adj_scaled_resids"]
    sigma = NegBin_Resids["sqrtScale"]

    Pseudo_LRs = npNaN((iterations, nc))
    Reserves = npNaN((iterations, nc+1))
    Ultimates = npNaN((iterations, nc+1))
    TotalReserve = npNaN(iterations)
    Cumulatives = npNaN((iterations, nc+1, nc+1))
    Complete_Cumulatives = npNaN((iterations, nc+1, nc+1))

    ResidsVector = []
    for i in range(len(ZeroAvg_Resids)):
        ResidsVector = [*ResidsVector, *ZeroAvg_Resids[i]]

    ResidsVector = [x for x in ResidsVector if (np.isnan(float(x)) == False)]

    ResidsVector.sort()

    if BootstrapDist=="Gamma":
        NegBin_pseudo_data = NegBin_pseudo_data_Gamma
    elif BootstrapDist=="Lognormal":
        NegBin_pseudo_data = NegBin_pseudo_data_Lognormal
    else:
        NegBin_pseudo_data = NegBin_pseudo_data_NP

    if ForecastDist=="Gamma":
        NegBin_Bstrap_forecast = NegBin_Forecast_Gamma
    elif ForecastDist=="Lognormal":
        NegBin_Bstrap_forecast = NegBin_Forecast_Lognormal
    else:
        NegBin_Bstrap_forecast = NegBin_Bstrap_Forecast_NP

    if UserSqrtScale is None:
        ForecastSigma = sigma
    else:
        ForecastSigma = UserSqrtScale

    kwargsData = {'sigma': sigma, 'avgRatio': CL_facs, 'weight': LRWeights}
    kwargs = {'tridata': tridata, 'sigma': ForecastSigma}

    for i in range(iterations):
        Resampled_Resids = Resample_Resids(ResidsVector, nc+1)

        kwargsData.update({'resids': Resampled_Resids})

        pseudo_ratios = NegBin_pseudo_data(**kwargsData)
        Pseudo_LRs[i] = LinkRatioMethod(tridata, Mask=Mask, pseudoLRs=pseudo_ratios)["LRM_Factors"] # gives chain ladder factors with exclusions
        
        kwargs.update({'factors': Pseudo_LRs[i], 'resids': Resampled_Resids})
        
        Forecast = NegBin_Bstrap_forecast(**kwargs)

        Reserves[i] = Forecast["Reserves"]
        Ultimates[i] = Forecast["Ultimates"]
        TotalReserve[i] = Forecast["TotalReserve"]
        Cumulatives[i] = Forecast["Cumulatives"]
        Complete_Cumulatives[i] = Forecast["Complete_Forecast"]

    Avg_Reserve = np.nanmean(Reserves, axis = 0)
    SD_Reserve = np.nanstd(Reserves, axis = 0)

    CoV_Reserve = abs(safe_divide(SD_Reserve, Avg_Reserve))
    
    Avg_TotalReserve = np.nanmean(TotalReserve)

    SD_TotalReserve = np.nanstd(TotalReserve)

    CoV_TotalReserve = abs(safe_divide(SD_TotalReserve, Avg_TotalReserve))

    Latest = CL_Result["Latest"]
    CL_Ultimates = CL_Result["Ultimates"]
    CL_Reserves = CL_Result["Reserves"]
    CL_Cumulatives = CL_Result["Cumulatives"]

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
              "iterations": iterations, 
              "finished": finished,

              "Complete_Cumulatives": Complete_Cumulatives}

    return result

def Sensitivities(Triangle):
    # Function to identify influential link ratios associated with a claims triangle
    # using Mack's model applied analytically and excluding each ratio in turn

    n = len(Triangle[0])-1
    Mask = np.ones((n, n))
    
    S_Reserves = npNaN((n, n))
    S_Reserves_diff = npNaN((n, n))
    S_Reserves_absdiff = npNaN((n, n))
    S_Reserves_rank = npNaN((n, n))
    S_ReservesSD = npNaN((n, n))
    S_ReservesSD_diff = npNaN((n, n))
    S_ReservesSD_absdiff = npNaN((n, n))
    S_ReservesSD_rank = npNaN((n, n))
    S_ReservesCoV = npNaN((n, n))
    S_ReservesCoV_diff = npNaN((n, n))
    S_ReservesCoV_absdiff = npNaN((n, n))
    S_ReservesCoV_rank = npNaN((n, n))

    Mack_Analytic_Result_Base = Mack_ChainLadder(Triangle, Mask)

    for i in range(n):
        for j in range(n-i):
            if i==0 and j==n-1:
                continue
            else:
                Mask_tmp = Mask.copy()
                Mask_tmp[i,j]=0
                Analytic_Result_tmp = Mack_ChainLadder(Triangle, Mask=Mask_tmp)
                # keep TotalReserves, TotalReserve_SD, TotalReserve_CoV
                # calculate change vs base
                # calculate abs(change vs base)
                # rank abs(change vs base)
                S_Reserves[i,j] = Analytic_Result_tmp["TotalReserves"]
                S_Reserves_diff[i,j] = Analytic_Result_tmp["TotalReserves"] - Mack_Analytic_Result_Base["TotalReserves"]
                S_Reserves_absdiff[i,j] = abs(S_Reserves_diff[i,j])
                
                S_ReservesSD[i,j] = Analytic_Result_tmp["TotalReserve_SD"]
                S_ReservesSD_diff[i,j] = Analytic_Result_tmp["TotalReserve_SD"] - Mack_Analytic_Result_Base["TotalReserve_SD"]
                S_ReservesSD_absdiff[i,j] = abs(S_ReservesSD_diff[i,j])
                
                S_ReservesCoV[i,j] = Analytic_Result_tmp["TotalReserve_CoV"]
                S_ReservesCoV_diff[i,j] = Analytic_Result_tmp["TotalReserve_CoV"] - Mack_Analytic_Result_Base["TotalReserve_CoV"]
                S_ReservesCoV_absdiff[i,j] = abs(S_ReservesCoV_diff[i,j])

    # Rank the differences in ascending order
    S_Reserves_rank = np.argsort(np.argsort((S_Reserves_diff).flatten())).reshape(S_Reserves_diff.shape) + 1
    S_Reserves_rank = S_Reserves_rank.astype(float)
    S_Reserves_rank[np.isnan(S_Reserves_diff)] = np.nan

    S_ReservesSD_rank = np.argsort(np.argsort((S_ReservesSD_diff).flatten())).reshape(S_ReservesSD_diff.shape) + 1
    S_ReservesSD_rank = S_ReservesSD_rank.astype(float)
    S_ReservesSD_rank[np.isnan(S_ReservesSD_diff)] = np.nan
    
    S_ReservesCoV_rank = np.argsort(np.argsort((S_ReservesCoV_diff).flatten())).reshape(S_ReservesCoV_diff.shape) + 1
    S_ReservesCoV_rank = S_ReservesCoV_rank.astype(float)
    S_ReservesCoV_rank[np.isnan(S_ReservesCoV_diff)] = np.nan

    result = {"Reserves_Base": Mack_Analytic_Result_Base["TotalReserves"],
              "ReservesSD_Base": Mack_Analytic_Result_Base["TotalReserve_SD"],
              "ReservesCoV_Base": Mack_Analytic_Result_Base["TotalReserve_CoV"],
              "S_Reserves": S_Reserves,
              "S_Reserves_diff": S_Reserves_diff,
              "S_Reserves_absdiff": S_Reserves_absdiff,
              "S_Reserves_rank": S_Reserves_rank,
              "S_ReservesSD": S_ReservesSD,
              "S_ReservesSD_diff": S_ReservesSD_diff,
              "S_ReservesSD_absdiff": S_ReservesSD_absdiff,
              "S_ReservesSD_rank": S_ReservesSD_rank,
              "S_ReservesCoV": S_ReservesCoV,
              "S_ReservesCoV_diff": S_ReservesCoV_diff,
              "S_ReservesCoV_absdiff": S_ReservesCoV_absdiff,
              "S_ReservesCoV_rank": S_ReservesCoV_rank
              }
    
    return result

def ShowSummaryStats(Stochastic_Results, Output="Reserves"):
    # Function for calculating summary statistics from bootstrap results

    if Output == "Reserves":
        ReserveSims = np.hstack((Stochastic_Results["Reserves"], Stochastic_Results["TotalReserve"].reshape(-1,1)))
    elif Output == "Ultimates":
        ReserveSims = np.hstack((Stochastic_Results["Ultimates"], 
                                 (Stochastic_Results["TotalReserve"]+np.sum(Stochastic_Results["Latest"], axis=0)).reshape(-1,1)))

    mean_reserves = np.mean(ReserveSims, axis=0)
    std_reserves = np.std(ReserveSims, axis=0)
    cov_reserves = abs(safe_divide(std_reserves, mean_reserves))  # Coefficient of variation
    min_reserves = np.min(ReserveSims, axis=0)
    max_reserves = np.max(ReserveSims, axis=0)

    percentiles = [0.5,1,5,10,25,50,75,90,95,99,99.5]
    percentile_results = np.percentile(ReserveSims, percentiles, axis=0)

    # Create a combined table with mean, SD, CoV, and percentiles
    num_cols = ReserveSims.shape[1]
    col_labels = [f"OP {i}" for i in range(1, num_cols)] + ["Total"]

    # Combine all rows: Mean, SD, CoV%, 5th, 50th, 95th percentiles
    all_stats = np.vstack([
        mean_reserves,
        std_reserves,
        cov_reserves * 100,  # Convert to percentage
        min_reserves,
        percentile_results,
        max_reserves
    ])

    row_labels_combined = [
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
    ]

    # Format the table: default 0 decimals, but set 2 decimals for the 3rd row (CoV)
    combined_table = arrayRound_and_format(all_stats, 0)
    # Ensure combined_table is mutable numpy array of strings
    combined_table = np.array(combined_table, dtype=object)
    # Format the CoV row (index 2) with 2 decimal places
    cov_row_formatted = arrayRound_and_format(all_stats[2].reshape(1, -1), 2)[0]
    combined_table[2, :] = cov_row_formatted

    table_plot(combined_table, row_labels_combined, col_labels, 
            "Simulation Summary Statistics" + "\n" + Output)

    return

def Scaled_Results(Simul_Results, Target_Ultimates, Scaling_Method):
    # Function for calculating scaled reserves given target ultimates and scaling method
    num_iterations = Simul_Results["Reserves"].shape[0]
    num_origin_periods = Simul_Results["Reserves"].shape[1]
    # Calculate target reserves and differences
    Target_Reserves = Target_Ultimates - Simul_Results["Latest"]
    Target_Diff = Target_Reserves - Simul_Results["Avg_Reserve"]
    for i in range(num_origin_periods):
        if Simul_Results["Avg_Reserve"][i] == 0:
            Target_Multiplier = 0
        else:
            Target_Multiplier = safe_divide(Target_Reserves, Simul_Results["Avg_Reserve"])

    # Apply selected scaling method for each origin period
    ScaledReserves = np.zeros_like(Simul_Results["Reserves"])
    for i in range(num_iterations):
        for j in range(num_origin_periods):
            if Scaling_Method[j] == 'Additive':
                ScaledReserves[i,j] = Simul_Results["Reserves"][i,j] + Target_Diff[j]
                #ScaledReserves[i, :] = AdditiveScaledReserves[i, :]
            elif Scaling_Method[j] == 'Multiplicative':
                ScaledReserves[i,j] = Simul_Results["Reserves"][i,j] * Target_Multiplier[j]
            #ScaledReserves[i, :] = MultiplicativeScaledReserves[i, :]
            else:
                raise ValueError(f"Unknown scaling method '{Scaling_Method[j]}' for origin period {j}")

    Latest = Simul_Results["Latest"]
    Reserves = ScaledReserves
    Ultimates = ScaledReserves + Latest
    TotalReserve = np.sum(Reserves, axis=1)
    Avg_Reserve = np.nanmean(Reserves, axis = 0)
    SD_Reserve = np.nanstd(Reserves, axis = 0)
    CoV_Reserve = abs(safe_divide(SD_Reserve, Avg_Reserve))
    Avg_TotalReserve = np.nanmean(TotalReserve)
    SD_TotalReserve = np.nanstd(TotalReserve)
    CoV_TotalReserve = abs(safe_divide(SD_TotalReserve, Avg_TotalReserve))

    result = {"Latest": Latest, 
              "Reserves": Reserves,
              "Ultimates": Ultimates, 
              "TotalReserve": TotalReserve, 
              "Avg_Reserve": Avg_Reserve, 
              "SD_Reserve": SD_Reserve, 
              "Avg_TotalReserve": Avg_TotalReserve, 
              "SD_TotalReserve": SD_TotalReserve, 
              "CoV_Reserve": CoV_Reserve, 
              "CoV_TotalReserve": CoV_TotalReserve}
    
    return result

def Calc_RMSEPs(triangle, method, Mask=None):
    # Function to calculate analytic reserves and standard deviations given selected method
    
    if method == "Mack":
        Analytic_Result = Mack_ChainLadder(triangle, Mask=Mask)
    elif method == "ODPConstant":
        Scale = "Constant"
        Analytic_Result = ODP_ChainLadder(triangle, Scale=Scale, Mask=Mask)
    elif method == "ODPNonConstant":
        Scale = "NonConstant"
        Analytic_Result = ODP_ChainLadder(triangle, Scale=Scale, Mask=Mask)
    elif method == "NegBinConstant":
        Scale = "Constant"
        Analytic_Result = NegBin_ChainLadder(triangle, Scale=Scale, Mask=Mask)
    elif method == "NegBinNonConstant":
        Scale = "NonConstant"
        Analytic_Result = NegBin_ChainLadder(triangle, Scale=Scale, Mask=Mask)

    Latest = Analytic_Result["Latest"]
    Reserves = Analytic_Result["Reserves"]
    TotalReserves = Analytic_Result["TotalReserves"]
    Ultimates = Analytic_Result["Ultimates"]
    Reserves_SD = Analytic_Result["Reserves_SD"]
    Reserves_CoV = Analytic_Result["Reserves_CoV"]
    TotalReserve_SD = Analytic_Result["TotalReserve_SD"]
    TotalReserve_CoV = Analytic_Result["TotalReserve_CoV"]

    result = {"Latest": Latest, 
              "Reserves": Reserves,
              "TotalReserves": TotalReserves,
              "Ultimates": Ultimates,
              "Reserves_SD": Reserves_SD,
              "Reserves_CoV": Reserves_CoV,
              "TotalReserve_SD": TotalReserve_SD,
              "TotalReserve_CoV": TotalReserve_CoV,
              "Model": Analytic_Result}

    return result

def Calc_Residuals(triangle, method, Mask=None):
    # Function to calculate residuals given selected method
    
    if method == "Mack":
        Resids = Mack_Residuals(triangle, Mask=Mask)
    elif method == "ODPConstant":
        Scale = "Constant"
        Resids = ODP_Residuals(triangle, Scale=Scale, Mask=Mask)
    elif method == "ODPNonConstant":
        Scale = "NonConstant"
        Resids = ODP_Residuals(triangle, Scale=Scale, Mask=Mask)
    elif method == "NegBinConstant":
        Scale = "Constant"
        Resids = NegBin_Residuals(triangle, Scale=Scale, Mask=Mask)
    elif method == "NegBinNonConstant":
        Scale = "NonConstant"
        Resids = NegBin_Residuals(triangle, Scale=Scale, Mask=Mask)
    
    result = {"unscaled_resids": Resids["unscaled_resids"],
              "adj_unscaled_resids": Resids["adj_unscaled_resids"],
              "scaled_resids": Resids["scaled_resids"], 
              "adj_scaled_resids": Resids["adj_scaled_resids"],
              "zeroavg_adj_scaled_resids": Resids["zeroavg_adj_scaled_resids"],
              "sqrtScale": Resids["sqrtScale"],
              "avg_resid": Resids["avg_resid"]}
    
    return result

def Run_Bootstrap(triangle, method, Mask=None, iterations=1000, seed = np.random.randint(1, 1000000),
                  BootstrapDist="NonParametric", ForecastDist="Gamma", UserSqrtScale=None):
    # Control function to run bootstrap based on selected method
    
    if method == "Mack":
        result = Main_Mack_Bstrap(triangle, Mask, iterations=iterations, seed=seed, 
                               BootstrapDist=BootstrapDist, ForecastDist=ForecastDist, UserSigma=UserSqrtScale)
    elif method == "ODPConstant":
        Scale = "Constant"
        result = Main_ODP_Bstrap(triangle, Mask=Mask, iterations=iterations, seed=seed, Scale=Scale,
                              BootstrapDist=BootstrapDist, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)
    elif method == "ODPNonConstant":
        Scale = "NonConstant"
        result = Main_ODP_Bstrap(triangle, Mask=Mask, iterations=iterations, seed=seed, Scale=Scale,
                              BootstrapDist=BootstrapDist, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)
    elif method == "NegBinConstant":
        Scale = "Constant"
        result = Main_NegBin_Bstrap(triangle, Mask=Mask, iterations=iterations, seed=seed, Scale=Scale,
                              BootstrapDist=BootstrapDist, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)
    elif method == "NegBinNonConstant":
        Scale = "NonConstant"
        result = Main_NegBin_Bstrap(triangle, Mask=Mask, iterations=iterations, seed=seed, Scale=Scale,
                              BootstrapDist=BootstrapDist, ForecastDist=ForecastDist, UserSqrtScale=UserSqrtScale)

    return result

def Incurred_to_Paid(Bstrap_Result, PaidTriangle):
    # this function takes a distribution of ultimates from an incurred bootstrap
    # and subtracts the latest paid from each simulation to give a distribution of reserves.
    # This is useful for comparing the CoV on a like for like basis with a paid bootstrap

    #num_sims = Bstrap_Result["Ultimates"].shape[0]
    #op = Bstrap_Result["Ultimates"].shape[1]
    latest_paid = np.diag(np.fliplr(PaidTriangle))
    Ultimates = Bstrap_Result["Ultimates"]
    Reserves = Ultimates - latest_paid
    TotalReserve = np.nansum(Reserves, axis=1)

    Avg_Reserve = np.nanmean(Reserves, axis = 0)
    SD_Reserve = np.nanstd(Reserves, axis = 0)
    CoV_Reserve = abs(safe_divide(SD_Reserve, Avg_Reserve))
    
    Avg_TotalReserve = np.nanmean(TotalReserve)
    SD_TotalReserve = np.nanstd(TotalReserve)
    CoV_TotalReserve = abs(safe_divide(SD_TotalReserve, Avg_TotalReserve))

    result = {"Latest": latest_paid,
              "Reserves": Reserves,
              "Ultimates": Ultimates,
              "TotalReserve": TotalReserve, 
              "Avg_Reserve": Avg_Reserve, 
              "SD_Reserve": SD_Reserve, 
              "Avg_TotalReserve": Avg_TotalReserve, 
              "SD_TotalReserve": SD_TotalReserve, 
              "CoV_Reserve": CoV_Reserve, 
              "CoV_TotalReserve": CoV_TotalReserve}

    return result 


