Stormsep.R
This function was made to quantify hydrograph rate of rise/fall and storm duration from Chesapeake Bay Model data. It has not been tested
on USGS gauge data or other observed/modeled data. Pass in a timeseries to separates storm and fit separate exponential regressions
to the rising and falling limbs of the hydrographs. In this manner, it creates exponential coefficients that represent the rate of rise 
or fall of the storm hydrographs. It records the adjusted coefficients of determination (R squared) values for each fit for filtering.
It also calculates the number of timesteps ellapsed during each storm to develop an estimate at duration. Optionally, it can plot all storm
so that a user can view the algorithim outputs and the exponential data fits (set plt to TRUE). The stormSeparate function outputs a list of two 
data frames. The Stats data frame contains the regression R squared values, the rise and fall coefficients, duration, and the number of storms.
The Storms data frame contains a data frame of length equal to the length of the time series. Baseflow and quickflow are recorded in columns 2 and 3
Each storm is recorded in a separate column so that the number of columns minus three is the number of storms. All values not in the storm are NA, but
the storms themselves will read as quickflow data

FlowStats_updated.R
This is a useful script for developing basic stats for Chesapeake Bay Model data, including mean baseflow, 7Q10, 30Q2, September 10%, August Low Flow,
mean storm duration (from Stormsep.R), rate of rise and fall, quantile flow, drought of record, and number of days of zero flow. It assumes input data
is hourly, but computes both hourly and daily statistics.