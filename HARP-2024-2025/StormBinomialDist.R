gageid = '01634000'
flowData <- readNWISdv(gageid,'00060')
flowData <- flowData %>%
  rename(flow = X_00060_00003)
inflow <- flowData$flow
timeIn <- as.Date(flowData$Date)
mindex <- which(timeIn == "2021-01-01")
maxdex <- which(timeIn == "2022-12-31")
seqdex <- seq(mindex,maxdex)
inflow <- inflow[seqdex]
timeIn <- timeIn[seqdex]

#Input variables are:
# timeIn = Timestamps associated with streamflow data as vector
# inflow = Streamflow daily data as vector in cfs
# plt = Boolean to determine if plots are necessary
# path = Directory to store plots in. Used if plt is TRUE

  #First, get baseflow associated with inflow. Use defaults of grwat for now
  #which involves three passes and the Lyne-Hollick (1979) hydrograph separation
  #method with coefficient a as 0.925
  baseQ <- grwat::gr_baseflow(inflow)
  
  #Add timestamp to the baseflow separation for convenience by creating a
  #dataframe
  baseQ <- data.frame(timestamp = timeIn, baseQ = baseQ,flow = inflow)
  
  #Find mins/maxes of three consecutive points such that the extreme is in the
  #middle. These represent potential local mins and maxes
  maxes <- rollapply(baseQ$baseQ,3,function(x) which.max(x)==2,align = "left")
  mins <- rollapply(baseQ$baseQ,3,function(x) which.min(x)==2,align = "left")
  #Create data frames of these local minima/maxima with the corresponding
  #timestamp from the original data frame
  mins <- data.frame(timestamp = baseQ$timestamp[mins],mins = baseQ$baseQ[mins])
  maxes <- data.frame(timestamp = baseQ$timestamp[maxes],maxes = baseQ$baseQ[maxes])
  
  #Need to find mins/maxes below baseline flow. But first baseline flow must be
  #defined. Below function hreg (horizontal regression) will try to fit mulitple
  #horizontal lines through subset of data involving every point below that line
  #until best fit is found. This becomes baseline flow, brk
  hreg <- function(x, limit = 1){
    #What is the numeric percentile of x that is of percent limit?
    lim <- as.numeric(quantile(x,limit))
    #Give all of x below the percentile of limit:
    x <- x[x <= lim]
    #Get all percentiles of x from 0 to 100% by 0.001%:
    lns <- as.numeric(quantile(x, seq(0,1,0.00001)))
    #Keep only values above 0
    lns <- lns[lns != 0]
    #A vector for mean square error
    mse <- numeric(length(lns))
    #For each values in lns, determine the mean square error if this is a
    #horizontal regression of the data in x
    for (i in 1:length(lns)){
      line <- lns[i]
      mse[i] <- mean((x - line)^2)
    }
    #Return the percentile x that created the minimum least square error:
    return(lns[which.min(mse)])
  }
  
  #Find the break associated with this run and buffer by 10%
  brk <- hreg(baseQ$baseQ,limit = 1)
  brk <- brk * 1.1
  
  #Quick plot to visualize baseflow relative to brk
  mindex <- which(timeIn == "2021-10-01")
  maxdex <- which(timeIn == "2022-12-31")
  seqdex <- seq(mindex,maxdex)
  plot(timeIn[seqdex],inflow[seqdex],type = "l",lwd = 2)
  lines(timeIn[seqdex],rep(brk,length(seqdex)),col = "blue",lwd = 2)
  lines(baseQ$timestamp[seqdex],baseQ$baseQ[seqdex],col = "red",lwd = 2)
  
  #Next step is to isolate storms. This can be accomplished by taking a minimum
  #and the next point to fall below baseline flow, brk. Each storm is checked to
  #ensure a maximum above some reference level, here 1.5*brk. This eliminates
  #small storms with little or no peak or rises in baseflow. Only minimums below
  #the brk value are considered as these are storms that occur at baseflow and
  #fully rise/recede.
  
  #Get the times associated with minimums that are below baseline flow brk:
  x <- mins$timestamp[mins$mins < brk]
  #A data frame to build with storms in each column
  stormsep <- data.frame(timestamp = as.POSIXct(baseQ$timestamp),
                         Baseflow = baseQ$baseQ,
                         QuickFlow = inflow)
  stormsep <- list()
  
  for (i in 1:(length(x) - 1)){
    #Initial guess at storm endpoints e.g. two local minimums
    storm <- c(x[i], x[i + 1])
    #initial stormflows and the times associated with those flows:
    stormflow <- inflow[timeIn >= x[i] & timeIn <= x[i + 1]]
    stormtimes <- timeIn[timeIn >= x[i] & timeIn <= x[i + 1]]
    
    #When does the maximum flow associated with this storm occur?
    maxtime <- stormtimes[stormflow == max(stormflow)]
    
    #If there is a point at baseflow before next minimum, use it instead to
    #prevent over elongated tails. We just need to ensure it takes place before
    #the next minima, is over brk, and occurs after maxtime
    endAlt <- (baseQ$timestamp[baseQ$flow < brk & 
                                 baseQ$timestamp >= x[i] & 
                                 baseQ$timestamp < x[i+1] & 
                                 baseQ$timestamp > maxtime])[1]
    #If an alternate endpoint for the storm was found, redefine the storm:
    if (!is.na(endAlt)){
      storm <- c(x[i],endAlt)
      stormflow <- inflow[timeIn >= x[i] & timeIn <= endAlt]
      stormtimes <- timeIn[timeIn >= x[i] & timeIn <= endAlt]
    }
    #data frame of storm data
    stormdat <- data.frame(
      timestamp = stormtimes,
      baseQ = baseQ$baseQ[baseQ$timestamp >= storm[1] & baseQ$timestamp <= storm[2]],
      flow = stormflow
    )
    #data frame of whole time series
    store <- data.frame(timestamp = baseQ$timestamp, flow = NA,baseflow = NA)
    #Fills in only flow data during storm, leaving rest as NA
    store$flow[store$timestamp >= storm[1] & 
                 store$timestamp <= storm[2]] <- stormdat$flow
    store$baseflow[store$timestamp >= storm[1] & 
                     store$timestamp <= storm[2]] <- stormdat$baseQ
    
    #If maximum exceeds limit, add it to the stormsep list:
    if(max(store$flow, na.rm = T) > (1.5 * brk)){
      stormsep[[length(stormsep) + 1]] <- store
    }
  }
  
  #Now plot each storm and fit exponential curves to rising and falling limbs
  #Store coefficients and statistics for each curve into a data frame, looking
  #at shape of curve and the adjusted R square values. Store each storm as a PNG
  #graph in the designated area. Need to prevent errors from zero flow. Added
  #0.0001 to all flow values. This WILL RESULT IN BIAS
  ext <- ".png"
  #Empty data frame to store statistics
  transients <- data.frame(rising=numeric(length(stormsep)),
                           RsqR=NA,falling=NA,RsqF=NA,durAll=NA,durF=NA,durR=NA)
  for (i in 1:length(stormsep)){
    #Find the storm of interest
    storm <- stormsep[[i]]
    #remove nas:
    storm <- storm[!is.na(storm[,2]),]
    #Look for where the max is
    maxtime <- storm[storm[,2] == max(storm[,2]),1]
    
    #Separate rising and falling limbs based on maxtime e.g. the rising limb is
    #all values leading up to maxtime
    rising <- storm[storm[,1] <= maxtime,]
    falling <- storm[storm[,1] >= maxtime,]
    
    #Create an exponential regression for the rising limb to roughly fit the
    #rising limb based on an "ideal" hydrograph
    modelR <- lm(log(rising[,2] + 0.0001) ~ seq(1,length(rising[,1])))
    #Store exponential coefficient and adjusted r squared values
    transients$rising[i] <- summary(modelR)$coefficients[2]
    transients$RsqR[i] <- summary(modelR)$adj.r.squared
    
    #Create an exponential regression for the falling limb
    modelF <- lm(log(falling[,2] + 0.0001) ~ seq(1,length(falling[,1])))
    transients$falling[i] <- summary(modelF)$coefficients[2]
    transients$RsqF[i] <- summary(modelF)$adj.r.squared
    
    #Finds duration of the storm, rising and falling limbs combined
    transients$durAll[i] <- length(storm$timestamp)
    #Finds duration of the rising limb
    transients$durF[i] <- length(rising$timestamp)
    #Finds duration of the falling limb
    transients$durR[i] <- length(falling$timestamp)
    
    #Plot the storm and the fitted rising and falling limbs and store them in
    #designated path. Include the baseflow and and the baseline flow brk
      #Set plot output path and dimensions
      png(paste0(path,"storm",i,ext), width=1820,height=760)
      #Set plot margines
      par(mar=c(5,6,2,4))
      #Plot the storm, making the labels a little thicker and the lines of the
      #plot and labeling the axes
      plot(storm[,1], storm[,2], type='l',
           xlab='Date', ylab='Flow (cfs)',
           lwd=2, cex.axis=2, cex.lab=2)
      #Plot the fitted rising limb:
      lines(storm[storm[,1] <= maxtime,1],
            exp(fitted(modelR)),
            col = 'darkblue',lwd = 2)
      #Plot the fitted falling limb:
      lines(storm[storm[,1] >= maxtime,1],
            exp(fitted(modelF)),
            col = 'darkred', lwd = 2)
      #Plot the baseflow
      lines(storm[,1], storm[,3],
            col = "darkgreen", lwd = 2)
      #Plot the baseline flow brk as a dashed line via lty = 3
      abline(h = brk,lty = 3,lwd = 2)
      #Put a small legend on the plot
      legend("topleft",c("Flow","Baseflow","Rise Limb","Fall Limb","Baseline"),
             col = c("black","darkgreen","darkblue","darkred","black"),
             lty = c(1,1,1,1,3),
             bty = "n")
      #Close the plot PNG and output the file
      dev.off()
  #rm(maxtime,storm,rising,falling,modelR,modelF,i,ext,fxn_locations)
  out <- list(Storms=stormsep,Stats=transients)
return(out)

  
# create plot for binomial distribution
# binomial dist plot for flow from stormdat (USGS)
plot(stormdat$flow,dbinom(stormdat$flow,size=500,prob=0.3),type="h",xlim = c(1,500))
#compare to PRISM data
#Load data from Baseflowsummary.RMD
startDate <- min(stormdat$timestamp)
endDate <- max(stormdat$timestamp)

prism_data <- read.csv("http://deq1.bse.vt.edu:81/files/met/usgs_ws_01634000-prism-2022-2023.csv")
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)), 
                                                month(as.Date(prism_data$obs_date)), 
                                                day(as.Date(prism_data$obs_date)), 
                                                week(as.Date(prism_data$obs_date)))
plot(stormdat$timestamp,stormdat$flow)

#prism_range <- sqldf('select * from prism_data where obs_date between startDate and endDate')

prism_range <- subset(prism_data, obs_date>startDate & obs_date<endDate)

plot(day(prism_range$obs_date),prism_range$precip_mm)

# linear models for both




























 
