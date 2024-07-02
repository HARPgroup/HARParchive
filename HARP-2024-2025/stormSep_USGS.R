# flowData <- dataRetrieval::readNWISdv("01613900",parameterCd = "00060")
# inflow <- flowData$X_00060_00003
# timeIn <- as.Date(flowData$Date)
# mindex <- which(timeIn == "2019-01-01")
# maxdex <- which(timeIn == "2021-12-31")
# seqdex <- seq(mindex,maxdex)
# inflow <- inflow[seqdex]
# timeIn <- timeIn[seqdex]


#Input variables are:
# timeIn = Timestamps associated with streamflow data as vector
# inflow = Streamflow daily data as vector in cfs
# plt = Boolean to determine if plots are necessary
# path = Directory to store plots in. Used if plt is TRUE
# allMinimaStorms = Considers ALL local minima as potnetial storm endpoints.
# Will distinguish between peaks in the case of a multi-day storm hydrograph if
# set to TRUE. Otherwise, a storm is only considered as two subseuqnet local
# minima that fall below baseline flow
# baselineFlowOption = One of c("Water Year","Month","Calendar Year","Climate Year","All").
# Defaults to "All". Determines how program will develop baseline flow. Uses
# horizontal line regression of baseflow and breaks it down based on all time,
# water year, calendar year, or month based on user selection
stormSeparate <- function(timeIn, inflow, 
                          plt = F,
                          path = paste0(getwd(),"/"),
                          allMinimaStorms = FALSE,
                          baselineFlowOption = "All"
){
  #Call packages required by function if not called already:
  require(grwat)
  require(zoo)
  require(sqldf)
  
  #First, get baseflow associated with inflow. Use defaults of grwat for now
  #which involves three passes and the Lyne-Hollick (1979) hydrograph separation
  #method with coefficient a as 0.925
  baseQ <- grwat::gr_baseflow(inflow)
  
  #Add timestamp to the baseflow separation for convenience by creating a
  #dataframe
  baseQ <- data.frame(timestamp = timeIn, Year = as.numeric(format(timeIn,"%Y")),
                      baseQ = baseQ,flow = inflow)
  
  #Find mins/maxes of three consecutive points such that the extreme is in the
  #middle. These represent potential local mins and maxes
  maxes <- rollapply(baseQ$flow,3,function(x) which.max(x) == 2,# & (x[2]!=x[1] & x[2]!= x[3]),
                     align = "left")
  mins <- rollapply(baseQ$flow,3,function(x) which.min(x) == 2,# & (x[2]!=x[1] & x[2]!= x[3]),
                    align = "left")
  #Create data frames of these local minima/maxima with the corresponding
  #timestamp from the original data frame. Note that the rolling looks at the
  #first three data points. If the first triplet has a local max as its second
  #value, the first value in maxes will be TRUE.The timestamp to be associated
  #with this should be that of the middle data point e.g. the second timestamp
  #of the triplet!
  mins <- data.frame(timestamp = baseQ$timestamp[c(FALSE,mins,FALSE)],
                     mins = baseQ$flow[c(FALSE,mins,FALSE)])
  maxes <- data.frame(timestamp = baseQ$timestamp[c(FALSE,maxes,FALSE)],
                      maxes = baseQ$flow[c(FALSE,maxes,FALSE)])
  
  #Need to find mins/maxes below baseline flow. But first baseline flow must be
  #defined. hreg() will try to fit mulitple horizontal lines through subset of
  #data involving every point below that line until best fit is found. This
  #becomes baseline flow, brk
  #Find the break associated with this run and buffer by 10%. Based the hreg on
  #the timescale selected by the user
  # baselineFlowOption = One of c("Water Year","Month","Calendar Year","All")
  if(baselineFlowOption == "All"){
    #Use the entire baseflow dataset to get baseline flow, brk
    brk <- hreg(baseQ$baseQ,limit = 1)
    brk <- brk * 1.1
    #Make as long as timeseries
    brk <- rep(brk,nrow(baseQ))
  }else if(baselineFlowOption %in% c("Calendar Year", "Climate Year","Water Year")){
    #Based on the user option, determine the start month of the year
    #designation. Water Years run from October - September, Climate years from
    #April - March, and Calendar Years from January - December
    if(baselineFlowOption == "Water Year"){
      WYS <- "10-01"
    }else if(baselineFlowOption == "Climate Year"){
      WYS <- "04-01"
    }else{
      WYS <- "01-01"
    }
    #Make dates of the input timesteps:
    dataDates <- as.Date(timeIn)
    #Create an emtpty vector for the new annual designations
    dataWY <- NA
    
    #Identify months in the next year that are classified as the previous year's
    #water/climate year e.g. January 2021 is in Water Year 2020 that begins in
    #October. Get the month using gsub to get only the characters prior to first
    #dash
    WYM <- as.numeric(gsub("-.*","",WYS))
    if(WYM > 1){
      WYP <- seq(1,(WYM - 1))
    }else{
      WYP <- 0
    }
    
    #Add leading zeros to month number January - September
    WYP[nchar(WYP) == 1]<-paste0("0",WYP[nchar(WYP) == 1])
    #Combine to form a regex pattern that searches for any of the months at the
    #END of a string ($)
    WYP <- paste0(WYP,"$",collapse = "|")
    #Get the year and month associated with the dates
    WY <- format(dataDates,"%Y-%m")
    
    #Initialize water year by making it the calendar year + 1
    dataWY <- as.numeric(gsub("-.*","",WY)) + 1
    
    #Search for all months from January to the start of the water year. These
    #months need to be assigned previous water year number (current calendar
    #year)
    dataWY[grepl(WYP,WY)] <- dataWY[grepl(WYP,WY)] - 1
    
    #Exception case occurs when water year is calendar year, in which case water
    #year ends in calendar year such that calendar year = water year
    if(WYS == "01-01"){
      dataWY <- as.numeric(format(dataDates,"%Y"))
    }
    #Store the years in the appropriate column in baseQ data frame
    baseQ$Year <- dataWY
    #Create an empty vector the length of the baseQ data frame
    brk <- numeric(nrow(baseQ))
    #For each unique water year, calendar year, etc., run the hreg and store in
    #brk
    for (i in unique(dataWY)){
      baseQsubset <- baseQ[baseQ$Year == i,]
      #Use the subset of the baseflow dataset to get baseline flow, brk
      brki <- hreg(baseQsubset$baseQ,limit = 1)
      brki <- brki * 1.1
      #Store results
      brk[baseQ$Year == i] <- brki
    }
  }else if(baselineFlowOption == "Month"){
    #Make dates of the input timesteps:
    dataDates <- as.Date(timeIn)
    
    #Create a vector of the months and years, essentially getting a vector of
    #all months in the timeIn vector
    monthYears <- format(dataDates,"%m-%Y")
    
    #Create an empty vector the length of the baseQ data frame
    brk <- numeric(nrow(baseQ))
    for (i in unique(monthYears)){
      baseQsubset <- baseQ[monthYears == i,]
      #Use the subset of the baseflow dataset to get baseline flow, brk
      brki <- hreg(baseQsubset$baseQ,limit = 1)
      brki <- brki * 1.1
      #Store results
      brk[monthYears == i] <- brki
    }
  }
  #Add baseline flow to baseQ
  baseQ$baselineQ <- brk
  
  #Next step is to isolate storms. This can be accomplished by taking a minimum
  #and the next point to fall below baseline flow, brk. Each storm is checked to
  #ensure a maximum above some reference level, here 1.5*brk. This eliminates
  #small storms with little or no peak or rises in baseflow. Only minimums below
  #the brk value are considered as these are storms that occur at baseflow and
  #fully rise/recede.
  
  #Get the times associated with minimums that are below baseline flow brk.
  #First, join in the baseline flow for the timeperiod:
  mins <- sqldf("SELECT mins.*, baseQ.baselineQ
        FROM mins 
        LEFT JOIN baseQ
        ON mins.timestamp = baseQ.timestamp")
  
  if(allMinimaStorms){
    #Use all minima as potential storm start and stop points
    x <- mins$timestamp
    #Get the corresponding local maximums
    y <- maxes$maxes
  }else{
    #Get only the minima timestamps that lie below baseline flow
    x <- mins$timestamp[mins$mins < mins$baselineQ]
    #Get the corresponding local maximums
    y <- maxes$maxes[mins$mins < mins$baselineQ]
  }
  
  #A data frame to build with storms in each column
  stormsep <- list()
  #An ID column to store the storm number in baseQ
  baseQ$stormID <- NA
  
  #Start evaluating each set of minima to evaluate if there is a qualifying
  #storm event. If there is, store it with all relevant data
  for (i in 1:(length(x) - 1)){
    # if(i==73){browser()}
    endIndex <- 1
    #Initial guess at storm endpoints e.g. two local minimums
    storm <- c(x[i], x[i + endIndex])
    #initial stormflows and the times associated with those flows:
    stormflow <- inflow[timeIn >= storm[1] & timeIn <= storm[2]]
    
    #When using allMinimaStorms, some minima may be combined. Skip loops that
    #feature combined storms. So, if at frist storm and nextStorm are January
    #1st - Jan 10 and Jan 10 - Jan 12, but these are combined into one (Jan 1 -
    #Jan 12), then we can skip the loop of Jan 10 - Jan 12. First, make sure
    #there is a storm in stormsep and that this is not the first loop i.e. i !=
    #1
    if(i > 1 && length(stormsep) > 0){
      #Get the previous storm
      prevStorm <- stormsep[[length(stormsep)]]
      #Remove the NAs so we only have the timestamps associated with the current
      #storm
      prevStorm <- prevStorm[!is.na(prevStorm$flow),]
      #If the end point of the storm is in the previous storm, skip this
      #iteration. This storm has already been accounted for.
      if(storm[2] %in% prevStorm$timestamp){
        #Skip to next iteration of for loop:
        next
      }
    }
    
    #If the second minimum flow is practically equal to the next local maximum,
    #combine with the next storm as they are likely of the same storm (of
    #course, this assumes that the timestamps match up which they should in the
    #current set-up regardless of allMinimaStorms). But only do this if
    #allMinimaStorms is TRUE since otherwise baselineflow is the cut-off.
    if(allMinimaStorms & !is.na(x[i + 1 + endIndex])){
      #Initial guess at storm endpoints e.g. two local minimums
      nextStorm <- c(x[i + 1], x[i + 1 + endIndex])
      #initial stormflows and the times associated with those flows:
      nextStormflow <- inflow[timeIn >= nextStorm[1] & timeIn <= nextStorm[2]]
      #What is the maximum of this storm event?
      nextMaxStormFlow <- max(nextStormflow)
      
      while(!is.na(nextMaxStormFlow) & 
            stormflow[length(stormflow)] >= 0.8 * nextMaxStormFlow){
        endIndex <- endIndex + 1
        #Initial guess at storm endpoints e.g. two local minimums
        storm <- c(x[i], x[i + endIndex])
        #initial stormflows and the times associated with those flows:
        stormflow <- inflow[timeIn >= x[i] & timeIn <= x[i + endIndex]]
        
        if(!is.na(x[i + 1 + endIndex])){
          #Initial guess at storm endpoints e.g. two local minimums
          nextStorm <- c(x[i + endIndex], x[i + 1 + endIndex])
          #initial stormflows and the times associated with those flows:
          nextStormflow <- inflow[timeIn >= nextStorm[1] & timeIn <= nextStorm[2]]
          #What is the maximum of this storm event?
          nextMaxStormFlow <- max(nextStormflow)
        }else{
          nextMaxStormFlow <- NA
        }
      }
    }
    
    #Get the times associated with the storm
    stormtimes <- timeIn[timeIn >= x[i] & timeIn <= x[i + endIndex]]
    
    #When does the maximum flow associated with this storm occur?
    maxtime <- stormtimes[stormflow == max(stormflow)][1]
    
    #If there is a point at baseflow before next minimum, use it instead to
    #prevent over elongated tails. We just need to ensure it takes place before
    #the next minima, is over brk, and occurs after maxtime
    endAlt <- (baseQ$timestamp[baseQ$flow < baseQ$baselineQ & 
                                 baseQ$timestamp > x[i] & 
                                 baseQ$timestamp < x[i + endIndex] & 
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
      flow = stormflow,
      baselineQ = baseQ$baselineQ[baseQ$timestamp >= storm[1] & baseQ$timestamp <= storm[2]]
    )
    
    #data frame of whole time series
    store <- data.frame(timestamp = baseQ$timestamp, flow = NA,baseflow = NA)
    #Fills in only flow data during storm, leaving rest as NA
    store$flow[store$timestamp >= storm[1] & 
                 store$timestamp <= storm[2]] <- stormdat$flow
    store$baseflow[store$timestamp >= storm[1] & 
                     store$timestamp <= storm[2]] <- stormdat$baseQ
    store$baselineflow[store$timestamp >= storm[1] & 
                     store$timestamp <= storm[2]] <- stormdat$baselineQ
    
    #If maximum exceeds limit, add it to the stormsep list:
    if(any(store$flow > (2.0 * store$baselineflow),na.rm = TRUE)){
      #Set the storm number in baseQ. In case of overlapping end/start points,
      #store both IDs.
      startID <- baseQ$stormID[baseQ$timestamp == storm[1]]
      if(!is.na(startID)){
        #If the stormID in baseQ is already entered for the start date, it
        #overlaps with a previous storm since both use that local minima. Store
        #both IDs.
        baseQ$stormID[baseQ$timestamp == storm[1]] <- paste0(startID,",",length(stormsep) + 1)
        baseQ$stormID[baseQ$timestamp > storm[1] & baseQ$timestamp <= storm[2]] <- length(stormsep) + 1
      }else{
        #If there is no overlap, enter the storm ID for all storm timestamps
        baseQ$stormID[baseQ$timestamp >= storm[1] & baseQ$timestamp <= storm[2]] <- length(stormsep) + 1
      }
      
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
  transients <- data.frame(startDate=character(length(stormsep)),
                           endDate = NA, maxDate = NA,
                           rising = NA, RsqR = NA, falling = NA, RsqF = NA,
                           durAll = NA,durF = NA,durR = NA,
                           volumeTotalMG = NA,
                           volumeAboveBaseQMG = NA,
                           volumeAboveBaselineQMG = NA,
                           volumeTotalMG_rise = NA,
                           volumeAboveBaseQMG_rise = NA,
                           volumeAboveBaselineQMG_rise = NA,
                           volumeTotalMG_fall = NA,
                           volumeAboveBaseQMG_fall = NA,
                           volumeAboveBaselineQMG_fall = NA)
  for (i in 1:length(stormsep)){
    #Find the storm of interest
    storm <- stormsep[[i]]
    #remove nas:
    storm <- storm[!is.na(storm[,2]),]
    #Look for where the max is
    maxtime <- storm[storm[,2] == max(storm[,2]),1][1]
    
    #Store the start and end time of the storm
    transients$startDate[i] <- format(storm$timestamp[1],"%Y-%m-%d")
    transients$endDate[i] <- format(storm$timestamp[nrow(storm)],"%Y-%m-%d")
    transients$maxDate[i] <- format(storm[storm[,2] == max(storm[,2]),1][1],"%Y-%m-%d")
    
    #Separate rising and falling limbs based on maxtime e.g. the rising limb is
    #all values leading up to maxtime
    rising <- storm[storm[,1] <= maxtime,]
    falling <- storm[storm[,1] >= maxtime,]
    
    #What is the volume of the storm streamflow i.e. total volume? First, get
    #the difference in timestamps throughout the storm from one time to the
    #next:
    timeDiff <- difftime(storm$timestamp[2:nrow(storm)], storm$timestamp[1:(nrow(storm) - 1)],
                         units = "secs")
    timeDiff <- as.numeric(timeDiff)
    #Repeat for rising and falling limbs only:
    #Rising:
    timeDiff_rise <- difftime(rising$timestamp[2:nrow(rising)], rising$timestamp[1:(nrow(rising) - 1)],
                         units = "secs")
    timeDiff_rise <- as.numeric(timeDiff_rise)
    #Falling:
    timeDiff_fall <- difftime(falling$timestamp[2:nrow(falling)], falling$timestamp[1:(nrow(falling) - 1)],
                         units = "secs")
    timeDiff_fall <- as.numeric(timeDiff_fall)
    #Using a trapezoidal approach, get the area of each trapezoid to estimate
    #volume of the storm. Use three approaches. Total storm volume, volume above
    #baseflow, volume above baseline flow
    #Total storm flow:
    trapzArea <- function(timeDiff,stormFlow){
      out <- timeDiff * ((stormFlow[1:(length(stormFlow) - 1)] + stormFlow[2:length(stormFlow)]) / 2)
    }
    trapz_total <- trapzArea(timeDiff,storm$flow)
    #Only flow above baseflow:
    trapz_abovebaseQ <- trapzArea(timeDiff,(storm$flow - storm$baseflow))
    #Only flow above baseline flow. THIS CAN BE NEGATIVE AND THEREFORE
    #UNRELIABLE?:
    trapz_abovebaselineQ <- trapzArea(timeDiff,(storm$flow - storm$baselineflow))
    
    #Rising/falling storm flow:
    trapz_total_rise <- trapzArea(timeDiff_rise,rising$flow)
    trapz_total_fall <- trapzArea(timeDiff_fall,falling$flow)
    #Only flow above baseflow:
    trapz_abovebaseQ_rise <- trapzArea(timeDiff_rise,(rising$flow - rising$baseflow))
    trapz_abovebaseQ_fall <- trapzArea(timeDiff_fall,(falling$flow - falling$baseflow))
    #Only flow above baseline flow. THIS CAN BE NEGATIVE AND THEREFORE
    #UNRELIABLE?:
    trapz_abovebaselineQ_rise <- trapzArea(timeDiff_rise,(rising$flow - rising$baselineflow))
    trapz_abovebaselineQ_fall <- trapzArea(timeDiff_fall,(falling$flow - falling$baselineflow))
    
    #Total volume is the sum of area of the trapezoids found above converted to
    #MG from CF (1 CF * 12^3 in^3/cf * 231 gal/in^3 * 1 MG/1000000 gal):
    volume_total <- sum(trapz_total) * 12 * 12 * 12 / 231 / 1000000
    volume_abovebaseQ <- sum(trapz_abovebaseQ) * 12 * 12 * 12 / 231 / 1000000
    volume_abovebaselineQ <- sum(trapz_abovebaselineQ) * 12 * 12 * 12 / 231 / 1000000
    #Rising Limb
    volume_total_rise <- sum(trapz_total_rise) * 12 * 12 * 12 / 231 / 1000000
    volume_abovebaseQ_rise <- sum(trapz_abovebaseQ_rise) * 12 * 12 * 12 / 231 / 1000000
    volume_abovebaselineQ_rise <- sum(trapz_abovebaselineQ_rise) * 12 * 12 * 12 / 231 / 1000000
    #Falling limb:
    volume_total_fall <- sum(trapz_total_fall) * 12 * 12 * 12 / 231 / 1000000
    volume_abovebaseQ_fall <- sum(trapz_abovebaseQ_fall) * 12 * 12 * 12 / 231 / 1000000
    volume_abovebaselineQ_fall <- sum(trapz_abovebaselineQ_fall) * 12 * 12 * 12 / 231 / 1000000
    #Store results:
    transients$volumeTotalMG[i] <- volume_total
    transients$volumeAboveBaseQMG[i] <- volume_abovebaseQ
    transients$volumeAboveBaselineQMG[i] <- volume_abovebaselineQ
    transients$volumeTotalMG_rise[i] <- volume_total_rise
    transients$volumeAboveBaseQMG_rise[i] <- volume_abovebaseQ_rise
    transients$volumeAboveBaselineQMG_rise[i] <- volume_abovebaselineQ_rise
    transients$volumeTotalMG_fall[i] <- volume_total_fall
    transients$volumeAboveBaseQMG_fall[i] <- volume_abovebaseQ_fall
    transients$volumeAboveBaselineQMG_fall[i] <- volume_abovebaselineQ_fall
    
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
    if(plt == T){
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
      lines(storm[,1], storm[,4],lty = 3,lwd = 2)
      #Put a small legend on the plot
      legend("topleft",c("Flow","Baseflow","Rise Limb","Fall Limb","Baseline"),
             col = c("black","darkgreen","darkblue","darkred","black"),
             lty = c(1,1,1,1,3),
             bty = "n")
      #Close the plot PNG and output the file
      dev.off()
    }
  }
  #rm(maxtime,storm,rising,falling,modelR,modelF,i,ext,fxn_locations)
  out <- list(Storms = stormsep,Stats = transients,
              flowData = baseQ)
  return(out)
}

#Below function hreg (horizontal regression) will try to fit mulitple
#horizontal lines through subset of data involving every point below that line
#until best fit is found. This becomes baseline flow, brk
hreg <- function(x, limit = 1){
  #What is the numeric percentile of x that is of percent limit?
  lim <- as.numeric(quantile(x,limit))
  #Give all of x below the percentile of limit:
  x <- x[x <= lim]
  #Get all percentiles of x from 0 to 100% by 0.1%:
  lns <- as.numeric(quantile(x, seq(0,1,0.001)))
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

# pathOut <- paste0(path,"storm",i,ext)


# outTest <- stormSeparate(timeIn, inflow,
#                          plt = FALSE,path = paste0(getwd(),"/"),
#                          allMinimaStorms = TRUE,
#                          baselineFlowOption = "Month"
# )
# length(outTest$Storms)
