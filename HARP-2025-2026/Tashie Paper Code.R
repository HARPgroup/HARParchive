##############################################################################
####    Part 0
####    Load Necessary Packages
################################################################################
# where to publish? here is a list...
# Journal of Hydrometeorology
# JAMES Journal of Advances in Modeling Earth Systems
# Journal of Geophysical Research: Atmospheres
# HESS
# Climate Dynamics



library(data.table)     # for fread, at least
library(lubridate)
library(tseries)
substrRight = function(x, n)    {   # for finding the last n characters in a string
  substr(x, nchar(x)-n+1, nchar(x))
}


recess_char = data.frame(       # for storing summary data for each gage
  gage = integer(),
  region = character(),
  tot_days = integer(),
  allmod_slope = double(),	# start of cloud based slope parameters
  allmod_a = double(),
  dry_rightmod_three_a = double(),
  dry_lowmod_one_a = double(),
  dry_rightmod_onefive_a = double(),
  dry_lowmod_onefive_a = double(),
  wet_rightmod_three_a = double(),
  wet_lowmod_one_a = double(),
  wet_rightmod_onefive_a = double(),
  wet_lowmod_onefive_a = double(),
  timelag_diff = integer(),
  stringsAsFactors = FALSE)

# read in a txt file with the names (and ref / non-ref status) of every gage in CONUS
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\spreadsheets-in-csv-format")#
all_gages = read.csv("conterm_bas_classif.txt", colClasses = c("STAID" = "character", "AGGECOREGION" = "character")) #   read in GAGES-II database
basinid = read.csv("conterm_basinid.txt", colClasses = c("STAID" = "character"))									# read in summary GAGES-II geomorph chars
ref_gages = which(all_gages$CLASS == "Ref")     #   identify which basins are chategorized as reference basins
run = 0

start_date = "1900-01-01"                   
end_date = "2019-10-01"

# Initializing Scenarios
decreasing_dQ = TRUE		# must -dQ decrease? if not, then dQ must always be
minRecessLength = 3		# minimum days for recession
rm_Ith = 1			# remove the first I days of recession
rm_Nth = 1			# remove the last N days of recession
minDays = 250
long10	=	FALSE		# only identifying the longest 10 events

# defining objects
slope = 2
intercept = 1


################################################################################
####    Part 1
####    Read in USGS Data
################################################################################
while (run < length(ref_gages)) {
  #while (run < 12)    {     #### fewer examples for testing
  run = run + 1
  if(run %% 10 == 0) {
    print(run)
    write.csv(recess_char, "dryandwet_test_4dec2019.csv")
  }
  gage_number = all_gages[ref_gages[run], "STAID"]
  recess_char[run, "gage"] = gage_number
  recess_char[run, "region"] = all_gages[ref_gages[run] , "AGGECOREGION"]
  
  
  USGS_webpage = paste0("https://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=",
                        gage_number, "&referred_module=sw&period=&begin_date=",
                        start_date, "&end_date=", end_date)
  
  # read in the initial ### lines from the webpage, then find the line where the comment characters actually end
  # the problem is that there are two lines of header on the webpage following a somewhat random number of lines of comment characters, while the number and order of columns is also indeterminite
  max_lines_to_read = 100         # 50 lines is probably safe, but whatever
  gage_header_data = scan(USGS_webpage, what = character(), nmax = max_lines_to_read, sep = "\n")
  if (NROW(gage_header_data) == 100)  {
    init_char = "#"                 # looking for the first line that doesnt start with "#"... prob not efficient
    increment = 0                   # ultimately tells where fread should start reading in data (+2)
    while (init_char == "#")    {
      increment = increment + 1
      init_char = substr(gage_header_data[increment], 1, 1)
    }
    
    col_names = strsplit(gage_header_data[increment], "\t")        # saving the column names for later
    q_col_num = which(substrRight(col_names[[1]], 11) == "00060_00003")[1] # identifying q using the final 11 chars of the column name
    #sometimes streamflow (00060_00003) is given in multiple units (e.g., cfs and cms) or by multiple gages, so later we identify which has the longest period of record and use that one
    # !!!!! the code currently just uses the first instance.... need to change in future
    if(!is.na(q_col_num))   {
      
      # use fread to skip the first XX lines and headers to read in the actual data
      gage_data = fread(USGS_webpage, header = FALSE, skip = increment + 1, na.strings=c("NA", "Ssn", "Ice", "Eqp", "Bkw", "Dry", "Zfl", "Pr", "Rat", "Fld", "Dis", "--", "Mnt", "***")) # na.strings should change all strings I've listed to NA value, which is fine since we simply won't use that time period
      colnames(gage_data) = col_names[[1]]
      
      
      # creating a new data frame using only streamflow, change in streamflow, and dates
      streamy = cbind.data.frame(as.Date(gage_data$datetime), gage_data[, q_col_num, with = FALSE])
      colnames(streamy) = c("Date", "origQ")
      
      # converting to mm/day
      #sqkm = basinid$DRAIN_SQKM[which(basinid$STAID == gage_number)]		# converting to mm/day
      #conversion = 60*60*24 / (sqkm * 3280.84 * 3280.84 * 3280.84) * 1000000
      # converting to m^3 / day
      conversion = 2446.58
      
      streamy$origQ = as.numeric(streamy$origQ) * conversion
      streamy$dQ = NA                    # creating a column for dQ
      streamy$dQ[1:(length(streamy$origQ)-1)] = diff(streamy$origQ)
      minDeltaQ = min(abs(streamy$dQ[which(streamy$dQ != 0)]), na.rm=TRUE)
      streamy$Q = streamy$origQ + streamy$dQ / 2    #finding the mean value between Q measurements
      
      # identifying wet and dry periods
      streamy$wet = NA
      lag_days = c(7,15,30,45,90,180)
      for(num_days_lag in lag_days)	{
        streamy[,paste0("wet_",num_days_lag)] = 9999
        col_num = which(names(streamy) == paste0("wet_",num_days_lag))
        streamy$wet = NA
        for(end_day in (num_days_lag+1):(nrow(streamy)-1))	{
          days_inc = (end_day - num_days_lag):end_day
          days_nona = which(!is.na(streamy$Q[days_inc]))
          streamy$wet[end_day] = sum(streamy$Q[days_inc], na.rm=TRUE) / length(days_nona)
        }
        quantile_Q = quantile(streamy$wet, probs=c(.8,.2), na.rm=TRUE)
        high_flows = which(streamy$wet > quantile_Q[1])
        low_flows = which(streamy$wet < quantile_Q[2])
        streamy[high_flows, paste0("wet_",num_days_lag)] = 2
        streamy[low_flows, paste0("wet_",num_days_lag)] = 1
      }
      
      
      ################################################################################
      ####    Part 2
      ####    Identifying Recession Events
      ################################################################################
      
      #    identifying days where recession is occurring
      days_to_remove = unique(c(
        which(streamy$dQ >= 0),    # removes days of non-decreasing streamflow
        which(streamy$Q <= 0),    #    removes days of zero streamflow
        if(decreasing_dQ){
          c(which(diff(streamy$dQ) == 0) + 1)    #removes days where rate of streamflow decay increases
        } else    {
          c(which(abs(streamy$dQ) == minDeltaQ))
        },
        which(is.na(streamy$Q)),    # removes days of missing streamflow values
        which(is.na(streamy$dQ))    # removes days of missing dQ values, i.e. the final day
      ))               
      strmfl = streamy[!days_to_remove, ]
      
      recess_length = minRecessLength	# the days of monontonic recession required to be considered an event
      lagdiff = diff(strmfl$Date, lag = recess_length) 
      longrecess = which(lagdiff == recess_length)
      keepers = NULL
      for(i in (rm_Ith):(recess_length-rm_Nth))	{
        keepers = c(keepers, longrecess + i)
      }
      
      
      
      # this section is for making plots illustrating how we are identifying the most appropriate time lag for disaggregating recession data
      par(mfrow=c(2,3), oma=c(5,4,0,0), mar=c(2,2,1,1))
      stream_recess = strmfl[sort(unique(keepers)),]	# new matrix only including periods of long recession
      plot(log10(stream_recess$Q), log10(-stream_recess$dQ), pch=16, col='grey90', ylab=NA, xlab=NA, axes=FALSE)
      #box(col='grey10', lwd=1.5)
      xtick=c(4,5,6)
      axis(side=1, at=xtick, labels=FALSE, tck=-0.025, col='grey10')
      text(x=xtick, par('usr')[3], labels=xtick, srt=0, pos=1, las=0, xpd=TRUE, cex=1.25, col='grey10')
      ytick=c(2,4,6)
      axis(side=2, at=ytick, labels=FALSE, tck=-0.025)
      text(par('usr')[1], y=ytick, labels=ytick, srt=0, pos=2, las=0, xpd=TRUE, cex=1.25, col='grey10')
      
      dry_days = which(stream_recess$wet_180 == 1)
      med_Qs = c(median(log10(stream_recess$Q[dry_days])), median(log10(stream_recess$Q[wet_days])))
      wet_days = which(stream_recess$wet_180 == 2)
      med_dQs = c(median(log10(-stream_recess$dQ[dry_days])), median(log10(-stream_recess$dQ[wet_days])))
      points(log10(stream_recess$Q[dry_days]), log10(-stream_recess$dQ[dry_days]), col='firebrick4', lwd=1)
      points(log10(stream_recess$Q[wet_days]), log10(-stream_recess$dQ[wet_days]), col='royalblue4', lwd=1)
      
      points(med_Qs[1], med_dQs[1], pch=21, col='grey95', bg='grey95', cex=3.5)
      points(med_Qs[2], med_dQs[2], pch=21, col='grey95', bg='grey95', cex=3.5)
      lines(med_Qs, med_dQs, col='grey95', lwd=5)
      lines(med_Qs, med_dQs, col='grey20', lwd=3)
      distance = sqrt(abs(diff(med_Qs))^2 + abs(diff(med_dQs))^2)
      text(5.5,2.5,paste("euc dist"), cex=1.2, col='grey20')
      text(5.5,2,paste(round(distance, 2)), cex=1.2, col='grey20')
      text(3.95,5.5,paste("days lag"), cex=1.2, col='grey20')
      text(3.95,5, 180, cex=1.2, col='grey20')
      
      points(med_Qs[1], med_dQs[1], pch=21, col='grey20', bg='firebrick1', cex=2.7, lwd=3)
      points(med_Qs[2], med_dQs[2], pch=21,col='grey20', bg='royalblue1', cex=2.7, lwd=3)
      
      
      
      
      
      
      
      events_captured = nrow(stream_recess)
      
      ##    <-- set variable tot_events
      recess_char[run, "tot_days"] = events_captured
      
      
      ################################################################################
      ####    Part 3
      ####    Recession Analysis
      ################################################################################
      if(events_captured >= minDays)   {       
        
        # log transformations of the data
        stream_recess$lg_dQ = log10(abs(stream_recess$dQ))
        stream_recess$lg_Q = log10(stream_recess$Q)
        
        # linear model for all data
        allmod = lm(stream_recess$lg_dQ ~ stream_recess$lg_Q)
        ##    <-- set variable allmod_slope
        recess_char[run, "allmod_slope"] = allmod$coef[slope]
        recess_char[run, "allmod_a"] = allmod$coef[intercept]
        
        
        # envelope regression
        # first, determing which set of point clouds is maximally disaggregated
        best_euc_dist = 0
        best_run = NA
        for(wetdry_run in lag_days)	{
          wetdry_col = which(names(stream_recess) == paste0("wet_",wetdry_run))
          the_drys = which(stream_recess[,..wetdry_col] == 1)
          the_wets = which(stream_recess[,..wetdry_col] == 2)
          if(length(the_drys) > 15 && length(the_wets) >= 15)	{
            med_dry_pts = c(median(stream_recess$lg_Q[the_drys]), median(stream_recess$lg_dQ[the_drys]))
            med_wet_pts = c(median(stream_recess$lg_Q[the_wets],na.rm=TRUE), median(stream_recess$lg_dQ[the_wets]))
            dist_pts = med_wet_pts - med_dry_pts
            euc_dist = sqrt(sum(dist_pts^2))
            if(euc_dist > best_euc_dist)	{
              best_euc_dist = euc_dist
              best_run = wetdry_col
            }
          }
        }
        
        wet_days = which(stream_recess[,..best_run] == 2)
        dry_days = which(stream_recess[,..best_run] == 1)
        
        recess_char[run, "timelag_diff"] = lag_days[best_run-5]			# this is not a great way to handle identifying the best time delay... need to reassess later
        
        
        if(length(dry_days) >=25)	{
          
          nmbrOfBins = 5	# number of bins for envelope regression
          nPerBin_wet = length(wet_days) %/% nmbrOfBins
          xtrs = length(wet_days) %% nmbrOfBins    # extra data points (the remainder from division)
          stream_recess$wet_bin_Qs = NA
          stream_recess$wet_bin_dQs = NA
          if(xtrs > 0)    {
            longs = rep(1:xtrs, each=nPerBin_wet+1)    # some bins get one extra data point (I chose the earlier bins for no particular reason)
            shorts = rep((xtrs+1):nmbrOfBins, each=nPerBin_wet)    # bins which don't get the extra data point
            
            stream_recess$wet_bin_Qs[wet_days][order(stream_recess$lg_Q[wet_days])] = c(longs,shorts)    # new column which identifies which bin each data point goes into by 'projecting up'
            stream_recess$wet_bin_dQs[wet_days][order(stream_recess$lg_dQ[wet_days])] = c(longs,shorts)    # new column which identifies which bin each data point goes into by 'projecting left'
          } else    {
            stream_recess$wet_bin_Qs[wet_days][order(stream_recess$lg_Q[wet_days])] = rep(1:nmbrOfBins, each = nPerBin_wet)
            stream_recess$wet_bin_dQs[wet_days][order(stream_recess$lg_dQ[wet_days])] = rep(1:nmbrOfBins, each = nPerBin_wet)
          }
          
          nPerBin_dry = length(dry_days) %/% nmbrOfBins
          xtrs = length(dry_days) %% nmbrOfBins    # extra data points (the remainder from division)
          stream_recess$dry_bin_Qs = NA
          stream_recess$dry_bin_dQs = NA
          if(xtrs > 0)    {
            longs = rep(1:xtrs, each=nPerBin_dry+1)    # some bins get one extra data point (I chose the earlier bins for no particular reason)
            shorts = rep((xtrs+1):nmbrOfBins, each=nPerBin_dry)    # bins which don't get the extra data point
            
            stream_recess$dry_bin_Qs[dry_days][order(stream_recess$lg_Q[dry_days])] = c(longs,shorts)    # new column which identifies which bin each data point goes into by 'projecting up'
            stream_recess$dry_bin_dQs[dry_days][order(stream_recess$lg_dQ[dry_days])] = c(longs,shorts)    # new column which identifies which bin each data point goes into by 'projecting left'
          } else    {
            stream_recess$dry_bin_Qs[dry_days][order(stream_recess$lg_Q[dry_days])] = rep(1:nmbrOfBins, each = nPerBin_dry)
            stream_recess$dry_bin_dQs[dry_days][order(stream_recess$lg_dQ[dry_days])] = rep(1:nmbrOfBins, each = nPerBin_dry)
          }
          
          
          medQs_dry = NULL
          maxQs_dry = NULL
          meddQs_dry = NULL
          mindQs_dry = NULL
          numBotVals_dry = ceiling(nPerBin_dry * .25)    # for selecting the events which form the upper/lower envelope
          medQs_wet = NULL
          maxQs_wet = NULL
          meddQs_wet = NULL
          mindQs_wet = NULL
          numBotVals_wet = ceiling(nPerBin_wet * .25)    # for selecting the events which form the upper/lower envelope
          for(i in 1:nmbrOfBins)    {
            valLocs_byQ_dry = which(stream_recess$dry_bin_Qs == i)
            valLocs_bydQ_dry = which(stream_recess$dry_bin_dQs == i)
            
            medQs_dry[i] = median(stream_recess[valLocs_byQ_dry,"lg_Q"][[1]])
            mindQs_dry[i] = median(head(sort(stream_recess[valLocs_byQ_dry,"lg_dQ"][[1]]), numBotVals_dry))
            
            maxQs_dry[i] = median(tail(sort(stream_recess[valLocs_bydQ_dry, "lg_Q"][[1]]), numBotVals_dry))
            meddQs_dry[i] = median(stream_recess[valLocs_bydQ_dry,"lg_dQ"][[1]])
            
            valLocs_byQ_wet = which(stream_recess$wet_bin_Qs == i)
            valLocs_bydQ_wet = which(stream_recess$wet_bin_dQs == i)
            
            medQs_wet[i] = median(stream_recess[valLocs_byQ_wet,"lg_Q"][[1]])
            mindQs_wet[i] = median(head(sort(stream_recess[valLocs_byQ_wet,"lg_dQ"][[1]]), numBotVals_wet))
            
            maxQs_wet[i] = median(tail(sort(stream_recess[valLocs_bydQ_wet, "lg_Q"][[1]]), numBotVals_wet))
            meddQs_wet[i] = median(stream_recess[valLocs_bydQ_wet,"lg_dQ"][[1]])
          }
          
          
          uppModList_dry = list()
          lowModList_dry = list()
          uppModList_wet = list()
          lowModList_wet = list()
          modNum = 0
          uppMinRMSE_dry = NULL
          lowMinRMSE_dry = NULL
          uppMinRMSE_wet = NULL
          lowMinRMSE_wet = NULL
          for(j in 3:nmbrOfBins)		{
            modNum = modNum+1
            uppmod_dry = lm(meddQs_dry[(j-2):nmbrOfBins] - 3 * maxQs_dry[(j-2):nmbrOfBins] ~ 1)
            uppMinRMSE_dry = c(uppMinRMSE_dry, sqrt(c(crossprod(uppmod_dry$residuals)) / length(uppmod_dry$residuals)))
            uppModList_dry[[modNum]] = uppmod_dry
            
            uppmod_wet = lm(meddQs_wet[(j-2):nmbrOfBins] - 3 * maxQs_wet[(j-2):nmbrOfBins] ~ 1)
            uppMinRMSE_wet = c(uppMinRMSE_wet, sqrt(c(crossprod(uppmod_wet$residuals)) / length(uppmod_wet$residuals)))
            uppModList_wet[[modNum]] = uppmod_wet
            
            lowmod_dry = lm(mindQs_dry[1:j] - 1 * medQs_dry[1:j] ~ 1)
            lowMinRMSE_dry = c(lowMinRMSE_dry, sqrt(c(crossprod(lowmod_dry$residuals)) / length(lowmod_dry$residuals)))
            lowModList_dry[[modNum]] = lowmod_dry
            
            lowmod_wet = lm(mindQs_wet[1:j] - 1 * medQs_wet[1:j] ~ 1)
            lowMinRMSE_wet = c(lowMinRMSE_wet, sqrt(c(crossprod(lowmod_wet$residuals)) / length(lowmod_wet$residuals)))
            lowModList_wet[[modNum]] = lowmod_wet
          }
          
          if(run %% 2 == 0)	{
            plot(stream_recess$lg_Q, stream_recess$lg_dQ)
            points(stream_recess$lg_Q[dry_days], stream_recess$lg_dQ[dry_days], col='red3', lwd=2)
            points(stream_recess$lg_Q[wet_days], stream_recess$lg_dQ[wet_days], col='blue3', lwd=2)
            points(medQs_dry, mindQs_dry, lwd=8, col='grey90', pch=3)
            points(maxQs_dry, meddQs_dry, lwd=8, col='grey90', pch=2)
            points(medQs_wet, mindQs_wet, lwd=8, col='grey90', pch=3)
            points(maxQs_wet, meddQs_wet, lwd=8, col='grey90', pch=2)
            
            points(medQs_dry, mindQs_dry, lwd=3, col='red4', pch=3)
            points(maxQs_dry, meddQs_dry, lwd=3, col='red1', pch=2)
            points(medQs_wet, mindQs_wet, lwd=3, col='blue4', pch=3)
            points(maxQs_wet, meddQs_wet, lwd=3, col='blue1', pch=2)
            
            abline(uppModList_wet[[which.min(uppMinRMSE_wet)]]$coefficients, 3, col='blue1', lwd=3, lty=2)
            abline(lowModList_wet[[which.min(lowMinRMSE_wet)]]$coefficients, 1, col='blue4', lwd=3, lty=2)
            abline(uppModList_dry[[which.min(uppMinRMSE_dry)]]$coefficients, 3, col='red1', lwd=3, lty=2)
            abline(lowModList_dry[[which.min(lowMinRMSE_dry)]]$coefficients, 1, col='red4', lwd=3, lty=2)
          }
          
          
          # for making plots illustrating the determination of the upper envelopes
          #				plot(stream_recess$lg_Q, stream_recess$lg_dQ, pch=16, col='grey90', ylab=NA, xlab=NA, axes=FALSE)
          #				#box(col='grey10', lwd=1.5)
          #				xtick=c(4,5,6)
          #				axis(side=1, at=xtick, labels=FALSE, tck=-0.025, col='grey10')
          #				text(x=xtick, par('usr')[3], labels=xtick, srt=0, pos=1, las=0, xpd=TRUE, cex=1.75, col='grey10')
          #				ytick=c(2,4,6)
          #				axis(side=2, at=ytick, labels=FALSE, tck=-0.025)
          #				text(par('usr')[1], y=ytick, labels=ytick, srt=0, pos=2, las=0, xpd=TRUE, cex=1.75, col='grey10')
          
          #				points(stream_recess$lg_Q[dry_days], stream_recess$lg_dQ[dry_days], col='firebrick4', lwd=2)
          #				points(stream_recess$lg_Q[wet_days], stream_recess$lg_dQ[wet_days], col='royalblue4', lwd=2)
          
          #				points(maxQs_dry, meddQs_dry, cex=2.5, bg='grey90',col='grey90', pch=24)
          #				points(maxQs_wet, meddQs_wet, cex=2.5, bg='grey90',col='grey90', pch=24)
          #				points(maxQs_dry, meddQs_dry, cex=2, bg='firebrick4',col='grey20', pch=24)
          #				points(maxQs_wet, meddQs_wet, cex=2, bg='royalblue4',col='grey20', pch=24)
          
          #				bestmod_upp_dry = which.min(uppMinRMSE_dry)
          #				bestmod_upp_wet = which.min(uppMinRMSE_wet)
          #				abline(uppModList_wet[[bestmod_upp_wet]]$coefficients, 3, col='royalblue1', lwd=3, lty=1)
          #				abline(uppModList_dry[[bestmod_upp_dry]]$coefficients, 3, col='firebrick1', lwd=3, lty=1)
          #				points(maxQs_dry[(bestmod_upp_dry):5], meddQs_dry[(bestmod_upp_dry):5], cex=2, bg='firebrick1',col='grey20', pch=24)
          #				points(maxQs_wet[(bestmod_upp_wet):5], meddQs_wet[(bestmod_upp_wet):5], cex=2, bg='royalblue1',col='grey20', pch=24)
          
          # for making plots illustrating the determination of the upper envelopes
          #				plot(stream_recess$lg_Q, stream_recess$lg_dQ, pch=16, col='grey90', ylab=NA, xlab=NA, axes=FALSE)
          #				#box(col='grey10', lwd=1.5)
          #				xtick=c(4,5,6)
          #				axis(side=1, at=xtick, labels=FALSE, tck=-0.025, col='grey10')
          #				text(x=xtick, par('usr')[3], labels=xtick, srt=0, pos=1, las=0, xpd=TRUE, cex=1.75, col='grey10')
          #				ytick=c(2,4,6)
          #				axis(side=2, at=ytick, labels=FALSE, tck=-0.025)
          #				text(par('usr')[1], y=ytick, labels=ytick, srt=0, pos=2, las=0, xpd=TRUE, cex=1.75, col='grey10')
          
          #				points(stream_recess$lg_Q[dry_days], stream_recess$lg_dQ[dry_days], col='firebrick4', lwd=2)
          #				points(stream_recess$lg_Q[wet_days], stream_recess$lg_dQ[wet_days], col='royalblue4', lwd=2)
          
          #				points(medQs_dry, mindQs_dry, cex=2.5, bg='grey90',col='grey90', pch=22)
          #				points(medQs_wet, mindQs_wet, cex=2.5, bg='grey90',col='grey90', pch=22)
          #				points(medQs_dry, mindQs_dry, cex=2, bg='firebrick4',col='grey20', pch=22)
          #				points(medQs_wet, mindQs_wet, cex=2, bg='royalblue4',col='grey20', pch=22)
          
          #				bestmod_low_dry = which.min(lowMinRMSE_dry)
          #				bestmod_low_wet = which.min(lowMinRMSE_wet)
          #				abline(lowModList_wet[[bestmod_low_wet]]$coefficients, 1, col='royalblue1', lwd=3, lty=1)
          #				abline(lowModList_dry[[bestmod_low_dry]]$coefficients, 1, col='firebrick1', lwd=3, lty=1)
          #				points(medQs_dry[1:(2+bestmod_low_dry)], mindQs_dry[1:(2+bestmod_low_dry)], cex=2, bg='firebrick1',col='grey20', pch=22)
          #				points(medQs_wet[1:(2+bestmod_low_wet)], mindQs_wet[1:(2+bestmod_low_wet)], cex=2, bg='royalblue1',col='grey20', pch=22)
          
          
          
          recess_char[run, "dry_rightmod_three_a"] =  uppModList_dry[[which.min(uppMinRMSE_dry)]]$coefficients
          recess_char[run, "wet_rightmod_three_a"] = uppModList_wet[[which.min(uppMinRMSE_wet)]]$coefficients
          recess_char[run, "dry_lowmod_one_a"] =  lowModList_dry[[which.min(lowMinRMSE_dry)]]$coefficients
          recess_char[run, "wet_lowmod_one_a"] =  lowModList_wet[[which.min(lowMinRMSE_wet)]]$coefficients
          
          uppModList_dry = list()
          lowModList_dry = list()
          uppModList_wet = list()
          lowModList_wet = list()
          modNum = 0
          uppMinRMSE_dry = NULL
          lowMinRMSE_dry = NULL
          uppMinRMSE_wet = NULL
          lowMinRMSE_wet = NULL
          for(j in 3:nmbrOfBins)		{
            modNum = modNum+1
            uppmod_dry = lm(meddQs_dry[(j-2):nmbrOfBins] - 1.5 * maxQs_dry[(j-2):nmbrOfBins] ~ 1)
            uppMinRMSE_dry = c(uppMinRMSE_dry, sqrt(c(crossprod(uppmod_dry$residuals)) / length(uppmod_dry$residuals)))
            uppModList_dry[[modNum]] = uppmod_dry
            
            uppmod_wet = lm(meddQs_wet[(j-2):nmbrOfBins] - 1.5 * maxQs_wet[(j-2):nmbrOfBins] ~ 1)
            uppMinRMSE_wet = c(uppMinRMSE_wet, sqrt(c(crossprod(uppmod_wet$residuals)) / length(uppmod_wet$residuals)))
            uppModList_wet[[modNum]] = uppmod_wet
            
            lowmod_dry = lm(mindQs_dry[1:j] - 1.5 * medQs_dry[1:j] ~ 1)
            lowMinRMSE_dry = c(lowMinRMSE_dry, sqrt(c(crossprod(lowmod_dry$residuals)) / length(lowmod_dry$residuals)))
            lowModList_dry[[modNum]] = lowmod_dry
            
            lowmod_wet = lm(mindQs_wet[1:j] - 1.5 * medQs_wet[1:j] ~ 1)
            lowMinRMSE_wet = c(lowMinRMSE_wet, sqrt(c(crossprod(lowmod_wet$residuals)) / length(lowmod_wet$residuals)))
            lowModList_wet[[modNum]] = lowmod_wet
          }
          
          recess_char[run, "dry_rightmod_onefive_a"] =  uppModList_dry[[which.min(uppMinRMSE_dry)]]$coefficients
          recess_char[run, "wet_rightmod_onefive_a"] = uppModList_wet[[which.min(uppMinRMSE_wet)]]$coefficients
          recess_char[run, "dry_lowmod_onefive_a"] =  lowModList_dry[[which.min(lowMinRMSE_dry)]]$coefficients
          recess_char[run, "wet_lowmod_onefive_a"] =  lowModList_wet[[which.min(lowMinRMSE_wet)]]$coefficients
          
        }
      }
    }
  }
}   
#write.csv(recess_char, "dryandwet_test_4dec2019.csv")







##############################################################################
####    Part 2
####    Estimating K and D for the continental US
################################################################################
# reading in the recession analysis
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3")#C:\\Users\\arikt\\OneDrive\\Documents\\PhD\\Research\\D2")
recess_char = read.csv("dryandwet_jan2020.csv", colClasses=c("gage"="character"))


# merging with USGS GAGES-II data in order to solve recession equations
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D2\\spreadsheets-in-csv-format")#C:\\Users\\arikt\\OneDrive\\Documents\\PhD\\Research\\D2\\spreadsheets-in-csv-format")
all_gages = read.csv("conterm_bas_classif.txt", colClasses = c("STAID" = "character", "AGGECOREGION" = "character")) #   read in the file
ref_gages = which(all_gages$CLASS == "Ref")     #   identify which basins are chategorized as reference basins
recess_char$gage = all_gages$STAID[ref_gages]



########
## note: the gages II data was used for an earlier version of model building; it is better than the NHDPlus data, but does not extend to all watersheds, so I'm not using it anymore
########
# reading in the GAGES-II catchment summary data
basinid = read.csv("conterm_basinid.txt", colClasses = c("STAID" = "character"))
hydro = read.csv('conterm_hydro.txt', colClasses = c('STAID' = 'character'))
topo = read.csv('conterm_topo.txt', colClasses = c('STAID' = 'character'))

basin_chars = merge(recess_char, basinid, by.x='gage', by.y='STAID')
basin_chars = merge(basin_chars, hydro, by.x='gage', by.y='STAID')
basin_chars = merge(basin_chars, topo, by.x='gage', by.y='STAID')





#####################################################################################################################
############### Estimating K and D, and Sensitivity testing 
####################################################################################################################
library("polynom")

###################################
##### Brut 1994
# early
#			b1=3
#			a1 = 1.133 / (k * f * D^3 * L^2 * cos(i))
# late
#			b2=1
#			a2 = ((pi^2 * p * k * D * L^2) / (f * A^2)) * cos(i) * (1 + (eta / (pi * p))^2)
#			a2 = ((pi^2 * p * k * D * L^2) / (f * A^2)) * cos(i * (1 + (eta / (pi * p))^2))
#		eta = (B/D) * tan(i)

# unknowns
# k hydraulic conductivity
# f drainable porosity	# ranges from 0 to 35
# D depth of aquifer
# p ; set to 1? typical range from 0 to 1; in brutsaert 1994 for horizontal aq btw 1/3 and 1/2
# i = slope
# A = total aquifer area; A = 2BL
# Bi = length from stream to hillslope boundary; B * cos(i) = Bi
# B = Bi / cos(i)
# L = length of stream network = Wo

# solve 0 = D(sqrt((10^a1)*(10^a2)/1.133) * (f*A*sqrt(p)) * D - pi*p) - B*tan(i)
Wo = basin_chars$STREAMS_KM_SQ_KM * basin_chars$DRAIN_SQKM * 1000	# stream length in meters
A = basin_chars$DRAIN_SQKM * 1000 *1000	# basin area in square meters
f = 0.1
p = 1
i_slp = atan((1 + basin_chars$SLOPE_PCT) / 100) 
#			i_slp = .05

##	for dry periods
a1 = 10^basin_chars$dry_rightmod_three_a
a2 = 10^basin_chars$dry_lowmod_one_a
B = (A / (2*Wo)) / cos(i_slp) 

p1 = sqrt(a1*a2/1.133) * (f*A*sqrt(p))
p2 = pi*p
p3 = B*tan(i_slp)
notnas = which(!is.na(p1))
depth = NULL
for (i in notnas)	{
  pp = polynomial(c(-p3[i], -p2, p1[i]))
  depth = c(depth, solve(pp)[2])
}
basin_chars$B_depth_dry = NA
basin_chars$B_depth_dry[notnas] = depth
D = basin_chars$B_depth_dry
#D = 10
#D=10

# solve (0 = D(p1*D - p2) - B*tan(i))
# or -p3 - p2x + x^2 * p1
# for k
k1 = (1.133 / (cos(i_slp)*a1*f*D^3*Wo^2)) *100/(60*60*24)
k2 = (a2*f*A^2)/(pi^2*p*D*Wo^2*cos(i_slp)*(1+(B*tan(i_slp)/(pi*p*D))^2)) *100/(60*60*24)
basin_chars$B_K1_dry = k1
basin_chars$B_K2_dry = k2

range(k2, na.rm=T)
range(D, na.rm=T)

##	for wet periods
a1 = 10^basin_chars$wet_rightmod_three_a
a2 = 10^basin_chars$wet_lowmod_one_a
B = (A / (2*Wo)) / cos(i_slp) 

p1 = sqrt(a1*a2/1.133) * (f*A*sqrt(p))
p2 = pi*p
p3 = B*tan(i_slp)
notnas = which(!is.na(p1))
depth = NULL
for (i in notnas)	{
  pp = polynomial(c(-p3[i], -p2, p1[i]))
  depth = c(depth, solve(pp)[2])
}
basin_chars$B_depth_wet = NA
basin_chars$B_depth_wet[notnas] = depth
D = basin_chars$B_depth_wet
#D = 10
#D=10

# solve (0 = D(p1*D - p2) - B*tan(i))
# or -p3 - p2x + x^2 * p1
# for k
k1 = (1.133 / (cos(i_slp)*a1*f*D^3*Wo^2)) *100/(60*60*24)
k2 = (a2*f*A^2)/(pi^2*p*D*Wo^2*cos(i_slp)*(1+(B*tan(i_slp)/(pi*p*D))^2)) *100/(60*60*24)
basin_chars$B_K1_wet = k1
basin_chars$B_K2_wet = k2

range(k2, na.rm=T)
range(D, na.rm=T)




################################################################	
####Sensitivity testing for f, assuming typical range of 1% to 35%
sensD_f = NULL
sensK_f = NULL
#for(f in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1:35))	{
for(f in c(1:35))	{
  p1 = sqrt(a1*a2/1.133) * ((f/100)*A*sqrt(p))
  notnas = which(!is.na(p1))
  depth = NULL
  for (i in notnas)	{
    pp = polynomial(c(-p3[i], -p2, p1[i]))
    depth = c(depth, solve(pp)[2])
  }
  D = NA
  D[notnas] = depth
  k2 = (a2*(f/100)*A^2)/(pi^2*p*D*Wo^2*cos(i_slp)*(1+(B*tan(i_slp)/(pi*p*D))^2)) *100/(60*60*24)
  sensD_f = rbind(sensD_f, c(f, summary(D, na.rm=TRUE)))
  sensK_f = rbind(sensK_f, c(f, summary(k2, na.rm=TRUE)))
}	

#Sensitivity testing for L, assuming typical change in lengths of 0.5x to 2.5x of lengths
A = basin_chars$DRAIN_SQKM * 1000 *1000	# basin area in square meters
f = 0.1
p = 1
i_slp = atan((1 + basin_chars$SLOPE_PCT / 1) / 100) 
p1 = sqrt(a1*a2/1.133) * (f*A*sqrt(p))
sensD_L = NULL
sensK_L = NULL
for(L in seq(0.5,2.5,0.1))	{
  # solve 0 = D(sqrt((10^a1)*(10^a2)/1.133) * (f*A*sqrt(p)) * D - pi*p) - B*tan(i)
  Wo = L * basin_chars$STREAMS_KM_SQ_KM * basin_chars$DRAIN_SQKM * 1000	# stream length in meters
  B = (A / (2*Wo)) / cos(i_slp) 
  
  p3 = B*tan(i_slp)
  notnas = which(!is.na(p1))
  depth = NULL
  for (i in notnas)	{
    pp = polynomial(c(-p3[i], -p2, p1[i]))
    depth = c(depth, solve(pp)[2])
  }
  D = NA
  D[notnas] = depth
  k2 = (a2*f*A^2)/(pi^2*p*D*Wo^2*cos(i_slp)*(1+(B*tan(i_slp)/(pi*p*D))^2)) *100/(60*60*24)
  sensD_L = rbind(sensD_L, c(L, summary(D, na.rm=TRUE)))
  sensK_L = rbind(sensK_L, c(L, summary(k2, na.rm=TRUE)))
}	


#Sensitivity testing for p, assuming range of 0.25 to 1
Wo = basin_chars$STREAMS_KM_SQ_KM * basin_chars$DRAIN_SQKM * 1000	# stream length in meters
A = basin_chars$DRAIN_SQKM * 1000 *1000	# basin area in square meters
f = 0.1
B = (A / (2*Wo)) / cos(i_slp) 
sensD_p = NULL
sensK_p = NULL
for(p in seq(0.3,1,0.05))	{
  # solve 0 = D(sqrt((10^a1)*(10^a2)/1.133) * (f*A*sqrt(p)) * D - pi*p) - B*tan(i)
  p1 = sqrt(a1*a2/1.133) * (f*A*sqrt(p))
  p2 = pi*p
  p3 = B*tan(i_slp)
  notnas = which(!is.na(p1))
  depth = NULL
  for (i in notnas)	{
    pp = polynomial(c(-p3[i], -p2, p1[i]))
    depth = c(depth, solve(pp)[2])
  }
  D = NA
  D[notnas] = depth
  k2 = (a2*f*A^2)/(pi^2*p*D*Wo^2*cos(i_slp)*(1+(B*tan(i_slp)/(pi*p*D))^2)) *100/(60*60*24)
  sensD_p = rbind(sensD_p, c(p, summary(D, na.rm=TRUE)))
  sensK_p = rbind(sensK_p, c(p, summary(k2, na.rm=TRUE)))
}	

############
#!!!! to convert meters (depth) into storage in mm, use
#	sensD_p[,-1] = sensD_p[,-1] * 100; sensD_L[,-1] = sensD_L[,-1] * 100; sensD_f[,-1] = sensD_f[,-1] * 1000 * sensD_f[,1] / 100
# making the plots of sensitivity analysis
png(paste0("sensitivity_of_depth.png"), width=400, height=600)
par(cex=9, mfrow=(c(3,1)), mar=c(4.5,5,1,0.5))

depth_range = range(c(sensD_p[,-c(1,8)], sensD_f[,-c(1,8)], sensD_L[,-c(1,8)]))

plot(sensD_p[,1], sensD_p[,4], type='l', lwd=3, ylim=depth_range, log='y',
     xlab="", ylab="", yaxt='n', xaxt='n')
title(xlab="p (fitting parameter)", line=2.8, cex.lab=2)
axis(1, at=sensD_p[c(1,5,10,15),1], labels=TRUE, cex.axis=1.7)
axis(2, at=c(10,100,1000,10000), cex.axis=1.7)
lines(sensD_p[,1], sensD_p[,3], lwd=2, lty=2)
lines(sensD_p[,1], sensD_p[,6], lwd=2, lty=2)
lines(sensD_p[,1], sensD_p[,2], lwd=1, lty=3)
lines(sensD_p[,1], sensD_p[,7], lwd=1, lty=3)


plot(sensD_f[,1], sensD_f[,4], type='l', lwd=3, ylim=depth_range, log='y',
     xlab="", ylab="", yaxt='n', xaxt='n')
title(xlab="f (drainable porosity in %)",line=2.8, cex.lab=2)
axis(1, at=sensD_f[c(5,15,25,35),1], labels=TRUE,cex.axis=1.7)
axis(2, at=c(10,100,1000,10000), cex.axis=1.7)
lines(sensD_f[,1], sensD_f[,3], lwd=2, lty=2)
lines(sensD_f[,1], sensD_f[,6], lwd=2, lty=2)
lines(sensD_f[,1], sensD_f[,2], lwd=1, lty=3)
lines(sensD_f[,1], sensD_f[,7], lwd=1, lty=3)


plot(sensD_L[,1], sensD_L[,4], type='l', lwd=3, ylim=depth_range, log='y',
     xlab="", ylab="", yaxt='n', xaxt='n')
title(xlab="scalar of L (stream network length)", line=2.8, cex.lab=2)
axis(1, at=sensD_L[c(1,6,11,16,21),1], labels=TRUE, cex.axis=1.7)
axis(2, at=c(10,100,1000,10000), cex.axis=1.7)
lines(sensD_L[,1], sensD_L[,3], lwd=2, lty=2)
lines(sensD_L[,1], sensD_L[,6], lwd=2, lty=2)
lines(sensD_L[,1], sensD_L[,2], lwd=1, lty=3)
lines(sensD_L[,1], sensD_L[,7], lwd=1, lty=3)

mtext(text="S (effective aquifer storage in mm)", side=2,line=-2.1,outer=TRUE, cex=1.4)
dev.off()



png(paste0("sensitivity_of_K.png"), width=400, height=600)
par(cex=9, mfrow=(c(3,1)), mar=c(4.5,5,1,0.5))

k_range = range(c(sensK_p[,-c(1,8)], sensK_f[,-c(1,8)], sensK_L[,-c(1,8)]))
plot(sensK_p[,1], sensK_p[,4], type='l', lwd=3, ylim=k_range, log='y',
     xlab="", ylab="", yaxt='n', xaxt='n')
title(xlab="p (fitting parameter)", line=2.8, cex.lab=2)
axis(1, at=sensK_p[c(1,5,10,15),1], labels=TRUE, cex.axis=1.7)
axis(2, at=c(.000001,.0001,0.01,1), labels=c("-6","-4","-2","0"), cex.axis=1.7)
lines(sensK_p[,1], sensK_p[,3], lwd=2, lty=2)
lines(sensK_p[,1], sensK_p[,6], lwd=2, lty=2)
lines(sensK_p[,1], sensK_p[,2], lwd=1, lty=3)
lines(sensK_p[,1], sensK_p[,7], lwd=1, lty=3)

plot(sensK_f[,1], sensK_f[,4], type='l', lwd=3, ylim=k_range, log='y',
     xlab="", ylab="", yaxt='n', xaxt='n')
title(xlab="f (drainable porosity in %)",line=2.8, cex.lab=2)
axis(1, at=sensK_f[c(5,15,25,35),1], labels=TRUE,cex.axis=1.7)
axis(2, at=c(.000001,.0001,0.01,1), labels=c("-6","-4","-2","0"), cex.axis=1.7)
lines(sensK_f[,1], sensK_f[,3], lwd=2, lty=2)
lines(sensK_f[,1], sensK_f[,6], lwd=2, lty=2)
lines(sensK_f[,1], sensK_f[,2], lwd=1, lty=3)
lines(sensK_f[,1], sensK_f[,7], lwd=1, lty=3)

plot(sensK_L[,1], sensK_L[,4], type='l', lwd=3, ylim=k_range, log='y',
     xlab="", ylab="", yaxt='n', xaxt='n')
title(xlab="scalar of L (stream network length)", line=2.8, cex.lab=2)
axis(1, at=sensK_L[c(1,6,11,16,21),1], labels=TRUE, cex.axis=1.7)
axis(2, at=c(.000001,.0001,0.01,1), labels=c("-6","-4","-2","0"), cex.axis=1.7)
lines(sensK_L[,1], sensK_L[,3], lwd=2, lty=2)
lines(sensK_L[,1], sensK_L[,6], lwd=2, lty=2)
lines(sensK_L[,1], sensK_L[,2], lwd=1, lty=3)
lines(sensK_L[,1], sensK_L[,7], lwd=1, lty=3)

mtext(text="log10(K) (effective hydraulic conductivity in cm/s)", side=2,line=-2.1,outer=TRUE, cex=1.4)
dev.off()



k_range = range(sensK_p[,c(3,6)], sensK_f[,c(3,6)], sensK_L[,c(3,6)])
k_range = range(sensK_p[16,])
#rel_range_p = sensK_p[,1] - min(sensK_p[,1])	; rel_range_p = rel_range_p / max(rel_range_p)
plot(sensK_p[,1], sensK_p[,4], type='l', lwd=3, ylim=k_range, log='y',
     xaxt='n')
axis(1, at=sensK_p[c(1,6,11,16),1], labels=TRUE)
#axis(2, at=c(0.1,1,10,100), labels=TRUE)
lines(sensK_p[,1], sensK_p[,3], lwd=2, lty=2)
lines(sensK_p[,1], sensK_p[,6], lwd=2, lty=2)

#rel_range_f = sensK_f[,1] - min(sensK_f[,1])	; rel_range_f = rel_range_f / max(rel_range_f)
plot(sensK_f[,1], sensK_f[,4], type='l', lwd=3, ylim=k_range, log='y',
     xaxt='n')
axis(1, at=sensK_f[c(5,15,25,35),1], labels=TRUE)
#axis(2, at=c(0.1,1,10,100), labels=TRUE)
lines(sensK_f[,1], sensK_f[,3], lwd=2, lty=2)
lines(sensK_f[,1], sensK_f[,6], lwd=2, lty=2)

#rel_range_L = sensK_L[,1] - min(sensK_L[,1])	; rel_range_L = rel_range_L / max(rel_range_L)
plot(sensK_L[,1], sensK_L[,4], type='l', lwd=3, ylim=k_range, log='y',
     xaxt='n')
axis(1, at=sensK_L[c(1,6,11,16,21),1], labels=TRUE)
lines(sensK_L[,1], sensK_L[,3], lwd=2, lty=2)
lines(sensK_L[,1], sensK_L[,6], lwd=2, lty=2)









library(viridis)
library("ggplot2")
library("sf")
library("rnaturalearth")	# for base map
library("rnaturalearthdata")	# for downlading lakes and rivers data
library("ggspatial")		# for scale bar
#library(rgdal)
#library(raster)
library(rgeos)
library(stars)
#library(rasterize)
library(spatial)
######################################################
#### For producing maps of GAGES-II reference watersheds
# the lines commented out are for integrating the rasterized glhymps dataset into 
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\watersheds-shapefile")
watersheds = st_read("basins01.shp")
for(i in 2:9)	{
  watersheds=rbind(watersheds, st_read(paste0("basins0",i,".shp")))
}
for(i in 10:18)	{
  watersheds=rbind(watersheds, st_read(paste0("basins",i,".shp")))
}	
merged = merge(basin_chars, watersheds, by.x='gage', by.y='SITE_NO')
merged <- st_as_sf(merged, crs=st_crs(42303))
transformed = st_transform(merged, crs=st_crs(4326))
bchars_sf_poly = subset(transformed, tot_days>500)
rm(merged)	;	rm(transformed)


# writing a summary of all S and K from recession analysis
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write.csv(rbind(summary(log10(bchars_sf_poly$B_K2_wet)),
                summary(log10(bchars_sf_poly$B_K2_dry)),
                summary(bchars_sf_poly$B_depth_wet *100),
                summary(bchars_sf_poly$B_depth_dry *100)), "summary_recessvars_all.csv")



setwd("C://Users//arik//Documents//PhD Research//D3//Figures")
large_to_small	= rev(order(bchars_sf_poly$DRAIN_SQKM))# reordering so small watersheds write over large watersheds
plot_data = bchars_sf_poly[large_to_small,]
nas_tobe_removed = which(is.na(plot_data$B_K2_dry))
plot_data = plot_data[-nas_tobe_removed,]

plot_data$K_dry = log10(plot_data$B_K2_dry)
plot_data$K_wet = log10(plot_data$B_K2_wet)

min_val = floor(min(c(plot_data$K_dry, plot_data$K_wet)))	# a cheat way to set limits on scale_color_viridis below, since native options are a mess
min_val_row = plot_data[1,]	; min_val_row$K_dry = min_val; min_val_row$K_wet = min_val
max_val = ceiling(max(c(plot_data$K_dry, plot_data$K_wet)))
max_val_row = plot_data[1,]	; max_val_row$K_dry = max_val ; max_val_row$K_wet = max_val
plot_data = rbind(min_val_row, max_val_row, plot_data)
my_breaks = c(-4,-2,0)

####  K (dry period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data, shape=16, alpha=1, aes(color=K_dry, fill=K_dry,  stroke=1))	+
  scale_color_viridis_c(name="log10(K)",option = "plasma",end=.9,
                        breaks = my_breaks, labels=my_breaks)	+
  scale_fill_viridis_c(name="log10(K)",option = "plasma",end=.9,
                       breaks = my_breaks, labels=my_breaks)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.18),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("K_dry_Brut1994.png", width = 12, height = 8, dpi = 150)


####	 K (wet period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data, shape=16, alpha=1, aes(color=K_wet, fill=K_wet,  stroke=1))	+
  scale_color_viridis_c(name="log10(K)",option = "plasma",end=.9,
                        breaks = my_breaks, labels=my_breaks)	+
  scale_fill_viridis_c(name="log10(K)",option = "plasma",end=.9,
                       breaks = my_breaks, labels=my_breaks)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.18),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("K_wet_Brut1994.png", width = 12, height = 8, dpi = 150)



large_to_small	= rev(order(bchars_sf_poly$DRAIN_SQKM))# reordering so small watersheds write over large watersheds
plot_data = bchars_sf_poly[large_to_small,]
nas_tobe_removed = which(is.na(plot_data$B_K2_dry))
plot_data = plot_data[-nas_tobe_removed,]

plot_data$S_dry = plot_data$B_depth_dry * f * 1000
plot_data$S_wet = plot_data$B_depth_wet * f * 1000

min_val = 10^floor(log10(min(c(plot_data$S_dry, plot_data$S_wet))))	# a cheat way to set limits on scale_color_viridis below, since native options are a mess
min_val_row = plot_data[1,]	; min_val_row$S_dry = min_val; min_val_row$S_wet = min_val
max_val = 10^ceiling(log10(max(c(plot_data$S_dry, plot_data$S_wet))))
max_val_row = plot_data[1,]	; max_val_row$S_dry = max_val ; max_val_row$S_wet = max_val
plot_data = rbind(min_val_row, max_val_row, plot_data)
my_breaks = c(1, 100, 10000)

####  S (dry period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data, shape=16, alpha=1, aes(color=S_dry, fill=S_dry,  stroke=1))	+
  scale_color_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                        breaks = my_breaks, labels=my_breaks)	+
  scale_fill_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                       breaks = my_breaks, labels=my_breaks)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.18),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("S_dry_Brut1994.png", width = 12, height = 8, dpi = 150)


####	S (wet period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data, shape=16, alpha=1, aes(color=S_wet, fill=S_wet,  stroke=1))	+
  scale_color_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                        breaks = my_breaks, labels=my_breaks)	+
  scale_fill_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                       breaks = my_breaks, labels=my_breaks)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.18),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("S_wet_Brut1994.png", width = 12, height = 8, dpi = 150)






















########
## combining calculated values of Ksat and D with NHDPlus catchment summary variables
library(feather)
aq_chars = basin_chars[,c("gage","region","HUC02","B_K1_dry","B_K1_wet","B_K2_dry","B_K2_wet","B_depth_dry","B_depth_wet", "tot_days")]
gg = subset(aq_chars, tot_days > 500)	# removing around 200 watersheds where data was not very 'robust' i.e. fewer than 501 days of recession
aq_chars = gg[,c("gage","HUC02",		# we've tried training the model with cross validation on Ecoregions, but HUC02s seems to work better
                 "B_K1_dry","B_K1_wet","B_K2_dry","B_K2_wet","B_depth_dry","B_depth_wet")] 

setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus")
gagesII_wNHDatts = merge(aq_chars, read_feather("gagesII_wNHDatts.feather"), by.x="gage", by.y="SOURCE_FEA")
gagesII_wNHDatts = gagesII_wNHDatts[!is.na(gagesII_wNHDatts[,3]),]		# eliminating watersheds for which we aren't able to calculated K or D; these should have been eliminated when we eliminated tot_days>500, but this is just to ensure no NAs slipped through

# creating new variables
# new seasonality variables: first WB or baseflow
first_month = which(names(gagesII_wNHDatts) == "TOT_WB5100_JAN")
new_bchars = gagesII_wNHDatts[,first_month:(first_month+11)]
for (i in 1:12)	{
  new_bchars[,i]	= abs(new_bchars[,i] - gagesII_wNHDatts$TOT_WB5100_ANN / 12)
}
gagesII_wNHDatts$Qbase_seasonality = apply(new_bchars, 1,sum) / gagesII_wNHDatts$TOT_WB5100_ANN
# new seasonality variables: next TAV or avg temperature
first_month = which(names(gagesII_wNHDatts) == "TOT_TAV7100_JAN")
new_bchars = gagesII_wNHDatts[,first_month:(first_month+11)]
kelv_converter = 273
for (i in 1:12)	{
  new_bchars[,i]	= abs(new_bchars[,i] + kelv_converter -
                         (gagesII_wNHDatts$TOT_TAV7100_ANN + kelv_converter) / 12)
}
gagesII_wNHDatts$TAV_seasonality = apply(new_bchars, 1,sum) / (gagesII_wNHDatts$TOT_TAV7100_ANN + kelv_converter)
# new PET to PPT ratio
gagesII_wNHDatts$PET_PPT = gagesII_wNHDatts$TOT_PET / gagesII_wNHDatts$TOT_PPT7100_ANN 
# eliminating vars that wont be used to build our model
gagesII_nousecols = c(1:9,11,16,131)				
gagesII_wNHDatts_vars = gagesII_wNHDatts[,-gagesII_nousecols]


# same variables organized by huc12
huc12_wNHDatts = read_feather("HUC12s_wNHDatts.feather")
huc12_nousecols = c(1,2,4,9)			# eliminating vars that wont be used to build our model
huc12_wNHDatts_vars = huc12_wNHDatts[,-huc12_nousecols]

# creating new variables
# new seasonality variable: first WB or baseflow
first_month = which(names(huc12_wNHDatts_vars) == "CAT_WB5100_JAN")
new_bchars = huc12_wNHDatts_vars[,first_month:(first_month+11)]
for (i in 1:12)	{
  new_bchars[,i]	= abs(new_bchars[,i] - huc12_wNHDatts_vars$CAT_WB5100_ANN / 12)
}
huc12_wNHDatts_vars$Qbase_seasonality = apply(new_bchars, 1,sum) / huc12_wNHDatts_vars$CAT_WB5100_ANN

# new seasonality variables: next TAV or avg temperature
first_month = which(names(huc12_wNHDatts_vars) == "CAT_TAV7100_JAN")
new_bchars = huc12_wNHDatts_vars[,first_month:(first_month+11)]
for (i in 1:12)	{new_bchars[,i]	= abs(new_bchars[,i] + kelv_converter -
                                        (gagesII_wNHDatts$TOT_TAV7100_ANN + kelv_converter) / 12)
}
huc12_wNHDatts_vars$TAV_seasonality = apply(new_bchars, 1,sum) / (huc12_wNHDatts_vars$CAT_TAV7100_ANN + kelv_converter)

# new PET to PPT ratio
huc12_wNHDatts_vars$PET_PPT = huc12_wNHDatts_vars$CAT_PET / huc12_wNHDatts_vars$CAT_PPT7100_ANN  

# renaming the huc12 vars (from CAT_ to TOT_) for simplifying model building below
names(huc12_wNHDatts_vars) <- names(gagesII_wNHDatts_vars)	
huc12_wNHDatts_vars$HUC_12 = huc12_wNHDatts$HUC_12





#setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus")
#nhd_huc12_streamCat_merge = st_read("nhd_streamCat_for_HUC12s.gpkg")













###########################################################################################
#####	modeling in ungauged basins
###########################################################################################
#merging estimates of the desired variable (K or D in wet or dry) from basin_chars onto NHD and StreamCat vars
library(caret)
library(CAST)

# RF without overtraining
######
## for each objective variable, we run the following procedure
## first we reduce our 124 variables down to a more manageable 30 by running iterations of RF models and removing the N least significant vars
## then we select an mtry value value with tuneLength
## then we use ffs() to select our best variables
## then we train our model on 80% of our data
## then we validate our model on 20% holdout

# Part 1: reducing 124 variables down to a more manageable number
wanted_var = gagesII_wNHDatts[,c("gage", "HUC02",						# region is ecoregion; also try with HUC02
                                 "B_depth_dry")]														# designates which aq property will be predicted
#wanted_var[,3] = log10(wanted_var[,3])									# log transform Ksat data

frst_hlr = which(names(gagesII_wNHDatts_vars) == "TOT_HLR_1")			# for removing HLRs from model building
hlr_cols = frst_hlr:(frst_hlr + 19)

predictors_df = cbind(wanted_var[,c(2,3)], gagesII_wNHDatts_vars[,-hlr_cols])
predictors_df[,-1] = signif(predictors_df[,-1], 3)	# rounding all numeric vals to 3 significant digits

indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[,-c(1,2)],
                   predictors_df[,2],			# initial model
                   method = "rf",
                   tuneLength = 3,
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like


# limit the number of potential variables before passing to ffs()
bestVars=1:122
while(length(bestVars) > 30)	{
  num_to_shorten = ifelse(length(bestVars) > 40, 8, 3)
  
  sortVars = row.names(varImp(model_LLO)$importance)[rev(order(varImp(model_LLO)$importance))]
  bestVars = sortVars[1:(length(sortVars) - num_to_shorten)]
  print(bestVars)
  
  indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                  k=5)																
  
  model_LLO <- train(predictors_df[,bestVars],predictors_df[,2],			# initial model
                     method="rf",
                     tuneLength=3,										
                     importance=TRUE,
                     trControl=trainControl(method="cv", index = indices$index))
  print(model_LLO)
}


# use ffs() to identify our only significant vars
ffsmodel_LLO <- ffs(predictors_df[,bestVars],
                    predictors_df[,2],
                    metric="RMSE",#metric="Rsquared",
                    method="rf",
                    tuneLength=5,
                    verbose=FALSE,
                    trControl=trainControl(method="cv", index = indices$index))
ffsmodel_LLO



set.seed(5)
validation = sample(nrow(predictors_df), ceiling(nrow(predictors_df) * .2))
indices <- CreateSpacetimeFolds(predictors_df[-validation,],spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[-validation,ffsmodel_LLO$selectedvars],
                   predictors_df[-validation,2],			# initial model
                   method = "rf",
                   tuneLength = length(ffsmodel_LLO$selectedvars),
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like



pred_data_ffs = predict(model_LLO, predictors_df[validation,])

maxval = max(c(pred_data_ffs, predictors_df$B_depth_dry[validation]))
plot(predictors_df$B_depth_dry[validation], pred_data_ffs,
     #log='xy',
     ylim=c(0,maxval),
     xlim=c(0,maxval))
abline(0,1, lty=3,lwd=3,col='red3')


fit <- lm(pred_data_ffs  ~ predictors_df$B_depth_dry[validation])
abline(fit$coeff[2], fit$coeff[2], col='blue3', lwd=3, lty=1)


validation_results = cbind(validation, pred_data_ffs, predictors_df$B_depth_dry[validation])
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write.csv(validation_results, "valid_data_depth_dry.csv")
write.csv(ffsmodel_LLO$selectedvars, "selected_vars_depth_dry.csv")


valid_mae = MAE(predictors_df$B_depth_dry[validation], pred_data_ffs)
valid_rmse = RMSE(predictors_df$B_depth_dry[validation], pred_data_ffs)
valid_sd = sd(summary(fit)$residuals)
valid_r2 = summary(fit)$r.squared

best_mtry = as.numeric(model_LLO$bestTune)
best_mtry_row = which(model_LLO$results$mtry == best_mtry)
cv_mae = model_LLO$results$MAE[best_mtry_row]
cv_rmse = model_LLO$results$RMSE[best_mtry_row]
#cv_sd = model_LLO$results$MAE[best_mtry_row]
cv_r2 = model_LLO$results$Rsquared[best_mtry_row]

model_results_depth_dry = data.frame(cbind(valid_mae, valid_rmse, valid_sd, valid_r2,
                                           cv_mae, cv_rmse, cv_r2), row.names="depth_dry")
write_feather(model_results_depth_dry, "model_results_depth_dry.feather")


# using the model to predict on NHD huc12 dat
# replacing NAs at HUC12s with 0s (this isn't efficient... need to vectorize)

for(i in ffsmodel_LLO$selectedvars)	{
  for(j in 1:nrow(huc12_wNHDatts_vars))		{
    if(is.na(huc12_wNHDatts_vars[j,i])) huc12_wNHDatts_vars[j,i] = 0	
  }
}


pred_data_NHD = predict(model_LLO, huc12_wNHDatts_vars[,ffsmodel_LLO$selectedvars])
huc12_wNHDatts_vars$B_depth_dry = NA
huc12_wNHDatts_vars$B_depth_dry = pred_data_NHD



# now combining NHDPlus attributes with HUC12s
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus\\wbdhu12_a_us_september2019")
huc12_boundaries = st_read("wbdhu12_a_us_september2019.gdb", type=6)

huc12_boundaries_stripped = huc12_boundaries[,c("HUC12", "Shape")]
huc12_boundaries_trans = st_transform(huc12_boundaries_stripped, 2163)

#combining the HUC12s for which we have data with those for which we don't
huc12_wNHDatts_geo = st_as_sf(merge(huc12_wNHDatts_vars, huc12_boundaries_trans,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))

huc12_wNHDatts_geo_centroid = st_centroid(huc12_wNHDatts_geo)		# centroids for more efficiently intersecting with HLRs

# intersecting to identify HLR
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D1\\HydrologicLandscapeRegions_Wolock\\hlrshape")
aqs = read_sf("hlrus.shp")
bchars_aqs = st_intersection(aqs, huc12_wNHDatts_geo_centroid)

# filling watersheds by median value of HLR
for(i in unique(bchars_aqs$HLR))		{
  this_HLR = which(bchars_aqs$HLR == i)
  this_HLR_med = median(bchars_aqs$B_depth_dry[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_depth_dry[this_HLR]))
  bchars_aqs$B_depth_dry[this_HLR][nas_to_fill] = this_HLR_med
}	

st_geometry(bchars_aqs) = NULL
huc12_wNHDatts_geo = st_as_sf(merge(bchars_aqs, huc12_boundaries_stripped,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))



#saving the model for future use
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write_feather(huc12_wNHDatts_vars, "huc12_wNHDatts_vars_depth_dry.feather")
st_write(huc12_wNHDatts_geo, "huc12_wNHDatts_geo_depth_dry.gdb")



library(viridis)
library("ggplot2")
library("sf")
library("rnaturalearth")	# for base map
library("rnaturalearthdata")	# for downlading lakes and rivers data
library("ggspatial")		# for scale bar

#library(rgdal)
#library(raster)
library(rgeos)
library(stars)
library(rasterize)
library(spatial)

###########################################################################################
#####	Making Publishable Plots
###########################################################################################
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3")

lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical', returnclass="sf")
rivers <- ne_download(scale = "medium", type = 'rivers_lake_centerlines', category = 'physical', returnclass='sf')
world <- ne_countries(scale = "medium", returnclass = "sf")



#########################################################
#### Static variables

sdex = c(900000, 1030000, 1230000)
sdwy = c(-1900000, -1900000, -1900000)
legend_points = data.frame(sdex, sdwy)	# for handmade legend for SD

####	median b, with variance
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  #		geom_sf(data=rivers, color='white') 	+
  #		geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = huc12_wNHDatts_geo, shape=16, alpha=1, aes(color=B_depth_dry, fill=B_depth_dry,  stroke=.1))	+
  #geom_point(data=legend_points, color=plasma(5)[1], shape=16, alpha=0.7, aes(sdex, sdwy, stroke=c(1,5,10)*2) )	+
  #geom_text(data=legend_points, size=8, aes(sdex,sdwy-140000,label=c(1,5,10)))	+
  #geom_text(data=legend_points, fontface='bold', size=8, aes(sdex[1]-135000,sdwy[1],label="SD"))	+
  #geom_rect(data=legend_points, fill=NA, color='grey20',
  #	aes(xmin=sdex[1]-230000, xmax=sdex[3]+118000, ymin=sdwy[1]-205000, ymax=sdwy[3]+118000))	+
  scale_color_viridis_c(name="B_depth_dry", trans='log10', option = "plasma",end=.9)	+
  scale_fill_viridis_c(name="B_depth_dry", trans='log10', option = "plasma",end=.9)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.93,.28),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("B_depth_dry_new.png", width = 12, height = 8, dpi = 150)





















#########################
### again but for B_depth_wet
# Part 1: reducing 124 variables down to a more manageable number
wanted_var = gagesII_wNHDatts[,c("gage", "HUC02",						# region is ecoregion; also try with HUC02
                                 "B_depth_wet")]														# designates which aq property will be predicted
#wanted_var[,3] = log10(wanted_var[,3])									# log transform Ksat data

frst_hlr = which(names(gagesII_wNHDatts_vars) == "TOT_HLR_1")			# for removing HLRs from model building
hlr_cols = frst_hlr:(frst_hlr + 19)

predictors_df = cbind(wanted_var[,c(2,3)], gagesII_wNHDatts_vars[,-hlr_cols])
predictors_df[,-1] = signif(predictors_df[,-1], 3)	# rounding all numeric vals to 3 significant digits

indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[,-c(1,2)],
                   predictors_df[,2],			# initial model
                   method = "rf",
                   tuneLength = 3,
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like


# limit the number of potential variables before passing to ffs()
bestVars=1:122
while(length(bestVars) > 30)	{
  num_to_shorten = ifelse(length(bestVars) > 40, 8, 3)
  
  sortVars = row.names(varImp(model_LLO)$importance)[rev(order(varImp(model_LLO)$importance))]
  bestVars = sortVars[1:(length(sortVars) - num_to_shorten)]
  print(bestVars)
  
  indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                  k=5)																
  
  model_LLO <- train(predictors_df[,bestVars],predictors_df[,2],			# initial model
                     method="rf",
                     tuneLength=3,										
                     importance=TRUE,
                     trControl=trainControl(method="cv", index = indices$index))
  print(model_LLO)
}


# use ffs() to identify our only significant vars
ffsmodel_LLO <- ffs(predictors_df[,bestVars],
                    predictors_df[,2],
                    metric="RMSE",#metric="Rsquared",
                    method="rf",
                    tuneLength=5,
                    verbose=FALSE,
                    trControl=trainControl(method="cv", index = indices$index))
ffsmodel_LLO



set.seed(5)
validation = sample(nrow(predictors_df), ceiling(nrow(predictors_df) * .2))
indices <- CreateSpacetimeFolds(predictors_df[-validation,],spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[-validation,ffsmodel_LLO$selectedvars],
                   predictors_df[-validation,2],			# initial model
                   method = "rf",
                   tuneLength = length(ffsmodel_LLO$selectedvars),
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like



pred_data_ffs = predict(model_LLO, predictors_df[validation,])

maxval = max(c(pred_data_ffs, predictors_df$B_depth_wet[validation]))
plot(predictors_df$B_depth_wet[validation], pred_data_ffs,
     #log='xy',
     ylim=c(0,maxval),
     xlim=c(0,maxval))
abline(0,1, lty=3,lwd=3,col='red3')

fit <- lm(pred_data_ffs  ~ predictors_df$B_depth_wet[validation])
abline(fit$coeff[2], fit$coeff[2], col='blue3', lwd=3, lty=1)


validation_results = cbind(validation, pred_data_ffs, predictors_df$B_depth_wet[validation])
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write.csv(validation_results, "valid_data_depth_wet.csv")
write.csv(ffsmodel_LLO$selectedvars, "selected_vars_depth_wet.csv")


valid_mae = MAE(predictors_df$B_depth_wet[validation], pred_data_ffs)
valid_rmse = RMSE(predictors_df$B_depth_wet[validation], pred_data_ffs)
valid_sd = sd(summary(fit)$residuals)
valid_r2 = summary(fit)$r.squared

best_mtry = as.numeric(model_LLO$bestTune)
best_mtry_row = which(model_LLO$results$mtry == best_mtry)
cv_mae = model_LLO$results$MAE[best_mtry_row]
cv_rmse = model_LLO$results$RMSE[best_mtry_row]
#cv_sd = model_LLO$results$MAE[best_mtry_row]
cv_r2 = model_LLO$results$Rsquared[best_mtry_row]

model_results_depth_wet = data.frame(cbind(valid_mae, valid_rmse, valid_sd, valid_r2,
                                           cv_mae, cv_rmse, cv_r2), row.names="depth_wet")

write_feather(model_results_depth_wet, "model_results_depth_wet.feather")



# using the model to predict on NHD huc12 dat
# replacing NAs at HUC12s with 0s (this isn't efficient... need to vectorize)

for(i in ffsmodel_LLO$selectedvars)	{
  for(j in 1:nrow(huc12_wNHDatts_vars))		{
    if(is.na(huc12_wNHDatts_vars[j,i])) huc12_wNHDatts_vars[j,i] = 0	
  }
}


pred_data_NHD = predict(model_LLO, huc12_wNHDatts_vars[,ffsmodel_LLO$selectedvars])
huc12_wNHDatts_vars$B_depth_wet = NA
huc12_wNHDatts_vars$B_depth_wet = pred_data_NHD



# now combining NHDPlus attributes with HUC12s
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus\\wbdhu12_a_us_september2019")
huc12_boundaries = st_read("wbdhu12_a_us_september2019.gdb", type=6)

huc12_boundaries_stripped = huc12_boundaries[,c("HUC12", "Shape")]
huc12_boundaries_trans = st_transform(huc12_boundaries_stripped, 2163)

#combining the HUC12s for which we have data with those for which we don't
huc12_wNHDatts_geo = st_as_sf(merge(huc12_wNHDatts_vars, huc12_boundaries_trans,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))

huc12_wNHDatts_geo_centroid = st_centroid(huc12_wNHDatts_geo)		# centroids for more efficiently intersecting with HLRs

# intersecting to identify HLR
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D1\\HydrologicLandscapeRegions_Wolock\\hlrshape")
aqs = read_sf("hlrus.shp")
bchars_aqs = st_intersection(aqs, huc12_wNHDatts_geo_centroid)

# filling watersheds by median value of HLR
for(i in unique(bchars_aqs$HLR))		{
  this_HLR = which(bchars_aqs$HLR == i)
  this_HLR_med = median(bchars_aqs$B_depth_wet[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_depth_wet[this_HLR]))
  bchars_aqs$B_depth_wet[this_HLR][nas_to_fill] = this_HLR_med
}	

st_geometry(bchars_aqs) = NULL
huc12_wNHDatts_geo = st_as_sf(merge(bchars_aqs, huc12_boundaries_stripped,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))



#saving the model for future use
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write_feather(huc12_wNHDatts_vars, "huc12_wNHDatts_vars_depth_wet.feather")
st_write(huc12_wNHDatts_geo, "huc12_wNHDatts_geo_depth_wet.gdb")





###########################################################################################
#####	Making Publishable Plots 
###########################################################################################
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3")
#########################################################
#### Static variables
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  #		geom_sf(data=rivers, color='white') 	+
  #		geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = huc12_wNHDatts_geo, shape=16, alpha=1, aes(color=B_depth_wet, fill=B_depth_wet,  stroke=.1))	+
  #geom_point(data=legend_points, color=plasma(5)[1], shape=16, alpha=0.7, aes(sdex, sdwy, stroke=c(1,5,10)*2) )	+
  #geom_text(data=legend_points, size=8, aes(sdex,sdwy-140000,label=c(1,5,10)))	+
  #geom_text(data=legend_points, fontface='bold', size=8, aes(sdex[1]-135000,sdwy[1],label="SD"))	+
  #geom_rect(data=legend_points, fill=NA, color='grey20',
  #	aes(xmin=sdex[1]-230000, xmax=sdex[3]+118000, ymin=sdwy[1]-205000, ymax=sdwy[3]+118000))	+
  scale_color_viridis_c(name="B_depth_wet", trans='log10', option = "plasma",end=.9)	+
  scale_fill_viridis_c(name="B_depth_wet", trans='log10', option = "plasma",end=.9)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.93,.28),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("B_depth_wet_new.png", width = 12, height = 8, dpi = 150)













#########################
### again but for B_K2_wet
# Part 1: reducing 124 variables down to a more manageable number
wanted_var = gagesII_wNHDatts[,c("gage", "HUC02",						# region is ecoregion; also try with HUC02
                                 "B_K2_wet")]														# designates which aq property will be predicted
wanted_var[,3] = log10(wanted_var[,3])									# log transform Ksat data

frst_hlr = which(names(gagesII_wNHDatts_vars) == "TOT_HLR_1")			# for removing HLRs from model building
hlr_cols = frst_hlr:(frst_hlr + 19)

predictors_df = cbind(wanted_var[,c(2,3)], gagesII_wNHDatts_vars[,-hlr_cols])
predictors_df[,-1] = signif(predictors_df[,-1], 3)	# rounding all numeric vals to 3 significant digits

indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[,-c(1,2)],
                   predictors_df[,2],			# initial model
                   method = "rf",
                   tuneLength = 3,
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like


# limit the number of potential variables before passing to ffs()
bestVars=1:122
while(length(bestVars) > 30)	{
  num_to_shorten = ifelse(length(bestVars) > 40, 8, 3)
  
  sortVars = row.names(varImp(model_LLO)$importance)[rev(order(varImp(model_LLO)$importance))]
  bestVars = sortVars[1:(length(sortVars) - num_to_shorten)]
  print(bestVars)
  
  indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                  k=5)																
  
  model_LLO <- train(predictors_df[,bestVars],predictors_df[,2],			# initial model
                     method="rf",
                     tuneLength=3,										
                     importance=TRUE,
                     trControl=trainControl(method="cv", index = indices$index))
  print(model_LLO)
}


# use ffs() to identify our only significant vars
ffsmodel_LLO <- ffs(predictors_df[,bestVars],
                    predictors_df[,2],
                    metric="RMSE",#metric="Rsquared",
                    method="rf",
                    tuneLength=5,
                    verbose=FALSE,
                    trControl=trainControl(method="cv", index = indices$index))
ffsmodel_LLO



set.seed(5)
validation = sample(nrow(predictors_df), ceiling(nrow(predictors_df) * .2))
indices <- CreateSpacetimeFolds(predictors_df[-validation,],spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[-validation,ffsmodel_LLO$selectedvars],
                   predictors_df[-validation,2],			# initial model
                   method = "rf",
                   tuneLength = length(ffsmodel_LLO$selectedvars),
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like



pred_data_ffs = predict(model_LLO, predictors_df[validation,])

rangeval = range(c(pred_data_ffs, predictors_df$B_K2_wet[validation]), na.rm=TRUE)
plot(predictors_df$B_K2_wet[validation], pred_data_ffs,
     #log='xy',
     ylim=rangeval,
     xlim=rangeval)
abline(0,1, lty=3,lwd=3,col='red3')


fit <- lm(pred_data_ffs  ~ predictors_df$B_K2_wet[validation])
abline(fit$coeff[1], fit$coeff[2], col='blue3', lwd=3, lty=1)


validation_results = cbind(validation, pred_data_ffs, predictors_df$B_K2_wet[validation])
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write.csv(validation_results, "valid_data_K2_wet.csv")
write.csv(ffsmodel_LLO$selectedvars, "selected_vars_K2_wet.csv")


valid_mae = MAE(predictors_df$B_K2_wet[validation], pred_data_ffs)
valid_rmse = RMSE(predictors_df$B_K2_wet[validation], pred_data_ffs)
valid_sd = sd(summary(fit)$residuals)
valid_r2 = summary(fit)$r.squared

best_mtry = as.numeric(model_LLO$bestTune)
best_mtry_row = which(model_LLO$results$mtry == best_mtry)
cv_mae = model_LLO$results$MAE[best_mtry_row]
cv_rmse = model_LLO$results$RMSE[best_mtry_row]
#cv_sd = model_LLO$results$MAE[best_mtry_row]
cv_r2 = model_LLO$results$Rsquared[best_mtry_row]

model_results_K_wet = data.frame(cbind(valid_mae, valid_rmse, valid_sd, valid_r2,
                                       cv_mae, cv_rmse, cv_r2), row.names="K_wet")

write_feather(model_results_K_wet, "model_results_K_wet.feather")


# using the model to predict on NHD huc12 dat
# replacing NAs at HUC12s with 0s (this isn't efficient... need to vectorize)

for(i in ffsmodel_LLO$selectedvars)	{
  for(j in 1:nrow(huc12_wNHDatts_vars))		{
    if(is.na(huc12_wNHDatts_vars[j,i])) huc12_wNHDatts_vars[j,i] = 0	
  }
}


pred_data_NHD = predict(model_LLO, huc12_wNHDatts_vars[,ffsmodel_LLO$selectedvars])
huc12_wNHDatts_vars$B_K2_wet = NA
huc12_wNHDatts_vars$B_K2_wet = pred_data_NHD



# now combining NHDPlus attributes with HUC12s
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus\\wbdhu12_a_us_september2019")
huc12_boundaries = st_read("wbdhu12_a_us_september2019.gdb", type=6)

huc12_boundaries_stripped = huc12_boundaries[,c("HUC12", "Shape")]
huc12_boundaries_trans = st_transform(huc12_boundaries_stripped, 2163)

#combining the HUC12s for which we have data with those for which we don't
huc12_wNHDatts_geo = st_as_sf(merge(huc12_wNHDatts_vars, huc12_boundaries_trans,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))

huc12_wNHDatts_geo_centroid = st_centroid(huc12_wNHDatts_geo)		# centroids for more efficiently intersecting with HLRs

# intersecting to identify HLR
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D1\\HydrologicLandscapeRegions_Wolock\\hlrshape")
aqs = read_sf("hlrus.shp")
bchars_aqs = st_intersection(aqs, huc12_wNHDatts_geo_centroid)

# filling watersheds by median value of HLR
for(i in unique(bchars_aqs$HLR))		{
  this_HLR = which(bchars_aqs$HLR == i)
  this_HLR_med = median(bchars_aqs$B_K2_wet[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_K2_wet[this_HLR]))
  bchars_aqs$B_K2_wet[this_HLR][nas_to_fill] = this_HLR_med
}	

st_geometry(bchars_aqs) = NULL
huc12_wNHDatts_geo = st_as_sf(merge(bchars_aqs, huc12_boundaries_stripped,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))



#saving the model for future use
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write_feather(huc12_wNHDatts_vars, "huc12_wNHDatts_vars_K_wet.feather")
st_write(huc12_wNHDatts_geo, "huc12_wNHDatts_geo_K_wet.gdb")





###########################################################################################
#####	Making Publishable Plots 
###########################################################################################
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3")
#########################################################
#### Static variables
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  #		geom_sf(data=rivers, color='white') 	+
  #		geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = huc12_wNHDatts_geo, shape=16, alpha=1, aes(color=10^B_K2_wet, fill=10^B_K2_wet,  stroke=.1))	+
  #geom_point(data=legend_points, color=plasma(5)[1], shape=16, alpha=0.7, aes(sdex, sdwy, stroke=c(1,5,10)*2) )	+
  #geom_text(data=legend_points, size=8, aes(sdex,sdwy-140000,label=c(1,5,10)))	+
  #geom_text(data=legend_points, fontface='bold', size=8, aes(sdex[1]-135000,sdwy[1],label="SD"))	+
  #geom_rect(data=legend_points, fill=NA, color='grey20',
  #	aes(xmin=sdex[1]-230000, xmax=sdex[3]+118000, ymin=sdwy[1]-205000, ymax=sdwy[3]+118000))	+
  scale_color_viridis_c(name="B_K2_wet", trans='log10', option = "plasma",end=.9)	+
  scale_fill_viridis_c(name="B_K2_wet", trans='log10',  option = "plasma",end=.9)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.93,.28),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("B_K_wet_new.png", width = 12, height = 8, dpi = 150)










#########################
### again but for B_K2_dry
# Part 1: reducing 124 variables down to a more manageable number
wanted_var = gagesII_wNHDatts[,c("gage", "HUC02",						# region is ecoregion; also try with HUC02
                                 "B_K2_dry")]														# designates which aq property will be predicted
wanted_var[,3] = log10(wanted_var[,3])									# log transform Ksat data

frst_hlr = which(names(gagesII_wNHDatts_vars) == "TOT_HLR_1")			# for removing HLRs from model building
hlr_cols = frst_hlr:(frst_hlr + 19)

predictors_df = cbind(wanted_var[,c(2,3)], gagesII_wNHDatts_vars[,-hlr_cols])
predictors_df[,-1] = signif(predictors_df[,-1], 3)	# rounding all numeric vals to 3 significant digits

indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[,-c(1,2)],
                   predictors_df[,2],			# initial model
                   method = "rf",
                   tuneLength = 3,
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like


# limit the number of potential variables before passing to ffs()
bestVars=1:122
while(length(bestVars) > 30)	{
  num_to_shorten = ifelse(length(bestVars) > 40, 8, 3)
  
  sortVars = row.names(varImp(model_LLO)$importance)[rev(order(varImp(model_LLO)$importance))]
  bestVars = sortVars[1:(length(sortVars) - num_to_shorten)]
  print(bestVars)
  
  indices <- CreateSpacetimeFolds(predictors_df,spacevar = "HUC02",
                                  k=5)																
  
  model_LLO <- train(predictors_df[,bestVars],predictors_df[,2],			# initial model
                     method="rf",
                     tuneLength=3,										
                     importance=TRUE,
                     trControl=trainControl(method="cv", index = indices$index))
  print(model_LLO)
}


# use ffs() to identify our only significant vars
ffsmodel_LLO <- ffs(predictors_df[,bestVars],
                    predictors_df[,2],
                    metric="RMSE",#metric="Rsquared",
                    method="rf",
                    tuneLength=5,
                    verbose=FALSE,
                    trControl=trainControl(method="cv", index = indices$index))
ffsmodel_LLO



set.seed(5)
validation = sample(nrow(predictors_df), ceiling(nrow(predictors_df) * .2))
indices <- CreateSpacetimeFolds(predictors_df[-validation,],spacevar = "HUC02",
                                k=5)																# k=5 to crossvalidate on 80:20

# establish a baseline for identifying good variables
model_LLO <- train(predictors_df[-validation,ffsmodel_LLO$selectedvars],
                   predictors_df[-validation,2],			# initial model
                   method = "rf",
                   tuneLength = length(ffsmodel_LLO$selectedvars),
                   importance = TRUE,
                   trControl = trainControl(method="cv",
                                            index = indices$index))
model_LLO																# just checking to see what it looks like



pred_data_ffs = predict(model_LLO, predictors_df[validation,])

rangeval = range(c(pred_data_ffs, predictors_df$B_K2_dry[validation]), na.rm=TRUE)
plot(predictors_df$B_K2_dry[validation], pred_data_ffs,
     #log='xy',
     ylim=rangeval,
     xlim=rangeval)
abline(0,1, lty=3,lwd=3,col='red3')

fit <- lm(pred_data_ffs  ~ predictors_df$B_K2_dry[validation])
abline(fit$coeff[1], fit$coeff[2], col='blue3', lwd=3, lty=1)


validation_results = cbind(validation, pred_data_ffs, predictors_df$B_K2_dry[validation])
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write.csv(validation_results, "valid_data_K2_dry.csv")
write.csv(ffsmodel_LLO$selectedvars, "selected_vars_K2_dry.csv")

valid_mae = MAE(predictors_df$B_K2_dry[validation], pred_data_ffs)
valid_rmse = RMSE(predictors_df$B_K2_dry[validation], pred_data_ffs)
valid_sd = sd(summary(fit)$residuals)
valid_r2 = summary(fit)$r.squared

best_mtry = as.numeric(model_LLO$bestTune)
best_mtry_row = which(model_LLO$results$mtry == best_mtry)
cv_mae = model_LLO$results$MAE[best_mtry_row]
cv_rmse = model_LLO$results$RMSE[best_mtry_row]
#cv_sd = model_LLO$results$MAE[best_mtry_row]
cv_r2 = model_LLO$results$Rsquared[best_mtry_row]

model_results_K_dry = data.frame(cbind(valid_mae, valid_rmse, valid_sd, valid_r2,
                                       cv_mae, cv_rmse, cv_r2), row.names="K_dry")

write_feather(model_results_K_dry, "model_results_K_dry.feather")



# using the model to predict on NHD huc12 dat
# replacing NAs at HUC12s with 0s (this isn't efficient... need to vectorize)

for(i in ffsmodel_LLO$selectedvars)	{
  for(j in 1:nrow(huc12_wNHDatts_vars))		{
    if(is.na(huc12_wNHDatts_vars[j,i])) huc12_wNHDatts_vars[j,i] = 0	
  }
}


pred_data_NHD = predict(model_LLO, huc12_wNHDatts_vars[,ffsmodel_LLO$selectedvars])
huc12_wNHDatts_vars$B_K2_dry = NA
huc12_wNHDatts_vars$B_K2_dry = pred_data_NHD



# now combining NHDPlus attributes with HUC12s
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus\\wbdhu12_a_us_september2019")
huc12_boundaries = st_read("wbdhu12_a_us_september2019.gdb", type=6)

huc12_boundaries_stripped = huc12_boundaries[,c("HUC12", "Shape")]
huc12_boundaries_trans = st_transform(huc12_boundaries_stripped, 2163)

#combining the HUC12s for which we have data with those for which we don't
huc12_wNHDatts_geo = st_as_sf(merge(huc12_wNHDatts_vars, huc12_boundaries_trans,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))

huc12_wNHDatts_geo_centroid = st_centroid(huc12_wNHDatts_geo)		# centroids for more efficiently intersecting with HLRs

# intersecting to identify HLR
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D1\\HydrologicLandscapeRegions_Wolock\\hlrshape")
aqs = read_sf("hlrus.shp")
bchars_aqs = st_intersection(aqs, huc12_wNHDatts_geo_centroid)

# filling watersheds by median value of HLR
for(i in unique(bchars_aqs$HLR))		{
  this_HLR = which(bchars_aqs$HLR == i)
  this_HLR_med = median(bchars_aqs$B_K2_dry[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_K2_dry[this_HLR]))
  bchars_aqs$B_K2_dry[this_HLR][nas_to_fill] = this_HLR_med
}	

st_geometry(bchars_aqs) = NULL
huc12_wNHDatts_geo = st_as_sf(merge(bchars_aqs, huc12_boundaries_stripped,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))



#saving the model for future use
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
write_feather(huc12_wNHDatts_vars, "huc12_wNHDatts_vars_K_dry.feather")
st_write(huc12_wNHDatts_geo, "huc12_wNHDatts_geo_K_dry.gdb")


###########################################################################################
#####	Making Publishable Plots 
###########################################################################################
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3")
#########################################################
#### Static variables
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  #		geom_sf(data=rivers, color='white') 	+
  #		geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = huc12_wNHDatts_geo, shape=16, alpha=1, aes(color=10^B_K2_dry, fill=10^B_K2_dry,  stroke=.1))	+
  #geom_point(data=legend_points, color=plasma(5)[1], shape=16, alpha=0.7, aes(sdex, sdwy, stroke=c(1,5,10)*2) )	+
  #geom_text(data=legend_points, size=8, aes(sdex,sdwy-140000,label=c(1,5,10)))	+
  #geom_text(data=legend_points, fontface='bold', size=8, aes(sdex[1]-135000,sdwy[1],label="SD"))	+
  #geom_rect(data=legend_points, fill=NA, color='grey20',
  #	aes(xmin=sdex[1]-230000, xmax=sdex[3]+118000, ymin=sdwy[1]-205000, ymax=sdwy[3]+118000))	+
  scale_color_viridis_c(name="B_K2_dry", trans="log10", option = "plasma",end=.9)	+
  scale_fill_viridis_c(name="B_K2_dry", trans="log10", option = "plasma",end=.9)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.93,.28),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("B_K_dry_new.png", width = 12, height = 8, dpi = 150)

















#reading in old models
# the validation data for illustrating model performance

k2dry = read.csv("valid_data_K2_dry.csv")
k2wet = read.csv("valid_data_K2_wet.csv")
k2range = range(c(k2dry[,3:4], k2wet[,3:4]))

depdry = read.csv("valid_data_depth_dry.csv") * 1000 / 10
depwet = read.csv("valid_data_depth_wet.csv") *1000 / 10
deprange = range(c(depdry[,3:4], depwet[,3:4]))

png(paste0("validation_plots.png"), width=1000, height=800)
par(cex=9, mfrow=(c(2,2)), mar=c(7.1,8,0,.5))

plot(k2dry[,4], k2dry[,3], pch=1, lwd=4, ylim=k2range, xlim=k2range, 
     xlab="", ylab="", yaxt='n', xaxt='n', col='grey40')
lmod = lm(k2dry[,3] ~ k2dry[,4])
title(ylab="log10(K) - cm/s", line=4, cex.lab=4)
axis(1, at=c(-4,-3,-2,-1,0), labels=TRUE, cex.axis=3, padj=0.6)
axis(2, at=c(-4,-3,-2,-1,0), labels=TRUE, cex.axis=3)
text(-2.5,-.5, paste0("r-squared = ", signif(summary(lmod)$r.squared, 2)),
     cex = 3, col='tomato')
text(-2.5,-1, paste0("slope = ", signif(lmod$coef[2], 2)),
     cex = 3, col='tomato')
abline(lmod$coef[1], lmod$coef[2], lwd=5, lty=1, col='tomato')
abline(0,1, lwd=2.5, lty=4, col="grey10")


plot(depdry[,4], depdry[,3], pch=1, lwd=4, ylim=deprange, xlim=deprange, 
     xlab="", ylab="", yaxt='n', xaxt='n', col='grey40')
lmod = lm(depdry[,3] ~ depdry[,4])
title(ylab="S - mm", line=4, cex.lab=4)
axis(1, at=c(1,2000,4000), labels=TRUE, cex.axis=3, padj=0.6)
axis(2, at=c(1,2000,4000), labels=TRUE, cex.axis=3)
text(1500,4200, paste0("r-squared = ", signif(summary(lmod)$r.squared, 2)),
     cex = 3, col='tomato')
text(1500,3700, paste0("slope = ", signif(lmod$coef[2], 2)),
     cex = 3, col='tomato')
abline(lmod$coef[1], lmod$coef[2], lwd=5, lty=1, col='tomato')
abline(0,1, lwd=2.5, lty=4, col="grey10")


plot(k2wet[,4], k2wet[,3], pch=1, lwd=4, ylim=k2range, xlim=k2range, 
     xlab="", ylab="", yaxt='n', xaxt='n', col='grey40')
lmod = lm(k2wet[,3] ~ k2wet[,4])
title(ylab="log10(K) - cm/s", line=4, cex.lab=4)
title(xlab="log10(K) - cm/s", line=5.8, cex.lab=4)
axis(1, at=c(-4,-3,-2,-1,0), labels=TRUE, cex.axis=3, padj=0.6)
axis(2, at=c(-4,-3,-2,-1,0), labels=TRUE, cex.axis=3)
text(-2.5,-.5, paste0("r-squared = ", signif(summary(lmod)$r.squared, 2)),
     cex = 3, col='tomato')
text(-2.5,-1, paste0("slope = ", signif(lmod$coef[2], 2)),
     cex = 3, col='tomato')
abline(lmod$coef[1], lmod$coef[2], lwd=5, lty=1, col='tomato')
abline(0,1, lwd=2.5, lty=4, col="grey10")


plot(depwet[,4], depwet[,3], pch=1, lwd=4, ylim=deprange, xlim=deprange, 
     xlab="", ylab="", yaxt='n', xaxt='n', col='grey40')
lmod = lm(depwet[,3] ~ depwet[,4])
title(ylab="S - mm", line=4, cex.lab=4)
title(xlab="S - mm", line=5.8, cex.lab=4)
axis(1, at=c(1,2000,4000), labels=TRUE, cex.axis=3, padj=0.6)
axis(2, at=c(1,2000,4000), labels=TRUE, cex.axis=3)
text(1500,4200, paste0("r-squared = ", signif(summary(lmod)$r.squared, 2)),
     cex = 3, col='tomato')
text(1500,3700, paste0("slope = ", signif(lmod$coef[2], 2)),
     cex = 3, col='tomato')
abline(lmod$coef[1], lmod$coef[2], lwd=5, lty=1, col='tomato')
abline(0,1, lwd=2.5, lty=4, col="grey10")
dev.off()





#the models themselves
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
model_results_all = merge(read_feather("huc12_wNHDatts_vars_K_dry.feather"),
                          read_feather("huc12_wNHDatts_vars_K_wet.feather")[,c("HUC_12", "B_K2_wet")],
                          by='HUC_12')
model_results_all = merge(model_results_all, 
                          read_feather("huc12_wNHDatts_vars_depth_dry.feather")[,c("HUC_12", "B_depth_dry")],
                          by='HUC_12')
model_results_all = merge(model_results_all, 
                          read_feather("huc12_wNHDatts_vars_depth_wet.feather")[,c("HUC_12", "B_depth_wet")],
                          by='HUC_12')

# writing a summary of all S and K from model results
write.csv(rbind(summary(model_results_all$B_K2_wet),
                summary(model_results_all$B_K2_dry),
                summary(model_results_all$B_depth_wet *100),
                summary(model_results_all$B_depth_dry *100)), "summary_recessvars_all_modeleld.csv")


###########################################################################################
#####	Making Publishable Plots 
###########################################################################################
# using HLRs to fill in missing watersheds
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\NHDPlus\\wbdhu12_a_us_september2019")
huc12_boundaries = st_read("wbdhu12_a_us_september2019.gdb", type=6)

huc12_boundaries_stripped = huc12_boundaries[,c("HUC12", "Shape")]
huc12_boundaries_trans = st_transform(huc12_boundaries_stripped, 2163)

#combining the HUC12s for which we have data with those for which we don't
huc12_wNHDatts_geo = st_as_sf(merge(model_results_all, huc12_boundaries_trans,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))

huc12_wNHDatts_geo_centroid = st_centroid(huc12_wNHDatts_geo)		# centroids for more efficiently intersecting with HLRs

# intersecting to identify HLR
setwd("C:\\Users\\arik\\Documents\\PhD Research\\D1\\HydrologicLandscapeRegions_Wolock\\hlrshape")
aqs = read_sf("hlrus.shp")
bchars_aqs = st_intersection(aqs, huc12_wNHDatts_geo_centroid)

# filling watersheds by median value of HLR
for(i in unique(bchars_aqs$HLR))		{
  this_HLR = which(bchars_aqs$HLR == i)
  
  this_HLR_med = median(bchars_aqs$B_depth_dry[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_depth_dry[this_HLR]))
  bchars_aqs$B_depth_dry[this_HLR][nas_to_fill] = this_HLR_med
  
  this_HLR_med = median(bchars_aqs$B_depth_wet[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_depth_wet[this_HLR]))
  bchars_aqs$B_depth_wet[this_HLR][nas_to_fill] = this_HLR_med
  
  this_HLR_med = median(bchars_aqs$B_K2_dry[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_K2_dry[this_HLR]))
  bchars_aqs$B_K2_dry[this_HLR][nas_to_fill] = this_HLR_med
  
  this_HLR_med = median(bchars_aqs$B_K2_wet[this_HLR], na.rm=TRUE)
  nas_to_fill = which(is.na(bchars_aqs$B_K2_wet[this_HLR]))
  bchars_aqs$B_K2_wet[this_HLR][nas_to_fill] = this_HLR_med
}	

st_geometry(bchars_aqs) = NULL
huc12_wNHDatts_geo = st_as_sf(merge(bchars_aqs, huc12_boundaries_stripped,
                                    by.x="HUC_12", by.y="HUC12", all=TRUE))

# supplementary plots for illustrating the 'clustering' of hydraulic values by hlr_col
boxplot(B_depth_dry ~ HLR, data=bchars_aqs, notch=TRUE,
        col='grey30', main="Dry Period Aquifer Thickness", xlab="HLR", ylab="Aq. Thickness (m)", log='y')



setwd("C://Users//arik//Documents//PhD Research//D3//Figures")
# to ensure the scales are the same here as with the purely recession data
large_to_small	= rev(order(bchars_sf_poly$DRAIN_SQKM))# reordering so small watersheds write over large watersheds
plot_data = bchars_sf_poly[large_to_small,]
nas_tobe_removed = which(is.na(plot_data$B_K2_dry))
plot_data = plot_data[-nas_tobe_removed,]

plot_data$K_dry = log10(plot_data$B_K2_dry)
plot_data$K_wet = log10(plot_data$B_K2_wet)

min_val = min(c(plot_data$K_dry, plot_data$K_wet))	# a cheat way to set limits on scale_color_viridis below, since native options are a mess
min_val_row = plot_data[1,]	; min_val_row$K_dry = min_val; min_val_row$K_wet = min_val
max_val = max(c(plot_data$K_dry, plot_data$K_wet))
max_val_row = plot_data[1,]	; max_val_row$K_dry = max_val ; max_val_row$K_wet = max_val



# variable values for the whole conus
plot_data_m= rbind(huc12_wNHDatts_geo[1,], huc12_wNHDatts_geo[1,],
                   huc12_wNHDatts_geo[which(!is.na(huc12_wNHDatts_geo$B_K2_dry)),])	# removing nas for easier plotting
plot_data_m$B_K2_wet[1:2] = c(min_val, max_val)
plot_data_m$B_K2_dry[1:2] = c(min_val, max_val)
my_breaks_K = c(-4,-2,0)

####  K (dry period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=B_K2_dry, fill=B_K2_dry,  stroke=1))	+
  scale_color_viridis_c(name="log10(K)",option = "plasma",end=.9,
                        breaks = my_breaks_K, labels=my_breaks_K)	+
  scale_fill_viridis_c(name="log10(K)",option = "plasma",end=.9,
                       breaks = my_breaks_K, labels=my_breaks_K)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.16),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("K_dry_modeled.png", width = 12, height = 8, dpi = 150)


####	 K (wet period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=B_K2_wet, fill=B_K2_wet,  stroke=1))	+
  scale_color_viridis_c(name="log10(K)",option = "plasma",end=.9,
                        breaks = my_breaks_K, labels=my_breaks_K)	+
  scale_fill_viridis_c(name="log10(K)",option = "plasma",end=.9,
                       breaks = my_breaks_K, labels=my_breaks_K)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.16),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("K_wet_modeled.png", width = 12, height = 8, dpi = 150)










large_to_small	= rev(order(bchars_sf_poly$DRAIN_SQKM))# reordering so small watersheds write over large watersheds
plot_data = bchars_sf_poly[large_to_small,]
nas_tobe_removed = which(is.na(plot_data$B_K2_dry))
plot_data = plot_data[-nas_tobe_removed,]

plot_data$S_dry = plot_data$B_depth_dry * f * 1000
plot_data$S_wet = plot_data$B_depth_wet * f * 1000

min_val = min(c(plot_data$S_dry, plot_data$S_wet))	# a cheat way to set limits on scale_color_viridis below, since native options are a mess
max_val = max(c(plot_data$S_dry, plot_data$S_wet))


# variable values for the whole conus
plot_data_m= rbind(huc12_wNHDatts_geo[1,], huc12_wNHDatts_geo[1,],
                   huc12_wNHDatts_geo[which(!is.na(huc12_wNHDatts_geo$B_K2_dry)),])	# removing nas for easier plotting
plot_data_m$S_dry = plot_data_m$B_depth_dry * f * 1000
plot_data_m$S_wet = plot_data_m$B_depth_wet * f * 1000
plot_data_m$S_wet[1:2] = c(min_val, max_val)
plot_data_m$S_dry[1:2] = c(min_val, max_val)
my_breaks = c(1, 10 100, 1000)



####  S (dry period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=S_dry, fill=S_dry,  stroke=1))	+
  scale_color_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                        breaks = my_breaks, labels=my_breaks)	+
  scale_fill_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                       breaks = my_breaks, labels=my_breaks)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.16),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("S_dry_modeled.png", width = 12, height = 8, dpi = 150)


####	S (wet period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=S_wet, fill=S_wet,  stroke=1))	+
  scale_color_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                        breaks = my_breaks, labels=my_breaks)	+
  scale_fill_viridis_c(name="S (mm)",trans='log10',option = "plasma",end=.9,
                       breaks = my_breaks, labels=my_breaks)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.16),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("S_wet_modeled.png", width = 12, height = 8, dpi = 150)








#### Comparing dry vs wet period chars
plot_data_m$K2_change = 10^(10^plot_data_m$B_K2_wet - 10^plot_data_m$B_K2_dry)

plot_data_m$depth_change = plot_data_m$S_wet - plot_data_m$S_dry


####	 K (wet period)
ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  geom_sf(data=rivers, color='white') 	+
  geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=K2_change, fill=K2_change,  stroke=1))	+
  scale_color_viridis_c(name="change in K)",option = "plasma",end=.9,
                        breaks = my_breaks_K, labels=my_breaks_K)	+
  scale_fill_viridis_c(name="change in K",option = "plasma",end=.9,
                       breaks = my_breaks_K, labels=my_breaks_K)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.24, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.16),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("K_change_modeled.png", width = 12, height = 8, dpi = 150)




ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  #		geom_sf(data=rivers, color='white') 	+
  #		geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=K2_change, fill=K2_change,  stroke=.1))	+
  #geom_point(data=legend_points, color=plasma(5)[1], shape=16, alpha=0.7, aes(sdex, sdwy, stroke=c(1,5,10)*2) )	+
  #geom_text(data=legend_points, size=8, aes(sdex,sdwy-140000,label=c(1,5,10)))	+
  #geom_text(data=legend_points, fontface='bold', size=8, aes(sdex[1]-135000,sdwy[1],label="SD"))	+
  #geom_rect(data=legend_points, fill=NA, color='grey20',
  #	aes(xmin=sdex[1]-230000, xmax=sdex[3]+118000, ymin=sdwy[1]-205000, ymax=sdwy[3]+118000))	+
  scale_color_viridis_c(name="K2_change", option = "plasma",end=.9)	+
  scale_fill_viridis_c(name="K2_change", option = "plasma",end=.9)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.1,.16),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("B_K_change_new.png", width = 12, height = 8, dpi = 150)


ggplot(data = world) +
  geom_sf(fill='grey92', color="grey92") +
  #		geom_sf(data=rivers, color='white') 	+
  #		geom_sf(data=lakes, fill='white', color='grey92') +
  geom_sf(data = plot_data_m, shape=16, alpha=1, aes(color=depth_change, fill=depth_change,  stroke=.1))	+
  #geom_point(data=legend_points, color=plasma(5)[1], shape=16, alpha=0.7, aes(sdex, sdwy, stroke=c(1,5,10)*2) )	+
  #geom_text(data=legend_points, size=8, aes(sdex,sdwy-140000,label=c(1,5,10)))	+
  #geom_text(data=legend_points, fontface='bold', size=8, aes(sdex[1]-135000,sdwy[1],label="SD"))	+
  #geom_rect(data=legend_points, fill=NA, color='grey20',
  #	aes(xmin=sdex[1]-230000, xmax=sdex[3]+118000, ymin=sdwy[1]-205000, ymax=sdwy[3]+118000))	+
  scale_color_viridis_c(name="depth_change", option = "plasma",end=.9)	+
  scale_fill_viridis_c(name="depth_change", option = "plasma",end=.9)	+
  coord_sf(crs = st_crs(2163), xlim = c(2600000, -2100000),ylim = c(-2200000, 950000),expand=FALSE)	+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.background=element_blank(), 
        legend.position=c(.93,.28),
        legend.title=element_text(face='bold', size=30),
        legend.text=element_text(size=20),
        panel.background=NULL ,
        panel.grid=NULL , panel.grid.major=element_line(color="grey92"), panel.grid.minor=NULL,
        panel.border=element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_text(size=20))	
ggsave("B_depth_change_new.png", width = 12, height = 8, dpi = 150)




setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
gg = read_feather("model_results_depth_dry.feather")
hh = read_feather("model_results_depth_wet.feather")
ii = read_feather("model_results_K_dry.feather")
jj = read_feather("model_results_K_wet.feather")




