#Dendron, VA 2019
site_idva <- '02047500'
startDate<-'2018-10-01'
endDate<-'2019-09-30'
pCode <- '00060'

siteinfova <- readNWISsite(site_idva)
rawDailyQva <- readNWISdv(site_idva,pCode,startDate,endDate)

va <- siteinfova$drain_area_va

plot(rawDailyQva$Date,rawDailyQva$X_00060_00003, type = 'ln')
library(ggplot2)
library(scales)
ggplot(rawDailyQva, aes(x = rawDailyQva$Date)) + 
          geom_line(aes(y = rawDailyQva$X_00060_00003)) + 
          geom_line(aes(y = yAvgva1), linetype = 'dotted', color = 'red') +
          theme_bw() + 
          labs( title = 'Specific Discharge in Dendron VA', x = NULL, y = 'Specific Discharge (cfs)') +
          theme(plot.title = element_text(face = 'bold',hjust = 0.5))

sRunoffva <- (rawDailyQva$X_00060_00003/(va))*(1/5280^2)*(12)*(86400)*(365)
plot(rawDailyQva$Date,sRunoffva, main = "2019 Specific Disharge Over Time", xlab = "Time", ylab = "Specific Discharge (in/yr)", type = 'ln')
ggplot(rawDailyQva, aes(x = rawDailyQva$Date,y = sRunoffva)) + 
        geom_line() + 
        theme_bw() + 
        labs( title = 'Specific Discharge in Dendron VA', x = NULL, y = 'Specific Discharge (in/yr)') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))
yAvgva1 <- mean(rawDailyQva$X_00060_00003)
yAvgva2 <- mean(sRunoffva)
maxSRunoffva <- max(sRunoffva)
minSRunoffva <- min(sRunoffva)


#2001 VA
startDateoldva <- '2000-10-01'
endDateoldva <- '2001-09-30'

rawDailyQoldva <- readNWISdv(site_idva,pCode,startDateoldva,endDateoldva)

plot(rawDailyQoldva$Date,rawDailyQoldva$X_00060_00003, type = 'ln')
ggplot(rawDailyQva, aes(x = rawDailyQoldva$Date,y = rawDailyQoldva$X_00060_00003)) + 
        geom_line() + 
        theme_bw() + 
        labs( title = 'Specific Discharge in Dendron VA', x = NULL, y = 'Specific Discharge (ft/s)') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))

sRunoffoldva <- (rawDailyQoldva$X_00060_00003/(va))*(1/5280^2)*(12)*(86400)*(365)
plot(rawDailyQoldva$Date,sRunoffoldva, main = "2001 Specific Discharge Over Time", xlab = "Time", ylab = "Specific Discharge (in/yr)", type = 'ln')
ggplot(rawDailyQva, aes(x = rawDailyQoldva$Date,y = sRunoffoldva)) + 
        geom_line() + 
        theme_bw() + 
        labs( title = 'Specific Discharge in Dendron VA', x = NULL, y = 'Specific Discharge (in/yr)') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))
yAvgoldva1 <- mean(rawDailyQoldva$X_00060_00003)
yAvgoldva2 <- mean(sRunoffoldva)
maxSRunoffoldva <- max(sRunoffoldva)
minSRunoffoldva <- min(sRunoffoldva)


#Santa Barbara, CA
site_idsb <- '11123000'
startDatesb <-'2018-10-01'
endDatesb <-'2019-09-30'
pCode <-'00060'

siteinfosb <- readNWISsite(site_idsb)
rawDailyQsb <- readNWISdv(site_idsb,pCode,startDatesb,endDatesb)

sb <- siteinfosb$drain_area_va

plot(rawDailyQsb$Date,rawDailyQsb$X_00060_00003, type = 'ln')
ggplot(rawDailyQva, aes(x = rawDailyQsb$Date,y = rawDailyQsb$X_00060_00003)) + 
        geom_line() + 
        theme_bw() + 
        labs( title = 'Specific Discharge on the Santa Ynez River, CA', x = NULL, y = 'Specific Discharge (ft/s)') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))

sRunoffsb <- (rawDailyQsb$X_00060_00003/(sb))*(1/5280^2)*(12)*(86400)*(365)
plot(rawDailyQsb$Date,sRunoffsb, main = "2019 Specific Discharge Over Time", xlab = "Time", ylab = "Specific Discharge (in/yr)", type='ln')
ggplot(rawDailyQva, aes(x = rawDailyQsb$Date,y = sRunoffsb)) + 
        geom_line() + 
        theme_bw() + 
        labs( title = 'Specific Discharge on the Santa Ynez River, CA', x = NULL, y = 'Specific Discharge (in/yr)') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))
yAvgsb1 <- mean(rawDailyQsb$X_00060_00003)
yAvgsb2 <- mean(sRunoffsb)
maxSRunoffsb <- max(sRunoffsb)
minSRunoffsb <- min(sRunoffsb)

##Comparison Plots
ggplot(rawDailyQva, aes(x = rawDailyQva$Date)) + 
              geom_line(aes(y = sRunoffva, color = 'Dendron, VA')) + 
              geom_line(aes(y = sRunoffsb, color = 'Santa Ynez River, CA')) +
              theme_bw() + 
              labs( title = '2019 Comparison of Specific Discharge between Gauges', x = NULL, y = 'Specific Discharge (in/yr)') +
              theme(plot.title = element_text(face = 'bold',hjust = 0.5))

ggplot(NULL, aes(x=rawDailyQva$Date))+
        geom_line(aes(y=sRunoffva, color = '2019'))+
        geom_line(aes(y=sRunoffoldva, color = '2001'))+
        scale_x_date(labels=date_format('%B'))+
        theme_bw()+
        labs(title='Comparison of Specific Discharge in Dendron, VA', x = NULL, y = 'Specific Discharge (in/yr)')+
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))
        
