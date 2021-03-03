# USGS plots

library(dataRetrieval)
library(ggplot2)
library(stringr)
library(scales)

start_date <- '2018-10-01'
end_date <- '2019-09-30' 
pcode <- '00060'

# Virginia Site: Appomattox River, site 02040000

va_id <- '02040000'
va_info <- readNWISsite(va_id)
va_area <- va_info$drain_area_va

#VA 2019
va_2019 <- readNWISdv(va_id,pcode,start_date,end_date)
va_rawdaily <- va_2019$X_00060_00003
va_spec_dis <- (va_rawdaily/va_area)

ggplot() + geom_line(aes(va_2019$Date,va_spec_dis))+theme_bw() +
  labs( y='Discharge [cfs/mi^2]',x='',
        title= 'Specific Discharge of the Appomattox River in 2019')
#2001 VA
start_2001 <- '2000-10-01'
end_2001 <- '2001-09-30'

va_2001 <- readNWISdv(va_id,pcode,start_2001,end_2001)
va_raw_2001 <- va_2001$X_00060_00003
va_spec_2001 <- (va_raw_2001/va_area)

ggplot() + geom_line(aes(va_2001$Date,va_spec_2001))+theme_bw() +
  labs( y='Discharge [cfs/mi^2]',x='',
        title= 'Specific Discharge of the Appomattox River in 2001')

# Comparison of 2001  and 2019 
ggplot() + geom_line(aes(va_2019$Date,va_spec_2001,color='2001')) +
  geom_line(aes(va_2019$Date,va_spec_dis,color = '2019')) +
  labs( y='Discharge [cfs/mi^2]',x='Time',
        title= ' Specific Discharge of the Appomattox River 2001 vs. 2019') + 
  theme_bw() + scale_x_date(labels = date_format("%B"))


# New York Site: Black River, site 04260500

ny_id <- '04260500'
ny_info <- readNWISsite(ny_id)
ny_flow <- readNWISdv(ny_id,pcode,start_date,end_date)
ny_area <- ny_info$drain_area_va

#specific discharge
ny_rawdaily <- ny_flow$X_00060_00003
ny_spec_dis <- (ny_rawdaily/ny_area)

ggplot() + geom_line(aes(ny_flow$Date,ny_spec_dis))+theme_bw() +
  labs( y='Discharge [cfs/mi^2]',x='',
        title= ' Specific Discharge of the Black River in 2019')

#2019 comparisons of both sites
ggplot() + geom_line(aes(ny_flow$Date,ny_spec_dis,color='Black River, NY')) + 
  theme_bw() + labs( y='Discharge [cfs/mi^2]',x='',
                     title= 'Specific Discharge of the Black River vs. Appomattox River in 2019') +
  geom_line(aes(va_2019$Date,va_spec_dis,color = 'Appomattox River, VA'))

# Additional Plotting
va_rawdaily_full <- readNWISdv(va_id,pcode,'1999-10-01',end_date)
va_rawdaily_full <- addWaterYear(va_rawdaily_full)
va_rawdaily_full$spec_dis <- va_rawdaily_full$X_00060_00003/va_area

#plot of annual discharge 
ggplot(va_rawdaily_full, aes(group=waterYear,x=waterYear,y=spec_dis)) + 
  geom_boxplot(outlier.shape=NA)+theme_bw() + labs(y='Discharge', x='Water Year',
                                   title= 'Specific Discharge by Water Year Since 2000') + 
  geom_point(stat='summary', color='red',fun='mean') + coord_cartesian(ylim = c(0, 4.5))

# Comparing summer months across the years
va_summer <- subset(va_rawdaily_full, month==6 | month==7 | month==8 | month==9)

ggplot(va_summer, aes(group=waterYear,x=waterYear,y=spec_dis)) + 
  geom_boxplot(outlier.shape=NA)+theme_bw() + labs(y='Discharge', x='Water Year',
                                                   title= 'Appomattox River Specific Discharge From June to September by Water Year') +
  geom_point(stat='summary', color='red',fun='mean') + coord_cartesian(ylim = c(0, 4))

# If you wanted to further separate into months
date <- va_rawdaily_full$Date
month <- format(as.Date(date,format = '%d-%m-%Y'), '%m')
month <- as.numeric(month)
va_rawdaily_full$month <- month

va_jan <- subset(va_rawdaily_full, month==1)
va_feb <- subset(va_rawdaily_full, month==2)

# boxplot of the above months by water year

#january 
ggplot(va_jan, aes(group=waterYear,x=waterYear,y=spec_dis)) + 
  geom_boxplot(outlier.shape=NA)+theme_bw() + labs(y='Discharge [cfs/mi^2]', x='Water Year',
                                                   title= 'Appomattox River January Discharge by Water Year Since 2000') +
  geom_point(stat='summary', color='red',fun='mean') + coord_cartesian(ylim = c(0, 10)) 
#feb
ggplot(va_feb, aes(group=waterYear,x=waterYear,y=spec_dis)) + 
  geom_boxplot(outlier.shape=NA)+theme_bw() + labs(y='Discharge [cfs/mi^2]', x='Water Year',
                                                   title= 'Appomattox River February Discharge by Water Year Since 2000') +
  geom_point(stat='summary', color='red',fun='mean') + coord_cartesian(ylim = c(0, 6))

