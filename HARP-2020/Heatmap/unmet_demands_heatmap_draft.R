site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh

#----------------------------------------------
# Load Libraries
basepath = '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

library(dplyr)
library(lattice)

# Inputs- Shennandoah @ Luray 
pid <- 4964892 
elid <-  299330 
runid <- 18 


dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE)
syear = min(dat$year)
eyear = max(dat$year)

if (syear != eyear) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0(eyear,"-09-30"))
} 

sdate<-as.Date("1985-01-01")
edate<-as.Date("2014-12-31")

datpd <- window(dat, start = sdate, end = edate);
mode(datpd) <- 'numeric'
scen.propname<-paste0('runid_', runid)

# GETTING SCENARIO PROPERTY FROM VA HYDRO
sceninfo <- list(
  varkey = 'om_scenario',
  propname = scen.propname,
  featureid = pid,
  entity_type = "dh_properties"
)
scenprop <- getProperty(sceninfo, site, scenprop)

if (identical(scenprop, FALSE)) {
  # create
  sceninfo$pid = NULL
} else {
  sceninfo$pid = scenprop$pid
}

scenprop <- getProperty(sceninfo, site, scenprop)

datpd_df <- as.data.frame(datpd)

modat <- sqldf(" select month months, 
  year years, count(*) count from 
  datpd_df where unmet_demand_mgd > 0
  group by month, year")
               

#table for each month year combo even if sum of unmet day = 0
modat2 <- sqldf("SELECT * FROM datpd_df LEFT JOIN modat ON modat.years = datpd_df.year AND 
                modat.months = datpd_df.month group by month, year")

modat2 <- sqldf('SELECT month, year, count count_unmet_days FROM modat2 GROUP BY month, year')

#Replace NA for count with 0s
modat2[is.na(modat2)] = 0

modatmat<-as.matrix(modat2)




ggplot(modat2, aes(x=month, y=year, fill= count_unmet_days)) + 
  geom_tile(col='white')+
  geom_text(aes(label = count_unmet_days), size=2)+
  scale_fill_gradient(low="green", high="red",name = "Unmet Days")+
  scale_x_continuous(expand = c(0,0), breaks=seq(1,12, 1), position='top',
                     labels = month.abb)+
  scale_y_reverse(expand=c(0,0), breaks=seq(1985,2014,1))+
  labs(x=NULL,
        y=NULL, 
        title='Shennandoah @ Luray - Runid 18 - Unmet Demands', 
       legend)+
theme(axis.ticks= element_blank())+
  theme_gray()+
theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5))+
theme(legend.title.align = 0.5)

#https://stackoverflow.com/questions/55787412/adding-marginal-totals-to-ggplot-heatmap-in-r
