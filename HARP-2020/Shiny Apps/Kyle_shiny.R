###########################
## Shiny Tutorial

library(shiny)
library(dataRetrieval)
library(ggplot2)
library(leaflet)
library(sqldf)
library(ggnewscale)
library(dplyr)
library(stringr)

site <- "http://deq2.bse.vt.edu/d.dh"
basepath <- '/var/www/R';


## Unmet demand plots

ui <- fluidPage(titlePanel("Interactive Facility Unmet Demand Data"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      inputId = "graph",
                      label = "Plot of Interest",
                      c("Unmet Demand Heatmap" , "Unmet Demand Flow Plot")),
                    textInput(
                      inputId = "runid",
                      label = "Scenario run",
                      value = 18),
                    textInput(
                      inputId = "elid",
                      label = "Element ID",
                      value = "299330"
                    ),
                    textInput(
                      inputId = "pid",
                      label = "PID",
                      value = "4964892"
                    )
                  ),
                  mainPanel(
                    plotOutput(outputId = "plot")
                )
                )
)

server <- function(input, output){
  output$plot <- renderPlot({
   
    #----------------------------------------------
    source(paste(basepath,'config.R',sep='/'))
    
    runid <- input$runid
    elid <- input$elid
    pid <- input$pid
    #----------------------------------------------
    if(input$graph == "Unmet Demand Heatmap"){
      dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) # get data
      
      syear = min(dat$year)
      eyear = max(dat$year)
      sdate <- as.Date(paste0(syear,"-01-01"))
      edate <- as.Date(paste0(eyear,"-12-31"))
      
      dat <- window(dat, start = sdate, end = edate);
      mode(dat) <- 'numeric'      
      datdf <- as.data.frame(dat)
      
      modat <- sqldf(" select month months, year years, count(*) count from datdf where unmet_demand_mgd > 0
      group by month, year") #Counts sum of unmet_days by month and year
      
      #Join counts with original data frame to get missing month and year combos then selects just count month and year
      modat <- sqldf("SELECT * FROM datdf LEFT JOIN modat ON modat.years = datdf.year AND modat.months = datdf.month group by month, year")
      modat <- sqldf('SELECT month, year, count count_unmet_days FROM modat GROUP BY month, year')
      
      #Replace NA for count with 0s
      modat[is.na(modat)] = 0
      
      ########################################################### Calculating Totals
      # numeric versions of eyear and syear
      
      num_syear <- as.numeric(syear)  
      num_eyear <- as.numeric(eyear)
      
      # monthly totals via sqldf
      mosum <- sqldf("SELECT  month, sum(count_unmet_days) count_unmet_days FROM modat GROUP BY month")
      mosum$year <- rep(num_eyear+1,12)
      
      #yearly sum
      yesum <-  sqldf("SELECT year, sum(count_unmet_days) count_unmet_days FROM modat GROUP BY year")
      yesum$month <- rep(13,length(yesum$year))
      
      # create monthly averages 
      moavg<-
        mosum %>%
        mutate(avg=count_unmet_days/((num_eyear-num_syear)+1)) %>%
        mutate(year=num_eyear+2)
      
      moavg$avg<-round(moavg$avg, 1)
      
      # create yearly averages
      yeavg <-  
        yesum %>%
        mutate(avg=count_unmet_days/12) %>%
        mutate(month=14)
      
      yeavg$avg<-round(yeavg$avg, 1)
      
      # create x and y axis breaks
      y_breaks <- seq(syear,num_eyear+2,1)
      x_breaks <- seq(1,14,1)
      
      # create x and y labels
      y_labs <- c(seq(syear,eyear,1),'Totals', 'Avg')
      x_labs <- c(month.abb,'Totals','Avg')
      
      
      ############################################################### Plot and Save
      if (sum(mosum$count_unmet_days) == 0) {
        count_grid <- ggplot() +
          geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
          geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
          scale_fill_gradient2(low = "#00cc00", mid= "#00cc00", high = "#00cc00", guide = "colourbar", 
                               name= 'Unmet Days') +
          theme(panel.background = element_rect(fill = "transparent"))+
          theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
          scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
          scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
          theme(axis.ticks= element_blank()) +
          theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
          theme(legend.title.align = 0.5) 
        
        unmet <- count_grid + new_scale_fill() +
          geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
          geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
          geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
          geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
          scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid="#63D1F4",
                               midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
        
        
        unmet_avg <- unmet + new_scale_fill()+
          geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
          geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
          geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
          geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
          scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                               name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
        unmet_avg
      } else{
        count_grid <- ggplot() +
          geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
          geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
          scale_fill_gradient2(low = "#00cc00", high = "red",mid ='yellow',
                               midpoint = 15, guide = "colourbar", 
                               name= 'Unmet Days') +
          theme(panel.background = element_rect(fill = "transparent"))+
          theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
          scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
          scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
          theme(axis.ticks= element_blank()) +
          theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
          theme(legend.title.align = 0.5) 
        
        unmet <- count_grid + new_scale_fill() +
          geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
          geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
          geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
          geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
          scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid='#CAB8FF',
                               midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
        
        
        unmet_avg <- unmet + new_scale_fill()+
          geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
          geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
          geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
          geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
          scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                               name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
        
        unmet_avg
      }
    }
    else{
      dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE)
      syear = min(dat$year)
      eyear = max(dat$year)
      if (syear != eyear) {
        sdate <- as.Date(paste0(syear,"-10-01"))
        edate <- as.Date(paste0(eyear,"-09-30"))
      } else {
        # special case to handle 1 year model runs
        # just omit January in order to provide a short warmup period.
        sdate <- as.Date(paste0(syear,"-02-01"))
        edate <- as.Date(paste0(eyear,"-12-31"))
      }
      dat <- window(dat, start = sdate, end = edate);
      mode(dat) <- 'numeric'
      scen.propname<-paste0('runid_', runid)
      
      # GETTING SCENARIO PROPERTY FROM VA HYDRO
      sceninfo <- list(
        varkey = 'om_scenario',
        propname = scen.propname,
        featureid = pid,
        entity_type = "dh_properties"
      )
     
      datdf <- as.data.frame(dat)
      modat <- sqldf("select month, avg(wd_mgd) as wd_mgd from datdf group by month")
      #barplot(wd_mgd ~ month, data=modat)
      
      # Calculate
      wd_mgd <- mean(as.numeric(dat$wd_mgd) )
      if (is.na(wd_mgd)) {
        wd_mgd = 0.0
      }
      gw_demand_mgd <- mean(as.numeric(dat$gw_demand_mgd) )
      if (is.na(gw_demand_mgd)) {
        gw_demand_mgd = 0.0
      }
      unmet_demand_mgd <- mean(as.numeric(dat$unmet_demand_mgd) )
      if (is.na(unmet_demand_mgd)) {
        unmet_demand_mgd = 0.0
      }
      ps_mgd <- mean(as.numeric(dat$discharge_mgd) )
      if (is.na(ps_mgd)) {
        ps_mgd = 0.0
      }
      
      # Analyze unmet demands
      flows <- zoo(as.numeric(dat$unmet_demand_mgd*1.547), order.by = index(dat));
      loflows <- group2(flows);
      
      unmet90 <- loflows["90 Day Max"];
      ndx = which.max(as.numeric(unmet90[,"90 Day Max"]));
      unmet90 = round(loflows[ndx,]$"90 Day Max",6);
      unmet30 <- loflows["30 Day Max"];
      ndx1 = which.max(as.numeric(unmet30[,"30 Day Max"]));
      unmet30 = round(loflows[ndx,]$"30 Day Max",6);
      unmet7 <- loflows["7 Day Max"];
      ndx = which.max(as.numeric(unmet7[,"7 Day Max"]));
      unmet7 = round(loflows[ndx,]$"7 Day Max",6);
      unmet1 <- loflows["1 Day Max"];
      ndx = which.max(as.numeric(unmet1[,"1 Day Max"]));
      unmet1 = round(loflows[ndx,]$"1 Day Max",6);
      
      #defines critical period based on Qintake if there is no unmet demand
      if (sum(datdf$unmet_demand_mgd)==0) {
        flows <- zoo(as.numeric(dat$Qintake*1.547), order.by = index(dat));
        loflows <- group2(flows)
        Qin30 <- loflows["30 Day Min"];
        ndx1 = which.min(as.numeric(Qin30[,"30 Day Min"]))
      }
      # Define year at which highest 30 Day Max occurs (Lal's code, line 405)
      u30_year2 = loflows[ndx1,]$"year";
      
      ##### Define data for graph, just within that defined year, and graph it
      # Lal's code, lines 410-446 (412 commented out)
      
      ddat2 <- window(dat, start = as.Date(paste0(u30_year2, "-06-01")), end = as.Date(paste0(u30_year2,"-09-15") ));
      
      #dmx2 = max(ddat2$Qintake)
      map2<-as.data.frame(ddat2$Qintake + (ddat2$discharge_mgd - ddat2$wd_mgd) * 1.547)
      colnames(map2)<-"flow"
      map2$date <- rownames(map2)
      map2$base_demand_mgd<-ddat2$base_demand_mgd * 1.547
      map2$unmetdemand<-ddat2$unmet_demand_mgd * 1.547
      
      df <- data.frame(as.Date(map2$date), map2$flow, map2$base_demand_mgd,map2$unmetdemand); 
      
      colnames(df)<-c("date","flow","base_demand_mgd","unmetdemand")
      
      #options(scipen=5, width = 1400, height = 950)
      ggplot(df, aes(x=date)) + 
        geom_line(aes(y=flow, color="Flow"), size=0.5) +
        geom_line(aes(y=base_demand_mgd, colour="Base demand"), size=0.5)+
        geom_line(aes(y=unmetdemand, colour="Unmet demand"), size=0.5)+
        theme_bw()+ 
        theme(legend.position="top", 
              legend.title=element_blank(),
              legend.box = "horizontal", 
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              legend.text=element_text(size=12),
              axis.text=element_text(size=12, color = "black"),
              axis.title=element_text(size=14, color="black"),
              axis.line = element_line(color = "black", 
                                       size = 0.5, linetype = "solid"),
              axis.ticks = element_line(color="black"),
              panel.grid.major=element_line(color = "light grey"), 
              panel.grid.minor=element_blank())+
        scale_colour_manual(values=c("purple","black","blue"))+
        guides(colour = guide_legend(override.aes = list(size=5)))+
        labs(y = "Flow (cfs)", x= paste("Critical Period:",u30_year2, sep=' '))
    }
  })
}

shinyApp(ui = ui, server = server)
