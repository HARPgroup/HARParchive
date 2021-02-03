library(shiny)
library(shinydashboard)
library(dataRetrieval)
library(dataRetrieval)
library(ggplot2)
library(datasets)
library(plotly)
library(gganimate)
library(png)
library(gifski)
library(leaflet)

site <- "http://deq2.bse.vt.edu/d.dh" 

basepath <-'/var/www/R'

source(paste(basepath,'config.R',sep='/'))
# source(paste(github_location,"hydro-tools/HARP-2020/Heatmap","unmet_heatmap.R", sep = "/"))


ui<-
dashboardPage(
  dashboardHeader(title="Harp Plots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("USGS Gage Plots", tabName="usgs",icon = icon('tint')),
      menuItem("VAHydro Plots",tabName="vahydro", icon = icon('tint')),
      menuItem("ELF Plots", tabName="elf", icon = icon('tint'))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("usgs",
        box(plotlyOutput("plot", height=225), width=6, height=250, background='black'),
        box(textInput("id",value = '13010065',label='Site ID'),
            dateInput("sdate", value=as.Date("2018-10-01"), label="Start Date"),
            dateInput("edate", value=as.Date("2019-09-30"), label="End Date")),
        box(plotlyOutput("plot2", height=225), width=6, height=235, background='black'),
        #box(leafletOutput("map"))
        ),
      tabItem("vahydro",
           box(plotOutput("plot3")),
           box(textInput("pid",value = 4964892,label='pid'),
               textInput("elid", value= 299330, label="elid"),
               textInput("runid", value=18, label="runid"))),
      tabItem("elf",
        box(plotOutput("elfplot")),
        box(textInput("hydroid",value = '59760',label='hydroid'),
          textInput("huc_level", value= 'huc8', label="huc level"),
          textInput("flow_metric", value='erom_q0001e_mean', label="flow metric"),
          textInput("flow_reduction_pct", value='10', label="flow reduction percent")),
        #box(textOutput("elftext"))
        
        
        )
    
 
    )))



server<-function(input, output){
  
  output$plot <-renderPlotly({

    pCode <-'00060'
    
    rawDailyQ <- readNWISdv(input$id, pCode, input$sdate, input$edate)
    
    p<-ggplot(rawDailyQ, 
           aes(x=Date, y = X_00060_00003))+
      geom_line(outlier.shape = NA, col='darkblue')+
      labs(title= 'USGS Gage Flow')+
      ylab('Discharge (cfs)')+ 
      xlab('Date')+
      theme_bw()+
      theme(plot.title = element_text(size = 8, face = "bold",  hjust = 0.5))
    
    ggplotly(p)
    
  })
  
  #output$map <- renderLeaflet({
    
    # siteinfo<- readNWISsite('13010065')
    # 
    # 
    # imap <- leaflet() %>%
    #   addTiles() %>%
    #   setView(lng = -78.90833333, lat = 38.05750000, zoom = 7) %>%
    #   addMarkers(lng = -78.90833333, lat = 38.05750000,
    #              popup = "Site #01626000 - South River")
    # imap
    

  output$plot2 <-renderPlotly({
    
    pCode <-'00060'
    
    rawDailyQ <- readNWISdv(input$id, pCode, input$sdate, input$edate)
    
    p<-ggplot(rawDailyQ, 
              aes(x=Date, y = X_00060_00003))+
      geom_line(outlier.shape = NA, col='darkblue')+
      labs(title= 'USGS Gage Flow (LOG)')+
      ylab('Discharge (cfs)')+ 
      xlab('Date')+
      scale_y_log10()+
      theme_bw()+
      theme(plot.title = element_text(size = 8, face = "bold",  hjust = 0.5))
    
    ggplotly(p)
  })
  output$plot3<-renderPlot({
    ###############################
    #### *** Water Supply Element
    ################################
    library(stringr)
    
    # dirs/URLs
    
    #----------------------------------------------
    site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
    save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');
    #----------------------------------------------
    # Load Libraries
    basepath='/var/www/R';
    source(paste(basepath,'config.R',sep='/'))
    
    save_directory <-  "/var/www/html/data/proj3/out"
    
    # Read Args
    pid <- input$pid #as.integer(argst[1])
    elid <-  input$elid #as.integer(argst[2])
    runid <- input$runid #as.integer(argst[3])
    
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
    scenprop <- getProperty(sceninfo, site, scenprop)
    # POST PROPERTY IF IT IS NOT YET CREATED
    if (identical(scenprop, FALSE)) {
      # create
      sceninfo$pid = NULL
    } else {
      sceninfo$pid = scenprop$pid
    }
    scenprop <- getProperty(sceninfo, site, scenprop)
    sceninfo <- list(
      varkey = 'om_scenario',
      propname = scen.propname,
      featureid = pid,
      entity_type = "dh_properties"
    )
    
    #omsite = site <- "http://deq2.bse.vt.edu"
    #dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE);
    #amn <- 10.0 * mean(as.numeric(dat$Qreach))
    
    #dat <- window(dat, start = as.Date("1984-10-01"), end = as.Date("2014-09-30"));
    #boxplot(as.numeric(dat$Qreach) ~ dat$year, ylim=c(0,amn))
    
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
    
    
    if (sum(datdf$unmet_demand_mgd)==0) {
      flows <- zoo(as.numeric(dat$Qintake*1.547), order.by = index(dat));
      loflows <- group2(flows)
      Qin30 <- loflows["30 Day Min"];
      ndx1 = which.min(as.numeric(Qin30[,"30 Day Min"]))
    }
    # Define year at which highest 30 Day Max occurs (Lal's code, line 405)
    u30_year2 = loflows[ndx1,]$"year";
    
    
    u30_year2 = loflows[ndx1,]$"year";
    
    ##### Define fname before graphing
    # hydroImpoundment lines 144-151
    
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
    #dev.off()
   
  })
  output$elfplot<-renderPlot({
    library(elfgen)
    library(sqldf)
    library(ggplot2)
    
    
    #### Load in directories and repositories
    site <- "http://deq2.bse.vt.edu/d.dh"
    save_directory <- "/var/www/html/data/proj3/out"
    save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');
    
    basepath ='/var/www/R';
    source(paste(basepath,'config.R',sep='/'))
    
    
    #### Take in input arguments
    
    # argst <- commandArgs(trailingOnly=T)
    # hydroid <- as.integer(argst[1])
    # huc_level <- as.integer(argst[2])
    # flow_metric <- as.integer(argst[3])
    # flow_reduction_pct <- as.integer(argst[4])
    
    hydroid <- input$hydroid
    huc_level<- input$huc_level
    flow_metric <- input$flow_metric
    flow_reduction_pct <- input$flow_reduction_pct
    
    #### Take in watershed and mean intake data
    
    site_comparison <- paste('http://deq1.bse.vt.edu/d.dh/dh-feature-contained-within-export', hydroid, 'watershed', sep = '/')
    
    containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")
    
    nhd_code <- sqldf(paste("SELECT hydrocode 
             FROM containing_watersheds 
             WHERE ftype = 'nhd_", huc_level,"'", sep = ""))
    
    hydroid2 <- sqldf("SELECT hydroid 
                  FROM containing_watersheds 
                  WHERE ftype 
                  LIKE '%nhdplus%'")
    
    #### Return property dataframe and mean intake
    
    inputs <- list(
      varkey = flow_metric,
      featureid = as.numeric(hydroid2$hydroid),
      entity_type = "dh_feature"
    )
    
    dataframe <- getProperty(inputs, site)
    
    mean_intake <- dataframe$propvalue
    
    #### Input parameters for retrieving data from VAHydro
    
    watershed.code <- as.character(nhd_code$hydrocode)
    watershed.bundle <- 'watershed'
    watershed.ftype <- paste("nhd_", huc_level, sep = "")
    x.metric <- flow_metric
    y.metric <- 'aqbio_nt_total'
    y.sampres <- 'species'
    
    # elfdata_vahydro() function for retrieving data from VAHydro
    watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,site)
    # clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
    watershed.df <- clean_vahydro(watershed.df)
    
    elf_quantile <- 0.80
    breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = elf_quantile, "blo" = 100, "bhi" = 1000)  
    
    elf <- elfgen("watershed.df" = watershed.df,
                  "quantile" = elf_quantile,
                  "breakpt" = breakpt,
                  "yaxis_thresh" = 53, 
                  "xlabel" = flow_metric,
                  "ylabel" = "Fish Species Richness")
    
    
    #### Solving for confidence interval lines
    
    xdat <- c(elf$plot$data$x_var)
    ydat <- c(elf$plot$data$y_var)
    data <- as.data.frame(elf$plot$data)
    uq <- elf$plot$plot_env$upper.quant
    
    upper.lm <- lm(y_var ~ log(x_var), data = uq)
    
    predict <- as.data.frame(predict(upper.lm, newdata = data.frame(x_var = mean_intake), interval = 'confidence'))
    
    species_richness<-elf$stats$m*log(mean_intake)+elf$stats$b
    
    # Comparing predict to actual values
    #fit<-as.numeric(predict$fit)
    #species_richness<-elf$stats$m*log(mean_intake)+elf$stats$b
    #percent_error<-((fit-species_richness)/species_richness)*100
    
    
    xmin <- min(uq$x_var)
    xmax <- max(uq$x_var)
    
    yval1 <- predict(upper.lm, newdata = data.frame(x_var = xmin), interval = 'confidence')
    yval2 <- predict(upper.lm, newdata = data.frame(x_var = xmax), interval = 'confidence')
    
    ymin1 <- yval1[2] # bottom left point, line 1
    ymax1 <- yval2[3] # top right point, line 1
    
    ymin2 <- yval1[3] # top left point, line 2
    ymax2 <- yval2[2] # bottom right point, line 2
    
    m <- elf$stats$m
    b <- elf$stats$b
    int <- m*log(mean_intake) + b      # solving for mean_intake y-value
    
    m1 <- (ymax1-ymin1)/(log(xmax)-log(xmin)) # line 1
    b1 <- ymax1-(m1*log(xmax))
    
    m2 <- (ymax2-ymin2)/(log(xmax)-log(xmin)) # line 2
    b2 <- ymax2 - (m2*log(xmax))
    
    
    #### Plot
    
    elf$plot +
      geom_segment(aes(x = mean_intake, y = -Inf, xend = mean_intake, yend = int), color = 'red', linetype = 'dashed') +
      geom_segment(aes(x = 0, xend = mean_intake, y = int, yend = int), color = 'red', linetype = 'dashed') +
      geom_point(aes(x = mean_intake, y = int, fill = 'Mean intake flow'), color = 'red', shape = 'triangle', size = 2) +
      geom_segment(aes(x = xmin, y = (m1 * log(xmin) + b1), xend = xmax, yend = (m1 * log(xmax)) + b1), color = 'blue', linetype = 'dashed') +
      geom_segment(aes(x = xmin, y = (m2 * log(xmin) + b2), xend = xmax, yend = (m2 * log(xmax)) + b2), color = 'blue', linetype = 'dashed') +
      labs(fill = 'Intake Legend')
    
    
  })
  
  #from hear on is not functional, need to learm more about reactivity
  
  pleasework <- reactive({
    
    elf$stats$m <- m1
    elf$stats$b <- b1
    
    pct_richness_change_1 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct, "xval" = mean_intake)
    abs_richness_change_1 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct)
    
    elf$stats$m <- m2
    elf$stats$b <- b2
    
    pct_richness_change_2 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct, "xval" = mean_intake)
    abs_richness_change_2 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct)
    
  })
  output$elftext <- renderText({
    
    paste('Percent Richness Change at intake ranges from ',
          round(pleasework()$pct_richness_change_2, digits = 5), 'to ', 
          round(pleasework()$pct_richness_change_1, digits = 5), 'percent.')
    
  })
  
  
    
 

}
shinyApp(ui, server)