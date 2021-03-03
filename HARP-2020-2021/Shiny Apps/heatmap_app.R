#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(sqldf)
library(ggnewscale)
library(shinydashboard)


# source config.r
basepath='/var/www/R'
site <-"http://deq2.bse.vt.edu/d.dh" 
source(paste(basepath,'config.R',sep='/'))


# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="HARP Plots"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Unmet Demand Heatmap", tabName="heatmap",icon = icon('tint'))
        )
    ),
        # Show a plot of the generated distribution
        dashboardBody(
            tabItems(
                tabItem("heatmap",
                        box(plotOutput("plot", height=700, width = 675 ),width=7, background='blue'),
                        box(numericInput("pid",
                                     "PID:",
                                     min = 1,
                                     max = 90000000,
                                     value = 4964892),
                        numericInput("elid",
                                     "ELID:",
                                     min = 1,
                                     max = 90000000,
                                     value = 299330),
                        numericInput("runid",
                                     "RunID:",
                                     min = 1,
                                     max = 1000,
                                     value = 18),
                        dateInput('sdate','Start Date',value = "1985-01-01",
                                  min='1984-01-01', max='2014-12-31'),
                        dateInput('edate','End Date',value = "2014-12-31",
                                  min='1984-01-01', max='2014-12-31'),
                        width=4)
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        unmet_grid <- function(elid,pid,runid,start_date,end_date) {
            dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) # get data
            # set start and end dates based on inputs
            if (missing(start_date)) {
                syear <- min(dat$year)
                sdate <- as.Date(paste0(syear,"-01-01"))
            } else {
                sdate <- start_date
                syear <- format(as.Date(start_date, format="%Y-%m-%d"),"%Y")
            }
            if (missing(end_date)) {
                eyear <- max(dat$year)
                edate <- as.Date(paste0(eyear,"-12-31"))
            } else {
                edate <- end_date
                eyear <- format(as.Date(end_date, format="%Y-%m-%d"),"%Y")
            }   
            dat <- window(dat, start = sdate, end = edate);
            mode(dat) <- 'numeric'
            
            #######################################
            # section that would get scen prop and info and post to va hydro/ not in use now/yet
            # scen.propname<-paste0('runid_', runid)
            # 
            # # GETTING SCENARIO PROPERTY FROM VA HYDRO
            # sceninfo <- list(
            #   varkey = 'om_scenario',
            #   propname = scen.propname,
            #   featureid = pid,
            #   entity_type = "dh_properties"
            # )
            # scenprop <- getProperty(sceninfo, site, scenprop)
            # # POST PROPERTY IF IT IS NOT YET CREATED
            # if (identical(scenprop, FALSE)) {
            #   # create
            #   sceninfo$pid = NULL
            # } else {
            #   sceninfo$pid = scenprop$pid
            # }
            # scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
            # scenprop <- getProperty(sceninfo, site, scenprop)
            # sceninfo <- list(
            #   varkey = 'om_scenario',
            #   propname = scen.propname,
            #   featureid = pid,
            #   entity_type = "dh_properties"
            # )
            ###################################
            
            # change runfile to data frame
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
            
            moavg$avg<-round(moavg$avg, 2)
            
            # create yearly averages
            yeavg <-  
                yesum %>%
                mutate(avg=count_unmet_days/12) %>%
                mutate(month=14)
            
            yeavg$avg<-round(yeavg$avg, 2)
            
            # create x and y axis breaks
            y_breaks <- seq(syear,num_eyear+2,1)
            x_breaks <- seq(1,14,1)
            
            # create x and y labels
            y_labs <- c(seq(syear,eyear,1),'Totals', 'Avg')
            x_labs <- c(month.abb,'Totals','Avg')
            
            
            ############################################################### Plot and Save
            if (sum(yesum$count_unmet_days)==0) {
                count_grid <- ggplot() +
                    geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
                    geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
                    scale_fill_gradient2(low = "#00cc00", high = "#00cc00",mid ='#00cc00',
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
                    scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid='#63D1F4',
                                         midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
                
                
                unmet_avg <- unmet + new_scale_fill()+
                    geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
                    geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
                    geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
                    geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
                    scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#DEB887",
                                         name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
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
            }
        
            return(unmet_avg)
        }
        
        print(unmet_grid(input$elid,input$pid,input$runid,start_date=input$sdate,end_date=input$edate))
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
