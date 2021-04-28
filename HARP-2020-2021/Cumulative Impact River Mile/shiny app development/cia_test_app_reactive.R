library(shiny)
library(shinydashboard)
library(plotly)
library(hydrotools)
library(leaflet)
library(DT)
library(kableExtra)

#PS3_5990_6161
#OR2_7900_7740

#RUN THIS BEFORE CLICKLING "RUN APP"
site <- "http://deq2.bse.vt.edu/d.dh"
basepath <-'/var/www/R'
source(paste(basepath,'config.R',sep='/'))
#source(paste(github_location,"/HARParchive/HARP-2020-2021/Cumulative Impact River Mile/CIA_maps_shiny.R",sep = '/'))



# #fn_auth_read()


# localpath <- tempdir()
# filename <- paste("vahydro_riversegs_export.csv",sep="")
# destfile <- paste(localpath,filename,sep="\\")
# download.file(paste(site,"/vahydro_riversegs_export",sep=""), destfile = destfile, method = "libcurl")
# RSeg.csv <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")


# # Trying to get All.Seglist to work
# AllSegList <- RSeg.csv$hydrocode
# AllSegList <- substring(AllSegList, 17)



#LOAD MAP LAYERS
source(paste(github_location,"/HARParchive/HARP-2020-2021/Cumulative Impact River Mile/CIA_maps.R",sep = '/'))
if(!exists("map_layers")) {map_layers <- load_MapLayers(site = "http://deq2.bse.vt.edu/d.dh")} #Load map layers if they're not already loaded in the RStudio environment
#fn_auth_read could be used here to make more effecient, durable


#ui
#----
ui <- 
  fluidPage(
    dashboardPage(
      dashboardHeader(title="Cumulative Impact Analysis"),
      dashboardSidebar( 
        sidebarMenu(
          menuItem("Main Stem Comparison", tabName = "mainstem", icon = icon('tint'))
        )
      ),
      
      dashboardBody( 
        fluidRow(
          tabItems(
        
            
            tabItem("mainstem",
                    box(plotlyOutput("plot", height=680), width= 6 , height=700, background='blue'),
                    submitButton("Update Run Conditions"),
                    box(textInput("riv_seg",value = 'OR2_7900_7740',label='Segment ID'), width =2),
                    box(textInput("flow_metric", value = "7q10", label="Flow Metric"), width = 2),
                    box(numericInput("runid1", value = 11,label='Runid #1'), width = 1),
                    box(numericInput("runid2",value = 18,label='Runid #2'), width = 1),
                    box(plotOutput("map"), width = 5, height = 550),
                    box(DT :: dataTableOutput("table"), width = 10)
               
            )
          )
        )
      )
    )
  )
#----

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  mydata <- reactive({
    
    CIA_data <- function(riv_seg, runid1, runid2, flow_metric, AllSegList){
      

      # # #GENERATE LIST OF RSEGS IN VAHYDRO, WILL REPLACE THE AllSegList IN THE CIA_data FUNCTION
       RSeg.csv <- map_layers[[which(names(map_layers) == "RSeg.csv")]]
       AllSegList <- sub("vahydrosw_wshed_", "", RSeg.csv$hydrocode)
      
      
      #Finding headwater and all downstring river segments
      upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
      upstream <- upstream[nrow(upstream):1,]
      upstream <- data.frame(upstream)
      names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
      
      upstream <- as.character(upstream[[1,1]])
      
      if(upstream == 'NA'){
        riv_seg <- riv_seg
      }else {
        riv_seg <- upstream
      }
      
      downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
      names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
      riv_seg <- as.data.frame(riv_seg)
      river <- rbind(riv_seg, downstream)
      
      #setting up dataframe for om_vahydro_metric_grid
      df <- data.frame(
        'model_version' = c('vahydro-1.0'),
        'runid' = c(paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2),
                    paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), '0.%20River%20Channel', 'local_channel'),
        'runlabel' = c('flow1', 'flow2','Qbaseline1','Qbaseline2', 'wd_cumulative_mgd1','wd_cumulative_mgd2','ps_cumulative_mgd1','ps_cumulative_mgd2', 'length', 'subcomp_da'),
        'metric' = c(flow_metric, flow_metric, 'Qbaseline','Qbaseline',
                      'wd_cumulative_mgd', 'wd_cumulative_mgd','ps_cumulative_mgd', 'ps_cumulative_mgd', 'length', 'drainage_area')
      )
      
      #importing dataframe of river segment metrics
      wshed_data <- om_vahydro_metric_grid(metric, df)
      
      #triming dataframe to just river segments on river of interest
      cia_data <- sqldf("SELECT * FROM river join wshed_data
                    WHERE riverseg like riv_seg")
      
      #Convert length column to miles
      cia_data$length <- cia_data$length/5280
      
      #pull the values that exist from length and subcomp_da (One of the two will be NA for each segment)
      da_data <- sqldf("SELECT pid, length, subcomp_da,
                    CASE
                    WHEN length is null then subcomp_da
                    ELSE length
                    END as da
                    from cia_data")
      
      #selecting the values that are positive
      cia_data$length <- da_data$da
      
      # Must make seg list numbered for x axis on graphs with bars, could be used in table companion
      cia_data$seglist = as.numeric(c(1:nrow(cia_data)))
      
      #Adding length segments together to form river mile column
      i <- 1
      while (i <= nrow(cia_data)) {
        
        river_length <- c()
        
        # Loop creates vector of current segment and upstream segment lengths
        for (n in 1:i) {
          n_length <- as.numeric(cia_data$length[n])
          river_length <- c(river_length, n_length)
        }
        # Makes length column to total length to segment from start of river
        cia_data$mile[i] <- sum(river_length)
        
        i <- i + 1
      }
      
      res <- list(cia_data)
      
    }
    
    cia_data <- CIA_data(riv_seg = input$riv_seg, runid1 = input$runid1, runid2 = input$runid2, flow_metric = input$flow_metric, AllSegList = input$AllSegList)
    
  })
  
 
  output$map <- renderPlot({
    
    
    source(paste(github_location,"/HARParchive/HARP-2020-2021/Cumulative Impact River Mile/CIA_maps.R",sep = '/'))
    
    cia_list <-  mydata()
    cia_df<- data.frame(cia_list)
    
    export_path <- "C:/Users/nabra/Documents"
    cia_map<- CIA_maps(cia_df, map_layers = map_layers)
    
    plot(cia_map$plot_env$plot)
    
    # ggsave(paste0(export_path,riv_seg,"_cia_map.png",sep = ""), width=5.5, height=5)
    
    
  } )
  output$plot <- renderPlotly({
    
    cia_list <-  mydata()
    cia_df<- data.frame(cia_list)
    
    library(ggplot2)
    library(plotly)
    
    p<-ggplot(cia_df, aes(x = mile)) +
      geom_point(aes(y = flow1, colour = paste0('Runid',input$runid1), text= propname)) +
      geom_line(aes(y = flow1, colour = paste0('Runid',input$runid1))) +
      geom_point(aes(y = flow2, colour = paste0('Runid',input$runid2), text= propname)) +
      geom_line(aes(y = flow2, colour = paste0('Runid',input$runid2))) +
      labs(colour = 'Legend') +
      ggtitle(paste('Comparison of', (input$flow_metric), 'Flow')) +
      xlab('River Mile [mi]') +
      ylab(paste0('Flow [cfs]'))
    
    
    ggplotly(p, tooltip = c("text", "mile", "flow1", "flow2"))
    
    
  })
  output$table <- DT :: renderDataTable({
    
    library(DT)
    library(kableExtra)
    
    cia_list <-  mydata()
    cia_df<- data.frame(cia_list)
    
    is.num <- sapply(cia_df, is.numeric)
    cia_df[is.num] <- lapply(cia_df[is.num], round, 3)
    
    #datatable(cia_df, class="compact cell-border", colnames= c("Segment Name","River Segment ID","River Mile","Flow Metric 1","Flow Metric 2","QBaseline 1",
                                                             #"QBaseline 2","Cumulative Withdrawal 1 (mgd)","Cumulative Withdrawal 2 (mgd)",
                                                            # "Cumulative Point Source 1 (mgd)","Cumulative Point Source 2 (mgd)"), options = list("pageLength" = 25))

    
    cia_data_table <- data.frame(cia_df$propname, cia_df$riv_seg, cia_df$mile, cia_df$flow1, cia_df$flow2,
                                 cia_df$Qbaseline1, cia_df$Qbaseline2, cia_df$wd_cumulative_mgd1, cia_df$wd_cumulative_mgd2,
                                 cia_df$ps_cumulative_mgd1, cia_df$ps_cumulative_mgd2)
    
    datatable(cia_data_table, class="compact cell-border", colnames= c("Segment Name","River Segment ID","River Mile","Flow Metric 1","Flow Metric 2","QBaseline 1",
                                                                       "QBaseline 2","Cumulative Withdrawal 1 (mgd)","Cumulative Withdrawal 2 (mgd)",
                                                                       "Cumulative Point Source 1 (mgd)","Cumulative Point Source 2 (mgd)"), options = list("pageLength" = 25))
  })
  


  
  #test 
  
}

# Run the application
shinyApp(ui = ui, server = server)