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
          menuItem("Tributaries Comparison", tabName= "tributaries", icon = icon('tint'))
        )
      ),
      
      dashboardBody( 
        fluidRow(
          tabItems(
            
            tabItem("tributaries",
                    box(plotOutput("plot", height=680), width=6, height=700, background='blue'),
                    box(textInput("riv_seg",value = 'OR2_7900_7740',label='Segment ID'), width =3),
                    box(numericInput("runid1", value = 11,label='Runid #1'), width =3 ),
                    box(textInput("flow_metric", value = "7q10", label="Flow Metric"), width=3),
                    box(numericInput("runid2",value = 18,label='Runid #2'), width =3),                    
                    submitButton("Update Run Conditions")
                    
                    
            )
                    
            )
          )
        )
      )
    )
  
#----

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # # #GENERATE LIST OF RSEGS IN VAHYDRO, WILL REPLACE THE AllSegList IN THE CIA_data FUNCTION
   RSeg.csv <- map_layers[[which(names(map_layers) == "RSeg.csv")]]
   AllSegList <- sub("vahydrosw_wshed_", "", RSeg.csv$hydrocode)
  source(paste(github_location,'hydro-tools/R/cia_utils.R',sep='/'))
 # source(paste(github_location,'hydro-tools/R/fn_plot_cia_dend.R',sep='/'))
  
  
  
    mydata <- reactive({
    

      riv_seg <- input$riv_seg
      runid1 <- input$runid1
      runid2 <- input$runid2
      flow_metric <- input$flow_metric
      AllSegList <- AllSegList
      
      
    cia_data <- CIA_data(riv_seg, runid1 , runid2, flow_metric, AllSegList)
    
    library(ggplot2)
    library(plotly)    
    
    # fn_plot_cia_dend.R
    # Main Plot for the CIA static and Shiny dashboards.
    # fn_plot_cia_dend Function: Takes CIA_data out put and returns plots
    #As of latest update to code 
    #we did not figure out a way to separte this into multiple functions because of the way the graphing is done
    fn_plot_cia_dend <- function(riv_seg, AllSegList, runid1, runid2, cia_data_frame, flow_metric){
      
      #Declaring initial inputed river segment for graphing dot
      riv_seg_i <- riv_seg
      
      #Calculates Upstream River Segments
      upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
      names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
      
      #While loop that runs the function for every upstream segment
      a <- 1
      cia_data <- data.frame()
      p <- ggplot(NULL)
      while(a <= nrow(upstream)){
        if(upstream == 'NA'){
          riv_seg <- riv_seg
        }else{
          riv_seg <- upstream[a,]
        }
        #only runs code if river segment is headwater
        if(fn_ALL.upstream(riv_seg,AllSegList) == 'NA'){
          #determines all downstream segments
          downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
          names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
          riv_seg <- as.data.frame(riv_seg)
          #creates dataframe of river segment and all downstream segments
          river <- rbind(riv_seg, downstream)
          names(river)[names(river) == colnames(river)[1]] <- "riv_seg"
          
          #pulls river data from river segments that match headwater and its downstream segs
          cia_data_loop <- sqldf("SELECT * FROM river join cia_data_frame
                        WHERE riv_seg like riverseg")
          
          #Adding length segments together to form river mile (distance from headwater) column
          i <- 1
          while (i <= nrow(cia_data_loop)) {
            
            river_length <- c()
            
            #Loop creates vector of current segment and upstream segment lengths
            for (n in 1:i) {
              n_length <- as.numeric(cia_data_loop$length[n])
              river_length <- c(river_length, n_length)
            }
            #Makes length column to total length to segment from start of river
            cia_data_loop$mile[i] <- sum(river_length)
            
            i <- i + 1
          }
          
          #Creating a river mile column
          for (i in 1:(length(cia_data_loop$mile))){
            if(i == 1){
              cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)]
            }
            else{
              cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)] - cia_data_loop$mile[i-1]
            }
          }
          
          #combine current data frame with new data frame
          cia_data <- rbind(cia_data_loop, cia_data)
          
          #plot graph
          p <- p +
            geom_line(data = cia_data_loop, aes(x = rmile, y = Metric_1, colour = Metric_change, size = metric_pc))
          
          
        }
        
        a <- a + 1
      }
      
      # Reversing scale for correct river mile orientation
      p <- p + scale_x_reverse()
      
      #Creating data frame with segment ID numbers
      cia_data <- cia_data[!duplicated(cia_data$riv_seg),]
      #Makes numbers ordered by river mile (is this whats best? should we make it based on tributary?)
      cia_data <- cia_data[order(cia_data$rmile, decreasing = TRUE),]
      cia_data$seglist <- 1:nrow(cia_data)
      
      #Creating data frame with just original inputed river segment to graph point
      riv_seg_og <- cia_data[cia_data$rivseg == riv_seg_i,]
      
      p <- p +
        geom_point(data = riv_seg_og, aes(x = rmile, y = Metric_1)) +
        geom_text(data = cia_data, aes(x = rmile, y = Metric_1, label = seglist, vjust = 1.0)) + 
        scale_size_continuous(range = c(0.2, 3), name = "Percent Change") +
        scale_colour_manual(values = c("blue", "brown"), name = "Percent Change") +
        theme_bw() +
        ggtitle(paste0("Percent Change in ", flow_metric, " Flow between runid", runid1, " and runid", runid2)) +
        xlab('River Mile [Mi]') +
        ylab('Flow [cfs]')
      
      return(p)
    }
    
    cia_plot <- fn_plot_cia_dend(riv_seg, AllSegList, runid1, runid2, cia_data_frame = cia_data, flow_metric)
    
    #TAKE OUT LOCAL FUNC AND REPLACE WITH HYDROTOOLS SOURCED FUNC WHEN FIXED
 })
  

  
  output$plot <- renderPlot({
    
    library(plotly)
    
   dend_plot_shiny <-  mydata()

  dend_plot_shiny
    
  })
  
  
  #output$table <- DT :: renderDataTable({
    # 
    # library(DT)
    # library(kableExtra)
    # 
    # cia_list <-  mydata(cia_data)
    # cia_df<- data.frame(cia_list)
    # 
    # is.num <- sapply(cia_df, is.numeric)
    # cia_df[is.num] <- lapply(cia_df[is.num], round, 3)
    # 
    # #datatable(cia_df, class="compact cell-border", colnames= c("Segment Name","River Segment ID","River Mile","Flow Metric 1","Flow Metric 2","QBaseline 1",
    # #"QBaseline 2","Cumulative Withdrawal 1 (mgd)","Cumulative Withdrawal 2 (mgd)",
    # # "Cumulative Point Source 1 (mgd)","Cumulative Point Source 2 (mgd)"), options = list("pageLength" = 25))
    # 
    # 
    # cia_data_table <- data.frame(cia_df$propname, cia_df$riverseg, cia_df$mile, cia_df$flow1, cia_df$flow2,
    #                              cia_df$Qbaseline1, cia_df$Qbaseline2, cia_df$wd_cumulative_mgd1, cia_df$wd_cumulative_mgd2,
    #                              cia_df$ps_cumulative_mgd1, cia_df$ps_cumulative_mgd2)
    # 
    # datatable(cia_data_table, class="compact cell-border", colnames= c("Segment Name","River Segment ID","River Mile","Flow Metric 1","Flow Metric 2","QBaseline 1",
    #                                                                    "QBaseline 2","Cumulative Withdrawal 1 (mgd)","Cumulative Withdrawal 2 (mgd)",
    #                                                                    "Cumulative Point Source 1 (mgd)","Cumulative Point Source 2 (mgd)"), options = list("pageLength" = 25))
  #})
  
  
}

# Run the application
shinyApp(ui = ui, server = server)