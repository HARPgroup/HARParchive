##USGS Gage Interactive Flow Graph Shiny App

#Load Libraries
library(shiny)
library(ggplot2)
library(dataRetrieval)

#Fluid Page
ui <- fluidPage(
  titlePanel("Interactive Streamflow Graph"),
  sidebarLayout(
    sidebarPanel(
      textInput("gage_id", "USGS Gage ID", value = "01646502"),
      dateInput("start_date", "Start Date", value = "1985-01-01"),
      dateInput("end_date", "End Date", value = "2014-12-31")
    ),
    mainPanel(
      plotOutput(outputId = "flowplot"),
    )
  )
)

#Server Function
server <- function(input, output) {
  output$flowplot <- renderPlot({
    start_date <- input$start_date
    end_date <- input$end_date
    gage_id <- input$gage_id
    dat <- readNWISdv(gage_id, '00060', start_date, end_date)
    flowplot <- ggplot(dat, aes(x=dat$Date, y=dat$X_00060_00003))+
                    geom_line()+
                    theme_bw()+
                    labs( title = paste('Streamflow of USGS gage site: ', gage_id, sep=""), x = paste(start_date, " to ", end_date, sep = ''), y = 'Flow (cfs)') +
                    theme(plot.title = element_text(face = 'bold',hjust = 0.5))
    flowplot
  })
}

#Running the app
shinyApp(ui=ui, server=server)

