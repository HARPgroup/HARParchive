# Define UI for application that downloads gage data and plots it
ui <- fluidPage(

  wellPanel(
    fluidRow(
      column(6, "User can click button to get Strasburg USGS data!"),
      column(6, actionButton("getData","Get USGS Data:"))
    )
  ),
  #Call out module
  moduleTestUI("plotData")
)
