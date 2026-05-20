## Shydrology UI
moduleTestUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    wellPanel(
      plotOutput(ns("dvPlot")),
      fluidRow(
        column(6, "Mean Flow:"),
        column(6, textOutput(ns("meanQ")))
      )
    )
    
  )
  
}