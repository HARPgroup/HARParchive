# Define server logic required to draw a histogram
server <- function(input, output) {

  gageData <- reactiveVal(NULL)
  observeEvent(input$getData,{
    #Try to get daily flow values (parameterCd = 00060) for strasburg
    tryGage <- dataRetrieval::readNWISdv("01631000","00060")
    #Store the gage data in the reactive if data was actually returned
    if(nrow(tryGage) > 0){
      #Rename columns
      tryGage <- renameNWISColumns(tryGage)
      gageData(tryGage)
    }else{
      #Reset the reactive if it failed
      showNotification("Couldn't get gage data.")
      gageData(NULL)
    }
  })
    
  #Call our module! Note that we pass in the reactive gageData WITHOUT
  #parentheses
  moduleTestServer("plotData", gageData)
  
  
}
