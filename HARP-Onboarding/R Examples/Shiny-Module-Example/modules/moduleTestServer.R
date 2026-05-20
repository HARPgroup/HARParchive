moduleTestServer <- function(id, usgsData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns<-NS(id)
      
      #Render a daily plot of flows to the plot container setup in the module UI
      output$dvPlot <- renderPlot({
        #Note how we call the reactive input usgsData, which is an input to the
        #module itself (see line 1 above). usgsData() must not be NULL (its
        #initial value!)
        if(!is.null(usgsData())){
          plot(usgsData()$Date, usgsData()$Flow)
        }
      })
      
      output$meanQ <- renderText({
        #the req() function means this won't execute if usgsData() is NULL
        req(usgsData())
        #We can store the reactive usgsData as a static variable only available
        #in this renderText() environment. It can't be used outside of this!
        allGageData <- usgsData()
        #Calculate a mean flow
        out <- mean(allGageData$Flow)
        #We can explicitly call what goes to the text output
        return(out)
      })
    }
  )
}