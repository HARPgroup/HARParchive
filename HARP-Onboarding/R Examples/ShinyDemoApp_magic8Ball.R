library(shiny)
library(raster)

#Put the code to build the user interface here. Usually starts with a fluidPage,
#then some columns and fluidRows populated by selectInput, numericInput,
#textInput, etc.
ui <- fluidPage(
  titlePanel(title = "Magic 8 Ball"),
  #Use half the page. Each page has 12 columns
  column(width = 6,
         #Put everything in a nice panel
         wellPanel(
           p(
             "Enter a question",
             strong("And here is some bolded text as well!")
           ),
           #Below is a numeric input:
           textInput("textIn","What is your 'Yes' or 'No' Question?"),
           uiOutput("test")
         )
  ),
  column(width = 6,
         fluidRow(
           column(6,
                   em("Click 'Run' and let your fate be revealed:")
           ),
           column(6,
                   actionButton("run","Run"),
           )
         ),
         verbatimTextOutput("answer")
  )
)

server <- function(input, output, session) {
  observeEvent(input$textIn,{
    test <- raster::raster('https://water.noaa.gov/resources/downloads/precip/stageIV/2024/11/20/nws_precip_wytd_20241120_conus.tif')
  })
  
  output$test <- renderUI({
    if(input$textIn == "idk"){
      actionButton("test2","Figure it out")
    }else if(input$textIn == "idk2"){
      selectInput("test3","Check it",c(1,2,3))
    }else{
      NULL
    }
  })
  
  #We create an empty reactive variable. This can be used in any number of
  #observe or reactive contexts, which means this is a global variable.
  magicResponse <- reactiveVal()
  
  #Here, we 'observe' the reactive input run. In other words, each time the
  #button (or event) called "run" is clicked, this code will run. The variables
  #we declare here cannot be used outside of this observe UNLESS we put them in
  #a reactiveVal
  observeEvent(input$run,{
    
    #First, check to see if the user input text in our textIn value within the
    #interface
    if(input$textIn == ""){
      #If they did not enter any text, display a warning and render nothing to
      #output$answer
      showNotification("Please enter a message.")
      sendResponse <- NULL
    }else{
      #If a question was entered, output the response!
      #When the button is clicked, generate a random number 1:9
      randNum <- sample(1:9,1)
      #Determine which of the classic responses should be sent
      possibleResponses <- c(
        "It is certain.", "It is decidedly so.",
        "Without a doubt.", "Yes definitely.",
        "Better not tell you now.", "Don't count on it.",
        "My reply is no.", "My sources say no.",
        "Outlook not so good."
      )
      #Store the response to send in a variable that CANNOT be used outside of
      #this observeEvent
      sendResponse <- possibleResponses[randNum]
    }

    
    #Now, store in our reactiveVal. Now, this can be used elsewhere i.e. in
    #other observe or reactive contexts!
    magicResponse(sendResponse)
  })
  
  #We defined a textOutput in our UI. We will "render" text to it using a
  #reactive context so that this can update with our reactiveVal above!
  output$answer <- renderText({
    #Call our reactiveVal which updates with each click of the button
    magicResponse()
  })
  

}

shinyApp(ui, server)