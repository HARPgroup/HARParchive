library(shiny)
library(tidyverse)
library(DT)

#Create a bogus data set with 10000 points and a few fields we may want to filter
#by
dfForDisplay <- data.frame(
  ID = as.integer(rnorm(10000,100,5)),
  SampleType = sample(LETTERS,10000,TRUE),
  Species = sample(iris$Species,10000,TRUE),
  SampleDate = sample(seq(as.Date('1980-01-01'), as.Date('2023-12-31'), by="day"),10000, TRUE),
  Type = sample(c("WELL","INTAKE","VPDES","OTHER"),10000,TRUE),
  value = rnorm(10000,1000,79)
)

#First, a reference. I used tutorials similar to this to get started in Shiny:
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/

#Second, you can use browser() within a reactive expression to stop the app and
#explore what's happening. very useful for debugging!


#Put the code to build the user interface here. Usually starts with a fluidPage,
#then some columns and fluidRows populated by selectInput, numericInput,
#textInput, etc.
#fluidPage = Sets up the entire HTML page. Usually start with this and everythin
#else goes inside
#column = divide section into 12 columns. Specify how many you want to take up
#with content
#fluidRow = A fluidRow can contain columns. Use with column for consistency of
#output
#Anything with output in its name like verbatimTextOutput or DTOutput are just
#output spaces that we can feed values into via the server code below. Think of
#these as blank spaces that are expecting values from our code. Like DTOutput
#below expects a table, but we need to first filter the data in server below!
#NOTE: Defining the UI is static so we don't/can't use reactiveValues in setting
#up these inputs
ui <- fluidPage(
  titlePanel(title = "Display and Filter"),
  wellPanel(
    fluidRow(
      column(6,
             selectInput(inputId = "IDFilter",
                         label = "ID Filter:",
                         choices = unique(dfForDisplay$ID)[order(unique(dfForDisplay$ID))],
                         multiple = TRUE),
             selectInput(inputId = "sampleTypeFilter",
                         label = "SampleType Filter:",
                         choices = unique(dfForDisplay$SampleType)[order(unique(dfForDisplay$SampleType))],
                         multiple = TRUE)
      ),
      column(6,
             checkboxGroupInput(inputId = "typeFilter",
                                label = "Type Filter:",
                                choices = unique(dfForDisplay$Type)[order(unique(dfForDisplay$Type))],
                                inline = TRUE),
             numericInput(inputId = "valueFilter",
                          label = "Display all Values Less Than:",
                          value = NULL)
      )
    )
  ),
  verbatimTextOutput("dataCount"),
  DTOutput("tableOutput")
)

server <- function(input, output, session) {
  #First, we make a "reactive" data frame of our existing data to make it easier
  #to edit it as users change inputs. "Reactive" data can be updated as we go,
  #whereas traditional vectors, data frames, etc. are static and are only
  #updated when the app first runs (with some exceptions). Note, "reactiveVal"
  #store themselves similar to functions. So, to get our value, we often need to
  #use reactiveDF(). reactvieDF() can be called from all other reactive
  #contexts. It's kind of like a global variable for this application!
  reactiveDF <- reactiveVal(dfForDisplay)
  
  #If we want to do something each time data changes, we can update data using
  #an observe or an observeEvent(). Below, we update reactiveDF() each time the
  #user changes an input. Note that to update reactiveDF(), we put the new value
  #IN the (). So, reactiveValue1(0) would set reactiveValue1 to 0!
  #With observe() below, the code in observe() updates any time any input or
  #reactive value reference in observe changes!
  observe({
    #First, when a user changes an input, we start with the entire STATIC
    #dfForDisplay. This makes the app work when users apply filters and then
    #remove them later on
    dataForDisplay <- dfForDisplay
    #We could filter the data if we want using the inputs provided by the user.
    #We can easily clean up the below code with better default values or a
    #function:
    if(!is.null(input$IDFilter)){
      dataForDisplay <- dataForDisplay %>% 
        filter(ID %in% input$IDFilter)
    }
    if(!is.null(input$sampleTypeFilter)){
      dataForDisplay <- dataForDisplay %>% 
        filter(SampleType %in% input$sampleTypeFilter)
    }
    if(!is.null(input$typeFilter)){
      dataForDisplay <- dataForDisplay %>% 
        filter(Type %in% input$typeFilter)
    }
    if(!is.null(input$valueFilter) && !is.na(input$valueFilter)){
      dataForDisplay <- dataForDisplay %>% 
        filter(value < input$valueFilter)
    }
    
    #DON'T FORGET TO UPDATE reactiveDF()!
    reactiveDF(dataForDisplay)
  })
  
  #Now, each time reactiveDF changes, let's update a statement that tells us how
  #much data is left. Lets start by defining a reactiveVal with the total number
  #of data rows in dfForDisplay (the STATIC data we created outside of the app)
  dataCount <- reactiveVal(nrow(dfForDisplay))
  
  #Each time the user filters the data and reactiveDF changes, we want dataCount
  #to update as well! We could put this in the observe above, or we can use a
  #separate observeEvent to execute any time reactiveDF changes!
  observeEvent(reactiveDF(),{
    #We can call our reactiveVal to get its current value. Note that we have to
    #call it like a function using (). From now on, because we are in a reactive
    #renderDT statment as specified via the {} above, we can call dataForDisplay
    #as normal i.e. because we are in a reactive context, the NEW variables we
    #create can be called like normal. BUT, they are NOT available outside of
    #this renderDT e.g. they are not global variables, unlike reactiveDF!
    dataForDisplay <- reactiveDF()
    
    #Any time reactiveDF() changes, update dataCount() reactive value with the
    #number of rows left in reactiveDF()
    dataCount(nrow(dataForDisplay))
  })
  
  #Now, we display a message with the data count in it! Note that we pass values
  #to pre-defined output spaces based on what was in our user interface. So, in
  #the UI call above, we created a verbatimTextOutput() with the ID of
  #dataCount. We "give" it reactive data by calling output$dataCount e.g. output
  #+ $ + ID of the output space
  output$dataCount <- renderText({
    paste0("There are ",dataCount()," entries in the data that match this criteria.")
  })
  
  #Let's start by creating a table for our data. By default, we will display ALL
  #data. We could change this if we want by updating the reactiveVal within the
  #render statement below. Note the use of the brackets {} below. This basically
  #specifies that all data within the {} should be considered reactive i.e. is
  #subject to update by the user. Again, we give the table to the pre-defined
  #output space defined in ui with output$tableOutput where tableOutput is the
  #id we prescribed it in the ui
  output$tableOutput <- renderDT({
    #We can call our reactiveVal to get its current value. Note that we have to
    #call it like a function using (). From now on, because we are in a reactive
    #renderDT statment as specified via the {} above, we can call dataForDisplay
    #as normal i.e. because we are in a reactive context, the NEW variables we
    #create can be called like normal. BUT, they are NOT available outside of
    #this renderDT e.g. they are not global variables, unlike reactiveDF!
    dataForDisplay <- reactiveDF()
    
    #We can do A LOT of cool things with DT::datatable. We can change the
    #formatting, limit how much data is shown on each page, have it display all
    #data, color rows based on the values of certain columns, and MUCH more. See
    #this reference for more info: https://rstudio.github.io/DT/
    DT::datatable(dataForDisplay,
                  #Don't worry too much about these options yet, but these let
                  #us create the buttons to export the table data easily. We
                  #could alternatively do this using a downloadButton and
                  #downloadHandler for more customization
                  extensions = 'Buttons', 
                  options = list(
                    #Dom basically configures what we want in the table and where:
                    #B = buttons
                    #f = filter
                    #r = processing
                    #t = table
                    #i = information
                    #p = pagination
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                  )
    )
  })
}

shinyApp(ui, server)