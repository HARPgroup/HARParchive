library(shiny)
library(ggplot2)
library(dplyr)

# Load your CS data – replace this with your actual loaded object
df <- results$CS$df
summary_df <- results$CS$summary

# Add checklist columns if not present
if (!"review_checklist" %in% colnames(summary_df)) {
  summary_df$review_checklist <- replicate(nrow(summary_df), list())
}
if (!"overall_review" %in% colnames(summary_df)) {
  summary_df$overall_review <- rep(NA, nrow(summary_df))
}

ui <- fluidPage(
  titlePanel("Interactive Recession Review"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("prev", "Previous Event"),
      actionButton("next_btn", "Next Event"),
      br(), br(),
      checkboxGroupInput("checklist", "Checklist (mark all that apply):",
                         choices = c(
                           "AGWR consistently < 1.0",
                           "delta_AGWR near 1.0",
                           "Flow declines smoothly",
                           "AGWR and delta_AGWR converge",
                           "Event duration ≥ 14 days",
                           "No disturbance/storm influence"
                         )),
      radioButtons("overall", "Overall Review:",
                   choices = c("Looks Good", "Does Not Look Good")),
      actionButton("save", "Save Review")
    ),
    
    mainPanel(
      plotOutput("flow_plot"),
      plotOutput("agwr_plot")
    )
  )
)

server <- function(input, output, session) {
  current <- reactiveVal(1)
  
  observeEvent(input$next_btn, {
    if (current() < nrow(summary_df)) current(current() + 1)
  })
  
  observeEvent(input$prev, {
    if (current() > 1) current(current() - 1)
  })
  
  observeEvent(input$save, {
    i <- current()
    summary_df$review_checklist[[i]] <<- input$checklist
    summary_df$overall_review[i] <<- input$overall
    message(paste("Saved review for group", i))
  })
  
  get_event_data <- reactive({
    i <- current()
    gid <- summary_df$GroupID[i]
    df %>% filter(GroupID == gid)
  })
  
  output$flow_plot <- renderPlot({
    data <- get_event_data()
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Flow), color = "black") +
      geom_point(data = data, aes(x = Date, y = Flow), color = "red") +
      labs(title = paste("Flow during Recession Event", summary_df$GroupID[current()]),
           y = "Flow (CFS)", x = "") +
      theme_minimal()
  })
  
  output$agwr_plot <- renderPlot({
    data <- get_event_data()
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = AGWR), color = "blue", linetype = "dashed") +
      geom_line(aes(y = delta_AGWR), color = "orange", linetype = "dotted") +
      labs(title = paste("AGWR and delta_AGWR for Event", summary_df$GroupID[current()]),
           y = "AGWR / delta_AGWR", x = "") +
      theme_minimal()
  })
}

shinyApp(ui, server)

write.csv(summary_df, "summary_df_export.csv", row.names = FALSE)

summary_df_simple <- summary_df[, !sapply(summary_df, is.list)]
write.csv(summary_df_simple, "summary_df_export.csv", row.names = FALSE)
