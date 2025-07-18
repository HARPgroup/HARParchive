library(shiny)
library(ggplot2)
library(dplyr)

#load your CS data- replace this with desired loaded object (if not CS, then MJ or S)
df <- results$CS$df
summary_df <- results$CS$summary

#add checklist columns
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
    
    start_date <- summary_df$StartDate[i]
    end_date <- summary_df$EndDate[i]
    
    buffer_start <- start_date - 5
    buffer_end <- end_date
    
    df %>% 
      filter(Date >= buffer_start & Date <= buffer_end) %>%
      mutate(
        AGWR_flag = case_when(
          AGWR < 1.0 & delta_AGWR >= 0.96 & delta_AGWR <= 1.04 ~ "In Threshold",
          TRUE ~ "Out of Threshold"
        )
      )
  })
  
  output$flow_plot <- renderPlot({
    data <- get_event_data()
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Flow), color = "black") +
      geom_point(data = data, aes(x = Date, y = Flow), color = "red") +
      labs(title = paste("Flow during Recession Event", summary_df$GroupID[current()]),
           y = "Flow (CFS)", x = "") +
      ylim(0, NA) +  #adds 0 as the minimum y-axis
      theme_minimal() +
      scale_x_date(date_labels = "%b %d, %Y")
  })
  
  output$agwr_plot <- renderPlot({
    data <- get_event_data()
    if (nrow(data) == 0) return(NULL)
    
    #threshold definitions
    agwr_condition <- data$AGWR < 1.0
    delta_condition <- data$delta_AGWR >= 0.96 & data$delta_AGWR <= 1.04
    
    #classify flags for shape mapping
    data <- data %>%
      mutate(
        AGWR_flag = ifelse(agwr_condition, "In", "Out"),
        delta_flag = ifelse(delta_condition, "In", "Out"),
        AGWR_shape = ifelse(AGWR_flag == "In", 16, 1),       #circle: filled vs open
        delta_shape = ifelse(delta_flag == "In", 15, 0)      #square: filled vs open
      )
    
    #count values for display
    agwr_counts <- table(data$AGWR_flag)
    delta_counts <- table(data$delta_flag)
    agwr_in <- agwr_counts["In"] %||% 0
    agwr_out <- agwr_counts["Out"] %||% 0
    delta_in <- delta_counts["In"] %||% 0
    delta_out <- delta_counts["Out"] %||% 0
    
    ggplot(data, aes(x = Date)) +
      #AGWR line and points
      geom_line(aes(y = AGWR), color = "blue", linetype = "dashed") +
      geom_point(aes(y = AGWR, shape = factor(AGWR_shape)), color = "blue", size = 2, stroke = 1) +
      
      #delta_AGWR line and points
      geom_line(aes(y = delta_AGWR), color = "orange", linetype = "dotted") +
      geom_point(aes(y = delta_AGWR, shape = factor(delta_shape)), color = "orange", size = 2, stroke = 1) +
      
      #reference lines
      geom_hline(yintercept = 1.0, linetype = "solid", color = "black") +
      geom_hline(yintercept = c(0.97, 1.03), linetype = "dashed", color = "gray50") +
      
      #shape legend
      scale_shape_manual(
        name = "Threshold Status",
        values = c("16" = 16, "1" = 1, "15" = 15, "0" = 0),
        labels = c(
          "16" = "AGWR In Threshold",
          "1"  = "AGWR Out of Threshold",
          "15" = "dAGWR In Threshold",
          "0"  = "dAGWR Out of Threshold"
        )
      ) +
      
      labs(
        title = paste("AGWR + delta_AGWR – Event", summary_df$GroupID[current()]),
        subtitle = paste0(
          "AGWR: ", agwr_in, " in | ", agwr_out, " out   |   ",
          "dAGWR: ", delta_in, " in | ", delta_out, " out"
        ),
        y = "AGWR / delta_AGWR",
        x = "Date"
      ) +
      scale_x_date(date_labels = "%b %d, %Y") +
      theme_minimal() +
      theme(legend.position = "right")
  })
}

shinyApp(ui, server)

write.csv(summary_df, "summary_df_export.csv", row.names = FALSE)

summary_df_simple <- summary_df[, !sapply(summary_df, is.list)]
write.csv(summary_df_simple, "summary_df_export.csv", row.names = FALSE)
