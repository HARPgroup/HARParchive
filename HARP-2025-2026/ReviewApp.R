library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

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
      selectizeInput("jump_to", "Jump to Event (type or select):", 
                     choices = summary_df$GroupID, 
                     selected = summary_df$GroupID[1], 
                     options = list(placeholder = 'Type or select GroupID...', create = FALSE)),
      numericInput("buffer_days", "Days of Antecedent Buffer:", value = 5, min = 0, max = 30, step = 1),
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
      actionButton("save", "Save Review"),
      br(), br(),
      tags$div(
        tags$h5("AGWR/dAGWR Legend:"),
        tags$ul(
          tags$li(tags$span(style = "color:blue; font-weight:bold;", "\u25CF"), " AGWR In Threshold"),
          tags$li(tags$span(style = "color:blue;", "\u25CB"), " AGWR Out of Threshold"),
          tags$li(tags$span(style = "color:orange; font-weight:bold;", "\u25A0"), " dAGWR In Threshold"),
          tags$li(tags$span(style = "color:orange;", "\u25A1"), " dAGWR Out of Threshold")
        ),
        style = "margin-top:20px;"
      )
    ),
    mainPanel(
      plotlyOutput("flow_plot"),
      plotlyOutput("agwr_plot")
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
  
  #sync dropdown when Previous/Next are used
  observe({
    updateSelectizeInput(session, "jump_to", selected = summary_df$GroupID[current()])
  })
  
  #jump to event by GroupID
  observeEvent(input$jump_to, {
    i <- which(summary_df$GroupID == input$jump_to)
    if (length(i) == 1) {
      current(i)
    }
  })
  
  observeEvent(input$save, {
    i <- current()
    checklist_vals <- if (is.null(input$checklist)) character(0) else input$checklist #handles null
    summary_df$review_checklist[[i]] <<- checklist_vals
    summary_df$overall_review[i] <<- input$overall
    message(paste("Saved review for group", i))
  })
  
  get_event_data <- reactive({
    i <- current()
    gid <- summary_df$GroupID[i]
    
    start_date <- summary_df$StartDate[i]
    end_date <- summary_df$EndDate[i]
    
    buffer_start <- start_date - input$buffer_days
    buffer_end <- end_date
    
    filtered_df <- df %>% 
      filter(Date >= buffer_start & Date <= buffer_end) %>%
      mutate(
        AGWR_flag = case_when(
          AGWR < 1.0 & delta_AGWR >= 0.96 & delta_AGWR <= 1.04 ~ "In Threshold",
          TRUE ~ "Out of Threshold"
        )
      )
    
    list(data = filtered_df, start_date = start_date)
  })
  
  output$flow_plot <- renderPlotly({
    event <- get_event_data()
    data <- event$data
    start_date <- event$start_date
    
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Flow), color = "black") +
      geom_point(aes(y = Flow), color = "red") +
      geom_point(data = data %>% filter(AGWR_flag == "In Threshold"),
                 aes(y = Flow),
                 color = "forestgreen",
                 size = 2,
                 shape = 18) +  # Triangle shape for emphasis
      geom_vline(xintercept = as.numeric(start_date), linetype = "dotted", color = "blue") +
      labs(
        title = paste("Flow during Recession Event", summary_df$GroupID[current()]),
        y = "Flow (CFS)", x = "Date"
      ) +
      ylim(0, NA) +
      theme_minimal()
  })
  
  output$agwr_plot <- renderPlotly({
    event <- get_event_data()
    data <- event$data
    start_date <- event$start_date
    
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
    
    p <- ggplot(data, aes(x = Date)) +
      geom_line(aes(y = AGWR), color = "blue", linetype = "dashed") +
      geom_point(aes(y = AGWR, shape = factor(AGWR_shape)), color = "blue", size = 2, stroke = 1) +
      geom_line(aes(y = delta_AGWR), color = "orange", linetype = "dotted") +
      geom_point(aes(y = delta_AGWR, shape = factor(delta_shape)), color = "orange", size = 2, stroke = 1) +
      geom_hline(yintercept = 1.0, linetype = "solid", color = "black") +
      geom_hline(yintercept = c(0.96, 1.04), linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = as.numeric(start_date), linetype = "dotted", color = "blue") +
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
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p) %>%
      layout(
        legend = list(
          orientation = "h",    #horizontal layout
          x = 0,                #left aligned
          y = -0.2,             #move legend below the plot
          xanchor = "left",
          yanchor = "top"
        ),
        margin = list(b = 100)  #add bottom margin to prevent overlap
      )
  })
}

shinyApp(ui, server)

# write.csv(summary_df, "summary_df_export.csv", row.names = FALSE)
# 
# summary_df_simple <- summary_df[, !sapply(summary_df, is.list)]
# write.csv(summary_df_simple, "summary_df_export.csv", row.names = FALSE)
