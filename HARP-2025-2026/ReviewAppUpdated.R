library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(gridExtra)
library(patchwork)
library(cowplot)

#call in functions from Event_summary_functions.R
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/ih_event_analysis/HARP-2025-2026/Event_summary_functions.R")

#load CS (or alternate site) data
df <- results$MJ$df
summary_df <- results$MJ$summary

#add the checklist columns
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
      downloadButton("download_plots", "Download Plots (JPG)"),
      br(), br(),
      downloadButton("download_event_plots", "Download Event Plots + Summary (JPG)")
    ),
    mainPanel(
      #original two plots
      plotlyOutput("flow_plot"),
      plotlyOutput("agwr_plot"),
      
      br(), hr(), br(),
      
      #new section below original plots
      h3("Detailed Event Analysis"),
      fluidRow(
        column(4,
               h4("Event Summary"),
               htmlOutput("event_text")   # Replaces reactable
        ),
        column(8,
               plotOutput("event_plots", height = "900px")
        )
      )
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
    if (length(i) == 1) current(i)
  })
  
  observeEvent(input$save, {
    i <- current()
    checklist_vals <- if (is.null(input$checklist)) character(0) else input$checklist
    summary_df$review_checklist[[i]] <<- checklist_vals
    summary_df$overall_review[i] <<- input$overall
    message(paste("Saved review for group", i))
  })
  
  get_event_data <- reactive({
    i <- current()
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
    
    list(data = filtered_df, start_date = start_date, end_date = end_date)
  })
  
  #original plots (unchanged)
  flow_ggplot <- reactive({
    event <- get_event_data()
    data <- event$data
    start_date <- event$start_date
    
    data <- data %>%
      mutate(threshold_flag = ifelse(AGWR_flag == "In Threshold", "In Threshold", "Out of Threshold"))
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Flow), color = "black") +
      geom_point(aes(y = Flow, color = threshold_flag), size = 2) +
      geom_vline(xintercept = as.numeric(start_date), linetype = "dotted", color = "blue") +
      scale_color_manual(
        name = "Flow Point Status",
        values = c("In Threshold" = "forestgreen", "Out of Threshold" = "red")
      ) +
      labs(
        title = paste("Flow during Recession Event", summary_df$GroupID[current()]),
        y = "Flow (CFS)", x = "Date"
      ) +
      ylim(0, NA) +
      theme_minimal()
  })
  
  agwr_ggplot <- reactive({
    event <- get_event_data()
    data <- event$data
    start_date <- event$start_date
    
    data <- data %>%
      mutate(
        AGWR_flag = ifelse(AGWR < 1.0, "AGWR In", "AGWR Out"),
        delta_flag = ifelse(delta_AGWR >= 0.96 & delta_AGWR <= 1.04,
                            "dAGWR In", "dAGWR Out"),
        group_label = case_when(
          !is.na(AGWR) ~ AGWR_flag,
          !is.na(delta_AGWR) ~ delta_flag
        )
      )
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = AGWR), color = "blue", linetype = "dashed") +
      geom_point(aes(y = AGWR, shape = AGWR_flag, color = AGWR_flag), size = 2, stroke = 1) +
      geom_line(aes(y = delta_AGWR), color = "orange", linetype = "dotted") +
      geom_point(aes(y = delta_AGWR, shape = delta_flag, color = delta_flag), size = 2, stroke = 1) +
      geom_hline(yintercept = 1.0, linetype = "solid", color = "black") +
      geom_hline(yintercept = c(0.96, 1.04), linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = as.numeric(start_date), linetype = "dotted", color = "blue") +
      scale_color_manual(
        name = "Threshold Status",
        values = c(
          "AGWR In" = "blue",
          "AGWR Out" = "blue",
          "dAGWR In" = "orange",
          "dAGWR Out" = "orange"
        )
      ) +
      scale_shape_manual(
        name = "Threshold Status",
        values = c(
          "AGWR In" = 16,
          "AGWR Out" = 1,
          "dAGWR In" = 15,
          "dAGWR Out" = 0
        )
      ) +
      labs(
        title = paste("AGWR + delta_AGWR – Event", summary_df$GroupID[current()]),
        y = "AGWR / delta_AGWR", x = "Date"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  output$flow_plot <- renderPlotly({ ggplotly(flow_ggplot()) })
  output$agwr_plot <- renderPlotly({ ggplotly(agwr_ggplot()) })
  
  #simple text summary (fixed robustly, was running into a lot of issues here)
  output$event_text <- renderUI({
    i <- current()
    ev <- summarize.event(analysis_MJ, summary_df$GroupID[i])
    
    if (is.null(ev)) {
      return(HTML("<p><em>No summary available for this event</em></p>"))
    }
    
    agwr_val <- ev$AGWR
    r2_val   <- ev$R2
    
    start <- summary_df$StartDate[i]
    end   <- summary_df$EndDate[i]
    duration <- as.numeric(difftime(end, start, units = "days")) + 1
    
    HTML(paste0(
      "<b>Start Date:</b> ", start, "<br/>",
      "<b>End Date:</b> ", end, "<br/>",
      "<b>Duration:</b> ", duration, " days<br/>",
      "<b>Calculated AGWR:</b> ", ifelse(!is.na(agwr_val), sprintf("%.3f", agwr_val), "Unavailable"), "<br/>",
      "<b>R-Squared:</b> ", ifelse(!is.na(r2_val), sprintf("%.3f", r2_val), "Unavailable")
    ))
  })
  
  #event plots stacked
  output$event_plots <- renderPlot({
    gid <- summary_df$GroupID[current()]
    ev <- tryCatch(summarize.event(analysis_MJ, gid), error = function(e) NULL)
    
    if (!is.null(ev)) {
      suppressWarnings(
        plot.event.values(analysis_MJ, gid)
      )
    }
  })
  
  ##NEW PLOTS (3) DOWNLOAD HANDLER##
  #had to make changes to make sure it handled it as a jpeg and not htm
  output$download_event_plots <- downloadHandler(
    filename = function() {
      paste0("Event_", summary_df$GroupID[current()], "_detailed_plots.jpg")
    },
    content = function(file) {
      gid <- summary_df$GroupID[current()]
      ev <- summarize.event(analysis_MJ, gid)
      
      #create the three stacked plots
      plots <- suppressWarnings(plot.event.values(analysis_MJ, gid))
      
      #build the summary text
      start <- summary_df$StartDate[current()]
      end   <- summary_df$EndDate[current()]
      duration <- as.numeric(difftime(end, start, units = "days")) + 1
      agwr_val <- ifelse(!is.null(ev$AGWR) && !is.na(ev$AGWR), sprintf("%.3f", ev$AGWR), "Unavailable")
      r2_val   <- ifelse(!is.null(ev$R2) && !is.na(ev$R2), sprintf("%.3f", ev$R2), "Unavailable")
      
      summary_text <- paste0(
        "Event ", gid, "\n",
        "Start Date: ", start, "\n",
        "End Date: ", end, "\n",
        "Duration: ", duration, " days\n",
        "Calculated AGWR: ", agwr_val, "\n",
        "R-Squared: ", r2_val
      )
      
      #open JPEG device
      jpeg(file, width = 1600, height = 2000, res = 150)
      gridExtra::grid.arrange(
        plots,
        bottom = grid::textGrob(summary_text, gp = grid::gpar(fontsize = 12))
      )
      dev.off()
    }
  )
  
  ##ORIGINAL PLOTS DOWNLOAD (relocated)##
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("Event_", summary_df$GroupID[current()], "_plots.jpg")
    },
    content = function(file) {
      #unified legend setup
      g1 <- flow_ggplot() + labs(color = "Flow Point Status")
      g2 <- agwr_ggplot() + labs(color = "Threshold Status", shape = "Threshold Status")
      #align both plots
      g1 <- g1 + theme(legend.position = "right")
      g2 <- g2 + theme(legend.position = "right")
      
      combined <- plot_grid(g1, g2, ncol = 1, rel_heights = c(1, 1.1))
      
      jpeg(file, width = 1400, height = 1600, res = 150)
      grid::grid.draw(combined)
      dev.off()
    }
  )
}

shinyApp(ui, server)
