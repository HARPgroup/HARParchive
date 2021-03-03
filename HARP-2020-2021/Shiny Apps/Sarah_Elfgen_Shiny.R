
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(elfgen)
library(sqldf)


header <- dashboardHeader(title = "Elfgen Algorithm")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Elfgen", tabName = 'elfgen', icon = icon('atlas'))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'elfgen',
            h2("Elfgen Content"),
            box(textInput("hydroid",value = 58905,label='HydroID')),
            box(textInput("huc_level",value = 'huc8',label='HUC Level')),
            box(textInput("flow_metric",value = 'erom_q0001e_mean',label='Flow Metric')),
            box(textInput("flow_reduction_pct", value = 10, label = 'Flow Reduction Percentage')),
            box(plotOutput("plot1"), width=8, height=400),
            box(textOutput('pctrange')),
            box(textOutput('absrange'))
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {

  output$plot1 <- renderPlot({
    
    site <- "http://deq2.bse.vt.edu/d.dh"
    basepath = '/var/www/R';
    source(paste(basepath,'config.R',sep='/'))
    
    hydroid <- input$hydroid
    huc_level <- input$huc_level
    flow_metric <- input$flow_metric
    
    site_comparison <- paste('http://deq1.bse.vt.edu/d.dh/dh-feature-contained-within-export', hydroid, 'watershed', sep = '/')
    
    containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")
    
    nhd_code <- sqldf(paste("SELECT hydrocode 
             FROM containing_watersheds 
             WHERE ftype = 'nhd_", huc_level,"'", sep = ""))
    
    hydroid2 <- sqldf("SELECT hydroid 
                  FROM containing_watersheds 
                  WHERE ftype 
                  LIKE '%nhdplus%'")
    
    inputs <- list(
      varkey = flow_metric,
      featureid = as.numeric(hydroid2$hydroid),
      entity_type = "dh_feature"
    )
    
    #property dataframe returned
    dataframe <- getProperty(inputs, site)
    
    mean_intake <- dataframe$propvalue
    
    # Example input parameters for retrieving data from VAHydro
    watershed.code <- as.character(nhd_code)
    watershed.bundle <- 'watershed'
    watershed.ftype <- paste("nhd_", huc_level, sep = "")
    x.metric <- flow_metric
    y.metric <- 'aqbio_nt_total'
    y.sampres <- 'species'
    
    # elfdata_vahydro() function for retrieving data from VAHydro
    watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,site)
    # clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
    watershed.df <- clean_vahydro(watershed.df)
    
    elf_quantile <- 0.80
    breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = elf_quantile, "blo" = 100, "bhi" = 1000)  
    
    elf <- elfgen("watershed.df" = watershed.df,
                  "quantile" = elf_quantile,
                  "breakpt" = breakpt,
                  "yaxis_thresh" = 53, 
                  "xlabel" = flow_metric,
                  "ylabel" = "Fish Species Richness")
    
    uq <- elf$plot$plot_env$upper.quant
    
    upper.lm <- lm(y_var ~ log(x_var), data = uq)
    
    predict.df <- as.data.frame(predict(upper.lm, newdata = data.frame(x_var = mean_intake), interval = 'confidence'))
    
    xmin <- min(uq$x_var)
    xmax <- max(uq$x_var)
    
    yval1 <- predict(upper.lm, newdata = data.frame(x_var = xmin), interval = 'confidence')
    yval2 <- predict(upper.lm, newdata = data.frame(x_var = xmax), interval = 'confidence')
    
    ymin1 <- yval1[2] # bottom left point, line 1
    ymax1 <- yval2[3] # top right point, line 1
    
    ymin2 <- yval1[3] # top left point, line 2
    ymax2 <- yval2[2] # bottom right point, line 2
    
    m <- elf$stats$m
    b <- elf$stats$b
    int <- m*log(mean_intake) + b      # solving for mean_intake y-value
    
    m1 <- (ymax1-ymin1)/(log(xmax)-log(xmin)) # slope and intercept of confidence interval line 1
    b1 <- ymax1-(m1*log(xmax))
    
    m2 <- (ymax2-ymin2)/(log(xmax)-log(xmin)) # slope and intercept of confidence interval line 2
    b2 <- ymax2 - (m2*log(xmax))
    
    p1 <- elf$plot +
      geom_segment(aes(x = mean_intake, y = -Inf, xend = mean_intake, yend = int), color = 'red', linetype = 'dashed') +
      geom_segment(aes(x = 0, xend = mean_intake, y = int, yend = int), color = 'red', linetype = 'dashed') +
      geom_point(aes(x = mean_intake, y = int, fill = 'Mean intake flow'), color = 'red', shape = 'triangle', size = 2) +
      geom_segment(aes(x = xmin, y = (m1 * log(xmin) + b1), xend = xmax, yend = (m1 * log(xmax)) + b1), color = 'blue', linetype = 'dashed') +
      geom_segment(aes(x = xmin, y = (m2 * log(xmin) + b2), xend = xmax, yend = (m2 * log(xmax)) + b2), color = 'blue', linetype = 'dashed') +
      labs(fill = 'Intake Legend')

    plot(p1)
  })

  dat <- reactive({
    
    elf$stats$m <- m1
    elf$stats$b <- b1
    
    pct_richness_change_1 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct, "xval" = mean_intake)
    abs_richness_change_1 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct)
    
    elf$stats$m <- m2
    elf$stats$b <- b2
    
    pct_richness_change_2 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct, "xval" = mean_intake)
    abs_richness_change_2 <- richness_change(elf$stats, "pctchg" = input$flow_reduction_pct)
    
    # dat <- as.numeric(c(pct_richness_change_1, pct_richness_change_2, abs_richness_change_1, abs_richness_change_2))
    # return(dat)
  })
  
  output$pctrange <- renderText({
    paste('Percent Richness Change at intake ranges from ',
          round(pct_richness_change_2, digits = 5), 'to ', 
          round(pct_richness_change_1, digits = 5), 'percent.')
  })
  
  output$absrange <- renderText({
    paste('Absolute Richness Change at intake ranges from ',
          round(abs_richness_change_2, digits = 5), 'to ', 
          round(abs_richness_change_1, digits = 5), 'taxa.')
  })
  
}

shinyApp(ui, server)

