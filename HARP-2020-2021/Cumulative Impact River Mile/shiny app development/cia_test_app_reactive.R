library(shiny)
library(shinydashboard)
library(plotly)
library(hydrotools)
library(leaflet)
library(DT)
library(kableExtra)

#PS3_5990_6161
#OR2_7900_7740

#RUN THIS BEFORE CLICKLING "RUN APP"
site <- "http://deq2.bse.vt.edu/d.dh"
basepath <-'/var/www/R'
source(paste(basepath,'config.R',sep='/'))
source(paste(github_location,"/HARParchive/HARP-2020-2021/Cumulative Impact River Mile/CIA_maps.R",sep = '/'))




ui <- 
  fluidPage(
    dashboardPage(
      dashboardHeader(title="Cumulative Impact Analysis"),
      dashboardSidebar( 
        sidebarMenu(
          menuItem("Runid Comparison", tabName = "runidcompare", icon = icon('tint'))
        )
      ),
      
      dashboardBody( 
        fluidRow(
          tabItems(
            tabItem("runidcompare",
                    box(plotlyOutput("plot", height=680), width=6, height=700, background='blue'),
                    submitButton("Update Run Conditions"),
                    box(textInput("riv_seg",value = 'OR2_7900_7740',label='Segment ID'), width =3),
                    box(numericInput("runid1", value = 11,label='Runid #1'), width =3 ),
                    box(textInput("flow_metric", value = "7q10", label="Flow Metric"), width=3),
                    box(numericInput("runid2",value = 18,label='Runid #2'), width =3),
                    box(leafletOutput("map1"), height = "100%"),
                    box(DT :: dataTableOutput("table"), width = 10),
                    #box(imageOutput("map"))
                    
            )
          )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  mydata <- reactive({
    
    # input$action
    # isolate(input$runid1)
    #  isolate(input$runid2) 
    #  isolate(input$riv_seg)
    #  isolate(input$flow_metric)
  
  CIA_data <- function(riv_seg, runid1, runid2, flow_metric){
    
    
    
    #All Segs List
    AllSegList <- c('OR5_7980_8200', 'OR2_8020_8130', 'OR2_8070_8120', 'OR4_8120_7890',
                    'OR2_8130_7900', 'OR5_8200_8370', 'OR4_8271_8120', 'TU3_8480_8680',
                    'TU1_8570_8680', 'TU3_8650_8800', 'TU4_8680_8810', 'TU2_8790_9070',
                    'TU4_8800_9290', 'TU4_8810_9000', 'BS4_8540_8441', 'BS3_8580_8440',
                    'BS2_8590_8440', 'BS1_8730_8540', 'MN2_8250_8190', 'MN4_8260_8400',
                    'MN0_8300_0001', 'MN4_8400_8380', 'MN4_8510_8380', 'MN2_8530_8510',
                    'NR6_7820_7960', 'NR1_7880_8050', 'BS3_8350_8330', 'BS4_8440_8441',
                    'MN3_7540_7680', 'MN1_7590_7860', 'MN1_7620_7710', 'MN3_7680_7860',
                    'MN4_7710_8161', 'MN2_7720_7830', 'MN1_7730_8160', 'MN3_7770_7930',
                    'MN4_7810_8080', 'MN2_7830_7950', 'MN3_7860_8080', 'MN3_7930_8010',
                    'MN4_7950_7710', 'MN1_7990_8100', 'MN3_8010_7950', 'MN4_8080_8110',
                    'MN2_8100_8190', 'MN5_8161_8160', 'MN3_8190_8260', 'MN5_8230_8161',
                    'NR6_7960_8050', 'NR1_8030_8051', 'NR6_8050_8051', 'NR6_8051_8000',
                    'NR6_8170_7960', 'NR6_8180_8051', 'NR2_8210_8180', 'NR3_8290_8170',
                    'NR3_8420_8430', 'NR3_8430_7820', 'NR6_8640_8500', 'NR3_8690_8500',
                    'NR5_8700_8640', 'NR3_8740_8500', 'NR5_8760_8640', 'NR1_8820_8760',
                    'NR5_8870_8760', 'NR1_8960_8870', 'NR1_9030_9080', 'NR5_9050_8870',
                    'NR5_9080_9050', 'NR4_9130_9080', 'NR1_9150_9050', 'NR3_9170_9130',
                    'NR3_9190_9170', 'NR3_9240_9130', 'NR2_9250_9170', 'NR3_9310_9240',
                    'OD3_8340_8520', 'OD3_8520_8621', 'OD2_8560_8630', 'OD6_8621_8470',
                    'OD3_8630_8720', 'OD6_8660_8621', 'OD2_8670_8890', 'OD3_8710_8470',
                    'OD3_8720_8900', 'OD5_8770_8780', 'OD5_8780_8660', 'OD2_8830_8710',
                    'OD2_8840_9020', 'OD3_8850_8931', 'OD5_8890_8770', 'OD5_8900_8770',
                    'OD1_8910_8930', 'OD2_8920_8830', 'OD3_8930_8931', 'OD3_8931_9140',
                    'OD5_8940_8780', 'OD4_8990_8900', 'OD3_9020_9110', 'OD4_9110_9140',
                    'OD4_9140_8990', 'OD1_9270_9110', 'OR2_7610_7780', 'OR2_7650_8070',
                    'OR2_7670_7840', 'OR1_7700_7980', 'OR3_7740_8271', 'OR2_7780_7890',
                    'OR2_7840_7970', 'OR5_7890_7970', 'OR2_7900_7740', 'OR5_7910_8410',
                    'OR5_7970_8200', 'OR1_8280_8020', 'OR1_8320_8271', 'OR5_8370_8410',
                    'OR5_8410_8470', 'OR2_8450_8490', 'OR2_8460_8271', 'OR7_8470_8490',
                    'TU2_8860_9000', 'TU3_8880_9230', 'TU2_8950_9040', 'TU2_8970_9280',
                    'TU5_9000_9280', 'TU1_9010_9290', 'TU3_9040_9180', 'TU3_9060_9230',
                    'TU2_9070_9090', 'TU2_9100_9200', 'TU3_9180_9090', 'TU2_9200_9180',
                    'TU1_9220_9200', 'TU3_9230_9260', 'NR2_8600_8700', 'NR6_8500_7820',
                    'EL0_4560_4562','EL0_4561_4562','EL0_4562_0001','EL2_4400_4590',
                    'EL2_4590_0001','EL2_5110_5270','EL2_5270_0001','PM0_4640_4820',
                    'PM1_3120_3400','PM1_3450_3400','PM1_3510_4000','PM1_3710_4040',
                    'PM1_4000_4290','PM1_4250_4500','PM1_4430_4200','PM1_4500_4580',
                    'PM2_2860_3040','PM2_3400_3340','PM2_4860_4670','PM3_3040_3340',
                    'PM3_4660_4620','PM3_4670_4660','PM4_3340_3341','PM4_3341_4040',
                    'PM4_4040_4410','PM7_4150_4290','PM7_4200_4410','PM7_4290_4200',
                    'PM7_4410_4620','PM7_4580_4820','PM7_4620_4580','PM7_4820_0001',
                    'PS0_6150_6160','PS0_6160_6161','PS1_4790_4830','PS1_4830_5080',
                    'PS2_5550_5560','PS2_5560_5100','PS2_6420_6360','PS2_6490_6420',
                    'PS2_6660_6490','PS2_6730_6660','PS3_5100_5080','PS3_5990_6161',
                    'PS3_6161_6280','PS3_6280_6230','PS3_6460_6230','PS4_5080_4380',
                    'PS4_5840_5240','PS4_6230_6360','PS4_6360_5840','PS5_4370_4150',
                    'PS5_4380_4370','PS5_5200_4380','PS5_5240_5200','PU0_3000_3090',
                    'PU0_3601_3602','PU0_3611_3530','PU0_3751_3752','PU0_3871_3690',
                    'PU0_5620_5380','PU0_6080_5620','PU1_3030_3440','PU1_3100_3690',
                    'PU1_3170_3580','PU1_3580_3780','PU1_3850_4190','PU1_3940_3970',
                    'PU1_4190_4300','PU1_4300_4440','PU1_4840_4760',
                    'PU1_5380_5050','PU1_5520_5210','PU1_5820_5380','PU2_2790_3290',
                    'PU2_2840_3080','PU2_3080_3640','PU2_3090_4050','PU2_3140_3680',
                    'PU2_3180_3370','PU2_3370_4020','PU2_3630_3590','PU2_3770_3600',
                    'PU2_3900_3750','PU2_4050_4180','PU2_4160_3930','PU2_4220_3900',
                    'PU2_4340_3860','PU2_4360_4160','PU2_4720_4750','PU2_4730_4220',
                    'PU2_5190_4310','PU2_5700_5210','PU2_6050_5190',
                    'PU3_2510_3290','PU3_3290_3390','PU3_3390_3730','PU3_3680_3890',
                    'PU3_3860_3610','PU3_4280_3860','PU3_4450_4440','PU3_5210_5050',
                    'PU4_3780_3930','PU4_3890_3990','PU4_3970_3890','PU4_3990_3780',
                    'PU4_4210_4170','PU4_4310_4210','PU4_4440_3970','PU4_5050_4310',
                    'PU5_3930_4170','PU5_4170_4020','PU6_3440_3590','PU6_3530_3440',
                    'PU6_3590_3640','PU6_3600_3602','PU6_3602_3730','PU6_3610_3530',
                    'PU6_3640_3600','PU6_3690_3610','PU6_3730_3750','PU6_3750_3752',
                    'PU6_3752_4080','PU6_3870_3690','PU6_4020_3870','PU6_4080_4180',
                    'PU6_4180_4150','JA0_7291_7290','JA2_7290_0001','JA1_7600_7570',
                    'JA1_7640_7280','JA2_7410_7470','JA2_7550_7280','JA2_7570_7480',
                    'JA4_7280_7340','JA4_7340_7470','JA4_7470_7480','JA5_7480_0001',
                    'JB3_6820_7053','JB3_7053_0001','PL1_4460_4780','PL1_4780_0001',
                    'JL1_6560_6440','JL1_6760_6910','JL1_6770_6850','JL1_6910_6960',
                    'JL1_6940_7200','JL1_7080_7190','JL1_7170_6800','JL1_7190_7250',
                    'JL1_7200_7250','JL1_7530_7430','JL2_6240_6520','JL2_6440_6441',
                    'JL2_6441_6520','JL2_6850_6890','JL2_7110_7120','JL2_7120_6970',
                    'JL2_7240_7350','JL2_7250_7090','JL2_7350_7090','JL3_7020_7100',
                    'JL3_7090_7150','JL4_6520_6710','JL4_6710_6740','JL6_6740_7100',
                    'JL6_6890_6990','JL6_6960_6970','JL6_6970_6740','JL6_6990_6960',
                    'JL6_7150_6890','JL6_7160_7440','JL6_7320_7150','JL6_7430_7320',
                    'JL6_7440_7430','JL7_6800_7070','JL7_7030_6800','JL7_7070_0001',
                    'JL7_7100_7030','JU1_6290_6590','JU1_6300_6650','JU1_6340_6650',
                    'JU1_6590_6600','JU1_6880_7260','JU1_7560_7500','JU1_7630_7490',
                    'JU1_7690_7490','JU1_7750_7560','JU2_6410_6640','JU2_6600_6810',
                    'JU2_6810_6900','JU2_7140_7330','JU2_7180_7380','JU2_7360_7000',
                    'JU2_7450_7360','JU3_6380_6900','JU3_6640_6790','JU3_6650_7300',
                    'JU3_6790_7260','JU3_6900_6950','JU3_6950_7330','JU3_7400_7510',
                    'JU3_7490_7400','JU4_7000_7300','JU4_7260_7380','JU4_7330_7000',
                    'JU4_7380_7160','JU5_7300_7510','JU5_7420_7160','JU5_7500_7420',
                    'JU5_7510_7500','PL0_5141_5140','PL1_5370_5470','PL2_4970_5250',
                    'PL2_5140_5360','PL2_5470_5360','PL3_5250_0001','PL3_5360_5250',
                    'PL0_5010_5130','PL1_5130_0001','PL0_5490_0001','PL0_5540_5490',
                    'PL2_5300_5630','PL2_5630_0001','PL0_5730_5690','PL1_5690_0001',
                    'PL0_5530_5710','PL0_5710_0001','RU2_5220_5640','RU2_5500_5610',
                    'RU2_5810_5610','RU2_5940_6200','RU2_6090_6220','RU2_6200_6170',
                    'RU2_6220_6170','RU3_5610_5640','RU3_6170_6040','RU4_5640_6030',
                    'RU4_6040_6030','RU5_6030_0001','XU0_4090_4270','XU0_4091_4270',
                    'XU0_4130_4070','XU2_4070_4330','XU2_4270_4650','XU2_4330_4480',
                    'XU2_4480_4650','XU3_4650_0001','YM1_6370_6620','YM2_6120_6430',
                    'YM3_6430_6620','YM4_6620_0001','YP1_6570_6680','YP1_6680_6670',
                    'YP2_6390_6330','YP3_6330_6700','YP3_6470_6690','YP3_6670_6720',
                    'YP3_6690_6720','YP3_6700_6670','YP4_6720_6750','YP4_6750_0001',
                    'YP0_6840_0001','YP0_6860_6840',
                    'EL0_5400_0001','EL1_5150_0001',
                    'EL1_5430_0001','EL1_5570_0001','EL1_6000_0001','JB0_7051_0001',
                    'JB0_7052_0001','JB1_8090_0001','JB2_7800_0001','PL0_4510_0001',
                    'PL0_5000_0001','PL0_5070_0001','PL0_5510_0001','PL0_5720_0001',
                    'PL0_5750_0001','PL0_5830_0001','PL1_4540_0001','PL1_5230_0001',
                    'PL1_5910_0001','RL0_6540_0001','RL1_6180_0001','YL2_6580_0001',
                    'PU1_4760_4451','PU2_4750_4451','PU3_4451_4450','PM1_3711_3710',
                    'PM1_4251_4250','PM1_4252_4250')
    
    #Finding headwater and all downstring river segments
    upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
    upstream <- upstream[nrow(upstream):1,]
    upstream <- data.frame(upstream)
    names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
    
    upstream <- as.character(upstream[[1,1]])
    
    if(upstream == 'NA'){
      riv_seg <- riv_seg
    }else {
      riv_seg <- upstream
    }
    
    downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
    names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
    riv_seg <- as.data.frame(riv_seg)
    river <- rbind(riv_seg, downstream)
    
    #setting up dataframe for om_vahydro_metric_grid
    df <- data.frame(
      'model_version' = c('vahydro-1.0'),
      'runid' = c(paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2),
                  paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), '0.%20River%20Channel', 'local_channel'),
      'runlabel' = c('flow1', 'flow2','Qbaseline1','Qbaseline2', 'wd_mgd1','wd_mgd2','ps_mgd1','ps_mgd2', 'length', 'subcomp_da'),
      'metric' = c( flow_metric, flow_metric, 'Qbaseline','Qbaseline',
                    'wd_mgd', 'wd_mgd','ps_mgd', 'ps_mgd', 'length', 'drainage_area')
    )
    
    #importing dataframe of river segment metrics
    wshed_data <- om_vahydro_metric_grid(metric, df)
    
    #triming dataframe to just river segments on river of interest
    cia_data <- sqldf("SELECT * FROM river join wshed_data
                    WHERE riverseg like riv_seg")
    
    #Convert length column to miles
    cia_data$length <- cia_data$length/5280
    
    #pull the values that exist from length and subcomp_da (One of the two will be NA for each segment)
    da_data <- sqldf("SELECT pid, length, subcomp_da,
                    CASE
                    WHEN length is null then subcomp_da
                    ELSE length
                    END as da
                    from cia_data")
    
    #selecting the values that are positive
    cia_data$length <- da_data$da
    
    #Adding length segments together to form river mile column
    i <- 1
    while (i <= nrow(cia_data)) {
      
      river_length <- c()
      
      # Loop creates vector of current segment and upstream segment lengths
      for (n in 1:i) {
        n_length <- as.numeric(cia_data$length[n])
        river_length <- c(river_length, n_length)
      }
      # Makes length column to total length to segment from start of river
      cia_data$mile[i] <- sum(river_length)
      
      i <- i + 1
    }
    
    res <- list(cia_data)
    
  }
  

  cia_data <- CIA_data(riv_seg = input$riv_seg, runid1 = input$runid1, runid2 = input$runid2, flow_metric = input$flow_metric)
  
  })
  
  output$plot <- renderPlotly({
    
    cia_list <-  mydata()
    cia_df<- data.frame(cia_list)
    
    library(ggplot2)
    library(plotly)
    
       p<-ggplot(cia_df, aes(x = mile)) +
      geom_point(aes(y = flow1, colour = paste0('Runid',input$runid1), text=propname)) +
      geom_line(aes(y = flow1, colour = paste0('Runid',input$runid1))) +
      geom_point(aes(y = flow2, colour = paste0('Runid',input$runid2), text= propname)) +
      geom_line(aes(y = flow2, colour = paste0('Runid',input$runid2))) +
      labs(colour = 'Legend') +
      ggtitle(paste('Comparison of', (input$flow_metric), 'Flow')) +
      xlab('River Mile [mi]') +
      ylab(paste0('Flow [cfs]'))
    
    
    ggplotly(p, tooltip = c("text", "mile", "flow1", "flow2"))
    
    
  })
  
  
  output$table <- DT :: renderDataTable({

    cia_list <-  mydata()
    cia_df<- data.frame(cia_list)
    
    
    is.num <- sapply(cia_df, is.numeric)
    cia_df[is.num] <- lapply(cia_df[is.num], round, 3)
    
    cia_data_table <- data.frame(cia_df$propname, cia_df$riv_seg, cia_df$mile, cia_df$flow1, cia_df$flow2,
                                 cia_df$Qbaseline1, cia_df$Qbaseline2, cia_df$wd_mgd1, cia_df$wd_mgd2,
                                 cia_df$ps_mgd1, cia_df$ps_mgd2)
    
    datatable(cia_data_table, class="compact cell-border", colnames= c("Segment Name","Riverseg","River Mile","Q1","Q2","Qbaseline1",
                                                                       "Qbaseline2","wd_mgd1","wd_mgd2","ps_mgd1","ps_mgd2"), options = list("pageLength" = 25))
  })
  
  # output$map <- renderImage(
  #    
  #   cia_list <-  mydata(),
  #   cia_df <- data.frame(cia_list),
  #   
  #   export_path <- "C:/Users/nabra/Documents",
  #   
  #    map_layers <- load_MapLayers(site = "http://deq2.bse.vt.edu/d.dh"), #WARNING - DO NOT ATTEMPT TO OUTPUT map_layers DIRECTLY TO YOUR CONSOLE, ITS A LIST OF MANY LARGE MAPPING LAYERS
  #    cia_map <- CIA_maps(cia_data = cia_data, map_layers = map_layers),
  #    ggsave(paste0(export_path,riv_seg,"_cia_map.png",sep = ""), width=5.5, height=5)
  # 
  #   
  
  
 #test 


}
  
# Run the application
shinyApp(ui = ui, server = server)
