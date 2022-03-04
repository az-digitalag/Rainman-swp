#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(ggplot2)
library(ggthemes)
library(cowplot)

# First tab: Seasonal timeseries by treatment
load("soilTempDaily.Rdata")
load("soilWaterContentDaily.Rdata")
load("airTempDaily.Rdata")
load("comboDaily.Rdata") # combo, now includes column for precip
load("season.Rdata")

# Second tab: Compare SWC and SWP by plot
load("soilWPWCDaily.Rdata")

# initialize global variable to record clicked rows
selected_points <- WPWC_daily[0,]

# Define UI for application with three tabs
ui <- navbarPage("RainManSR",
  tabPanel("Daily time series",
           titlePanel("Explore by treatment and season"),
           # Sidebar with drop down input for treatments, times, and date ranges
           sidebarLayout(
             sidebarPanel(
               # Select Summer treatment
               selectInput(inputId = "Summer",
                           label = "Select summer treatment",
                           choices = levels(Ts_daily$Summer)),
               # # Select Winter treatment
               # selectInput(inputId = "Winter",
               #             label = "Select winter treatment",
               #             choices = unique(Ts_daily$Winter)),
               # Select Year
               selectInput(inputId = "Year",
                           label = "Select hydrological year", 
                           choices = unique(season$Year)), 
               # Select Season
               uiOutput("dyn_season"),
               # Select range of dates
               uiOutput("dyn_slider")
             ),
             # Show a size plot for selected species
             mainPanel(
               fluidRow(plotOutput("seasonal_ts", width = "100%", height = "800px"))
             )
           )),
  tabPanel("Compare treatments",
           titlePanel("SWC by treatment type and year"),
           # Sidebar with drop down input for treatments, times, and date ranges
           sidebarLayout(
             sidebarPanel(
               # Select treatment
               selectInput(inputId = "Treatment",
                           label = "Select treatment type",
                           choices = c("Summer", "Winter")),
               # Select Year
               selectInput(inputId = "Year1",
                           label = "Select hydrological year", 
                           choices = unique(season$Year)),
               # Select range of dates
               uiOutput("dyn_slider1")
             ),
             # Show a size plot for selected species
             mainPanel(
               fluidRow(plotOutput("treatment_ts", width = "100%", height = "800px"))
             )
           )),
  tabPanel("Compare SWP and SWC",
           titlePanel("Relationships by treatment and year"),
           sidebarLayout(
             sidebarPanel(
               # Select Plot from House 3
               selectInput(inputId = "Summer2",
                           label = "Select summer treatment",
                           choices = unique(WPWC_daily$Summer)),
               # Select Year
               selectInput(inputId = "Year2",
                           label = "Select hydrological year", 
                           choices = unique(season$Year)), 
               # Select range of dates
               uiOutput("dyn_slider2")
             ),
             
             # Show a size plot for selected species
             mainPanel(
               h5('Click on point to obtain values. '),
               fluidRow(plotOutput("WPWC_scatter", 
                                   width = "100%", 
                                   height = "300px",
                                   click = clickOpts("plot_click"))),
               uiOutput("click_info"),
               fluidRow(plotOutput("WPWC_ts", width = "100%", height = "500px"))
             )
           ))
)


server <- function(input, output) {
  
  # Render a UI for selecting Season
  output$dyn_season <- renderUI({
    temp <- season %>%
      filter(Year == input$Year) %>%
      pull(Season)
    
    selectInput(inputId = "Season",
                label = "Select season", 
                choices = temp)
  })
  
  # Render a UI for selecting date range, landing page
  output$dyn_slider <- renderUI({
    temp <- season %>%
      filter(Year == input$Year,
             Season == input$Season)
    sliderInput(inputId = "date_range_selector", 
                label = "Select Date Range", 
                min = temp$st,
                max = temp$en,
                value = c(temp$st,
                          temp$en))
  })
  

  # Render a UI for selecting date range, tab2
  output$dyn_slider1 <- renderUI({
    temp <- season %>%
      filter(Year == input$Year1)
    sliderInput(inputId = "date_range_selector1", 
                label = "Select Date Range", 
                min = min(temp$st),
                max = max(temp$en),
                value = c(min(temp$st),
                          max(temp$en)))
  })

  
  # Render a UI for selecting date range tab 3
  output$dyn_slider2 <- renderUI({
    temp <- season %>%
      filter(Year == input$Year2)
    sliderInput(inputId = "date_range_selector2", 
                label = "Select Date Range", 
                min = min(temp$st),
                max = max(temp$en),
                value = c(min(temp$st),
                          max(temp$en)))
  })
  
  # Function for plotting the first tab
  # Daily timeseries of all variables by treatment and season
  output$seasonal_ts <- renderPlot({
    # Soil Temp
    temp_soilT <- Ts_daily %>%
      filter(Summer == input$Summer, # input$Summer
             # date >= as.Date("2020-06-01"),
             # date <= as.Date("2020-09-30")) %>%
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2]) %>%
      group_by(Summer, date, Depth) %>%
      summarize(mean_Ts_mean = mean(Ts_mean, na.rm = TRUE),
                mean_Ts_min = mean(Ts_min,  na.rm = TRUE),
                mean_Ts_max = mean(Ts_max,  na.rm = TRUE))
    
    temp_irig <- combo %>%
      filter(Summer == input$Summer, # input$Summer
             # date >= as.Date("2020-07-01"),
             # date <= as.Date("2020-09-30")) %>%
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2]) %>%
      mutate(ppt_mm = ifelse(is.na(ppt_mm), 0, ppt_mm),
             irrigation_mm = ifelse(is.na(irrigation_mm), 0, irrigation_mm))
    
    # For tuning the relative sizes of left and right y axes
    ratio1 <- if(nrow(temp_irig) == 0) { 
      1 } else {max(max(temp_irig$irrigation_mm,  na.rm = TRUE), 
                    max(temp_irig$ppt_mm,  na.rm = TRUE),
                    na.rm = TRUE) / 
          max(temp_soilT$mean_Ts_max, na.rm = TRUE)}
    
    # For controlling whether/how many watering inputs are present
    color_ind <- if(nrow(temp_irig) == 0) { 0 } else { 2 }
    # For controlling fill when no irrigation or precip present
    fill_ind <- if(nrow(temp_irig) == 0) { 0 } else { 1 }
    
    fig_a <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm/ratio1,
                   fill = "Irrigation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_bar(data = temp_irig,
               aes(x = date, y = ppt_mm/ratio1,
                   fill = "Precipitation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_line(data = temp_soilT,
                aes(x = date, 
                    y = mean_Ts_mean,
                    color = as.factor(Depth)),
                lty = 2) +
      geom_errorbar(data = temp_soilT,
                    aes(x = date,
                        ymin = mean_Ts_min,
                        ymax = mean_Ts_max,
                        color = as.factor(Depth)),
                    width = 0,
                    alpha = 0.3) +
      geom_point(data = temp_soilT,
                 aes(x = date,
                     y = mean_Ts_mean,
                     color = as.factor(Depth)),
                 size = 3) +
      scale_y_continuous(expression(paste(T[soil], " (", degree, "C)")),
                         limits = c(0, NA),
                         sec.axis = sec_axis(~.*ratio1,
                                             name = "Watering (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 week") +
      theme_bw(base_size = 16) +
      scale_fill_manual(values = c("lightblue", "gray")[1:color_ind]) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm")[unique(temp_soilT$Depth)], 
                         values = c("#8C510A", "#BF812D", "#DFC27D")[unique(temp_soilT$Depth)]) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank()) +
      guides(color = guide_legend(
        override.aes = list(linetype = c(0, 0, 0)[unique(temp_soilT$Depth)])))
    
    #  Soil water content
    temp_WC <- WC_daily %>%
      filter(Summer == input$Summer,
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2]) %>%
      group_by(Summer, date, Depth) %>%
      summarize(mean_WC_mean = mean(WC_mean, na.rm = TRUE),
                mean_WC_min = mean(WC_min,  na.rm = TRUE),
                mean_WC_max = mean(WC_max,  na.rm = TRUE))
    
    ratio2 <- if(nrow(temp_irig) == 0) { 
      1 } else {max(max(temp_irig$irrigation_mm,  na.rm = TRUE), 
                    max(temp_irig$ppt_mm,  na.rm = TRUE),
                    na.rm = TRUE) / 
          max(temp_WC$mean_WC_max, na.rm = TRUE)}
    
    fig_b <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm/ratio2,
                   fill = "Irrigation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_bar(data = temp_irig,
               aes(x = date, y = ppt_mm/ratio2,
                   fill = "Precipitation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_line(data = temp_WC,
                aes(x = date, 
                    y = mean_WC_mean,
                    color = as.factor(Depth)),
                lty = 2) +
      geom_errorbar(data = temp_WC,
                    aes(x = date,
                        ymin = mean_WC_min,
                        ymax = mean_WC_max,
                        color = as.factor(Depth)),
                    width = 0,
                    alpha = 0.3) +
      geom_point(data = temp_WC,
                 aes(x = date, 
                     y = mean_WC_mean,
                     color = as.factor(Depth)),
                 size = 3) +
      scale_y_continuous(expression(paste(Theta[soil], "( ", m^3, " ", m^-3, ")")),
                         limits = c(0, NA),
                         sec.axis = sec_axis(~.*ratio2,
                                             name = "Watering (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 week") + 
      theme_bw(base_size = 16) +
      scale_fill_manual(values = c("lightblue", "gray")[1:color_ind]) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm")[unique(temp_WC$Depth)], 
                         values = c("#52958b", "#494e6b", "#b9c4c9")[unique(temp_WC$Depth)]) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank()) +
      guides(color = guide_legend(
        override.aes = list(linetype = c(0, 0, 0)[unique(temp_WC$Depth)])))
    
    # Air temp and VPD
    temp_airT <- Ta_daily %>%
      filter(Variable == "T",
             Location == "outside",
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2]) %>%
      group_by(date) %>%
      summarize(mean_T_mean = mean(mean, na.rm = TRUE),
                mean_T_min = mean(min,  na.rm = TRUE),
                mean_T_max = mean(max,  na.rm = TRUE))
    
    temp_D <- Ta_daily %>%
      filter(Variable == "D",
             Location == "outside",
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2]) %>%
      group_by(date) %>%
      summarize(mean_D_mean = mean(mean, na.rm = TRUE),
                mean_D_min = mean(min,  na.rm = TRUE),
                mean_D_max = mean(max,  na.rm = TRUE))
    
    ratio3 <- max(temp_D$mean_D_max, na.rm = TRUE) / 
      max(temp_airT$mean_T_max,  na.rm = TRUE)
    
    fig_c <- ggplot() +
      geom_line(data = temp_airT,
                aes(x = date,
                    y = mean_T_mean, 
                    color = "air T"),
                lty = 2) +
      geom_errorbar(data = temp_airT,
                    aes(x = date,
                        ymin = mean_T_min,
                        ymax = mean_T_max,
                        color = "air T"),
                    width = 0,
                    alpha = 0.3) +
      geom_point(data = temp_airT,
                 aes(x = date,
                     y = mean_T_mean,
                     color = "air T"),
                 size = 3) +
      geom_line(data = temp_D,
                aes(x = date,
                    y = mean_D_mean/ratio3, 
                    color = "VPD"),
                lty = 2) +
      geom_errorbar(data = temp_D,
                    aes(x = date,
                        ymin = mean_D_min/ratio3,
                        ymax = mean_D_max/ratio3,
                        color = "VPD"),
                    width = 0,
                    alpha = 0.3) +
      geom_point(data = temp_D,
                 aes(x = date,
                     y = mean_D_mean/ratio3, 
                     color = "VPD"),
                 size = 3) +
      scale_y_continuous(expression(paste(T[air], " (", degree, "C)")),
                         limits = c(0, NA),
                         sec.axis = sec_axis(~.*ratio3,
                                             name = "VPD (kPa)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 week") + 
      theme_bw(base_size = 16) +
      scale_color_manual(values = c("#67aeca", "#cda34f")) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank()) +
      guides(color = guide_legend(
        override.aes = list(linetype = c(0, 0))))
    
    plot_grid(fig_a, fig_b, fig_c,
              ncol = 1, 
              align = "v")
    
  })

  # Function for comparing all treatments by year
  output$treatment_ts <- renderPlot({
    temp_irig <- combo %>%
      # filter(date >= as.Date("2019-07-01"),
      #        date <= as.Date("2019-10-31"))
      filter(date >= input$date_range_selector1[1],
             date <= input$date_range_selector1[2])
    
    temp_VWC <- WC_daily %>%
      filter(date >= input$date_range_selector1[1],
             date <= input$date_range_selector1[2]) %>%
      # filter(date >= as.Date("2019-07-01"),
      #        date <= as.Date("2019-10-31")) %>%
      tidyr::pivot_longer(cols = Summer:Winter,
                          names_to = "treatType",
                          values_to = "Treatment") %>%
      filter(treatType == input$Treatment) %>% # input$Treatment
      group_by(Treatment, date, Depth) %>%
      summarize(mean_WC_mean = mean(WC_mean),
                mean_WC_min = mean(WC_min),
                mean_WC_max = mean(WC_max)) %>%
      mutate(depth_labs = case_when(Depth == 1 ~ "0-12 cm",
                                    Depth == 2 ~ "25 cm",
                                    Depth == 3 ~ "75 cm"))
    
    fig_WC <- ggplot(temp_VWC, aes(x = date, y = mean_WC_mean, color = Treatment)) + #
      geom_errorbar(aes(ymin = mean_WC_min,
                        ymax = mean_WC_max),
                    width = 0, 
                    alpha = 0.3) +
      geom_point(size = 3) +
      scale_y_continuous(expression(paste(Theta[soil], "( ", m^3, " ", m^-3, ")"))) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 month") +
      scale_color_manual(values = c("dodgerblue4",
                                    "cyan2",
                                    "gold",
                                    "sandybrown")) +
      facet_wrap(~depth_labs,
                 ncol = 1,
                 scale = "free_y") +
      theme_bw(base_size = 16) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    
    fig_irig <- ggplot(temp_irig) +
      geom_bar(aes(x = date, 
                   y = irrigation_mm, 
                   fill = Summer),
               stat = "identity",
               position = "dodge",
               width = 1) +
      geom_bar(aes(x = date, 
                   y = ppt_mm, 
                   fill = "Precipitation"),
               stat = "identity",
               position = "dodge",
               width = 1) +
      scale_y_continuous("Watering (mm)",
                         limits = c(0, NA)) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 month",
                   limits = c(min(temp_VWC$date), NA)) +
      scale_fill_manual(values = c("gray",
                                   "dodgerblue4",
                                   "cyan2",
                                   "gold",
                                   "sandybrown")) +
      theme_bw(base_size = 16) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    
    plot_grid(fig_irig, fig_WC,
              ncol = 1,
              align = "v",
              rel_heights = c(1, 3))
    
  })
  
  # Select clicked points 1 at a time
  observe({

    selected_points <<- rbind(selected_points,
                              nearPoints(WPWC_daily, input$plot_click,
                                         threshold = 5, maxpoints = 1, addDist = TRUE))

    selected_points <<- tail(selected_points, 1)
    dim(selected_points)

  })
  
  output$WPWC_ts <- renderPlot({
    input$plot_click
    
    temp_WPWC <- WPWC_daily %>%
      filter(Summer == input$Summer2,
             date >= input$date_range_selector2[1],
             date <= input$date_range_selector2[2]) %>%
      mutate(Depth = as.factor(Depth))
    
    temp_irig <- combo %>%
      filter(Summer == input$Summer2,
             date >= input$date_range_selector2[1],
             date <= input$date_range_selector2[2]) %>%
      mutate(ppt_mm = ifelse(is.na(ppt_mm), 0, ppt_mm),
             irrigation_mm = ifelse(is.na(irrigation_mm), 0, irrigation_mm))
    
    ratio1 <- if(nrow(temp_irig) == 0) { 1
      } else {min(temp_WPWC$WP_min, na.rm = TRUE) / 
          max(max(temp_irig$irrigation_mm,  na.rm = TRUE),
              max(temp_irig$ppt_mm, na.rm = TRUE),
              na.rm = TRUE)}
    
    ratio2 <- if(nrow(temp_irig) == 0) { 1
      } else {max(temp_WPWC$WC_max, na.rm = TRUE) / 
          max(max(temp_irig$irrigation_mm,  na.rm = TRUE),
              max(temp_irig$ppt_mm, na.rm = TRUE),
              na.rm = TRUE)}
    
    # For controlling whether/how many watering inputs are present
    color_ind <- if(nrow(temp_irig) == 0) { 0 } else { 2 }
    # For controlling fill when no irrigation or precip present
    fill_ind <- if(nrow(temp_irig) == 0) { 0 } else { 1 }
    

    fig_WP <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm*ratio1,
                   fill = "Irrigation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_bar(data = temp_irig,
               aes(x = date, y = ppt_mm*ratio1,
                   fill = "Precipitation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_errorbar(data = temp_WPWC,
                    aes(x = date, 
                        y = WP_mean,
                        ymin = WP_min, 
                        ymax = WP_max,
                        color = Depth), 
                    width = 0, 
                    alpha = 0.3) +
      geom_point(data = temp_WPWC,
                 aes(x = date, 
                     y = WP_mean, 
                     color = Depth),
                 size = 3) +
      geom_point(data = selected_points, 
                 aes(x = date,
                     y = WP_mean),
                 color = "black", size = 4)+
      scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                         limits = c(NA, 0),
                         sec.axis = sec_axis(~./ratio1,
                                             name = "Watering (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 month") +
      theme_bw(base_size = 16) +
      scale_fill_manual(values = c("lightblue", "gray")[1:color_ind]) +
      scale_color_manual(labels = c("5 cm", "25 cm", "75 cm"),
                         values = c("#52958b", "#494e6b", "#b9c4c9")) +
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    
    fig_WC <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm*ratio2,
                   fill = "Irrigation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_bar(data = temp_irig,
               aes(x = date, y = ppt_mm*ratio2,
                   fill = "Precipitation"[fill_ind]),
               stat = "identity",
               width = 1,
               alpha = 0.5) +
      geom_errorbar(data = temp_WPWC,
                    aes(x = date, 
                        y = WC_mean,
                        ymin = WC_min, 
                        ymax = WC_max,
                        color = Depth), 
                    width = 0, 
                    alpha = 0.3) +
      geom_point(data = temp_WPWC,
                 aes(x = date, 
                     y = WC_mean, 
                     color = Depth),
                 size = 3) +
      geom_point(data = selected_points, 
                 aes(x = date,
                     y = WC_mean),
                 color = "black", size = 4)+
      scale_y_continuous(expression(paste(Theta[soil], "( ", m^3, " ", m^-3, ")")),
                         limits = c(0, NA),
                         sec.axis = sec_axis(~./ratio2,
                                             name = "Watering (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 month") +
      theme_bw(base_size = 16) +
      scale_fill_manual(values = c("lightblue", "gray")[1:color_ind]) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"),
                         values = c("#52958b", "#494e6b", "#b9c4c9")) +
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    
    plot_grid(fig_WP, fig_WC,
              ncol = 1,
              align = "v")
  })

  output$WPWC_scatter <- renderPlot({

    temp_scatter <- WPWC_daily %>%
      filter(Summer == input$Summer2,
             date >= input$date_range_selector2[1],
             date <= input$date_range_selector2[2]) 
    
      # Lookup table for facet labels
      depths <- c(
        `1` = "shallow",
        `2` = "25 cm",
        `3` = "75 cm"
      )

      ggplot(temp_scatter,
             mapping = aes(x = WC_mean, y = WP_mean, color = as.factor(Depth))) +
      geom_errorbar(aes(ymin = WP_min, ymax = WP_max), 
                    width = 0, 
                    alpha = 0.3) +
      geom_errorbarh(aes(xmin = WC_min, xmax = WC_max), 
                     height = 0, 
                     alpha = 0.3) +
      geom_point(size = 3) +
      scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
      scale_x_continuous(expression(paste(Theta, " (", m^3, " ", m^-3, ")"))) +
      facet_wrap(~Depth,
                 ncol = 3,
                 labeller = labeller(Depth = depths)) +
      theme_bw(base_size = 16) +
      scale_color_manual(values = c("#52958b", "#494e6b", "#b9c4c9")) +
      theme(strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(color = "none")
  })
  
  # output$info <- renderPrint({
  #   nearPoints(df = WPWC_daily, 
  #              coordinfo = input$plot_click,
  #              threshold = 15, maxpoints = NULL, addDist = TRUE)
  # })
  
  output$click_info <- renderUI({
    click <- input$plot_click
    point <- nearPoints(WPWC_daily, 
                        click,
                        threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_px <- click$coords_css$x
    top_px <- click$coords_css$y
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 1, 
                    "px; top:", top_px + 1, "px;")
    
    # Write tooltip
    tooltip <- paste0("<b> Date</b>: ", point$date,
                      "<br> <b>Season</b>: ", point$Season,
                      "<br> <b>&#x398;</b>: ", round(point$WC_mean, 3),
                      "<br> <b>&#x3A8;</b>: ", round(point$WP_mean, 3))
    wellPanel(
      style = style,
      p(HTML(tooltip))
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
