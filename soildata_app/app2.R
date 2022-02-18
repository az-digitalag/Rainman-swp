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
library(cowplot)

# First tab: Seasonal timeseries by treatment
load("soilTempDaily.Rdata")
load("soilWaterContentDaily.Rdata")
load("airTempDaily.Rdata")
load("irrigationDaily.Rdata")
load("season.Rdata")

# Second tab: Compare SWC and SWP by plot
load("soilWPWCDaily.Rdata")

# Define UI for application with two tabs
ui <- navbarPage("RainManSR",
  tabPanel("Daily time series",
           titlePanel("Daily Soil variables"),
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
               fluidRow(plotOutput("seasonal_ts", width = "100%", height = "500px"))
             )
           )),
  tabPanel("Compare SWP-SWC",
           titlePanel("House 3"),
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
               fluidRow(plotOutput("WPWC_ts", width = "100%", height = "500px")),
               fluidRow(plotOutput("WPWC_scatter", width = "750px", height = "250px"))
             )
           ))
  # tabPanel("Hourly SWP-SWC",
  #          titlePanel("House 3"),
  #          sidebarLayout(
  #            sidebarPanel(
  #              # Select Plot from House 3
  #              selectInput(inputId = "Plot2",
  #                          label = "Select Plot",
  #                          choices = unique(WC$Plot)),
  #              # Select range of dates
  #              sliderInput("slider3", label = h3("Date/time Range"), 
  #                          min = min(WC$dt), 
  #                          max = max(WC$dt), 
  #                          value = range(WC$dt),
  #                          step = 60*30
  #              ),
  #            ),
  #            
  #            # Show a size plot for selected species
  #            mainPanel(
  #              fluidRow(plotOutput("SWCSWP_ts_hourly", width = "100%", height = "500px")),
  #              fluidRow(plotOutput("SWCSWP_scatter_hourly", width = "100%", height = "250px"))
  #            )
  #          ))
  
)

# Create timeseries plot for swc and swp on same panel
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
  
  # Render a UI for selecting date range, tab 1
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
  
  # Render a UI for selecting date range tab 2
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

  output$seasonal_ts <- renderPlot({
    
    # Soil Temp
    temp_soilT <- Ts_daily %>%
      filter(Summer == input$Summer,
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2]) %>%
      group_by(Summer, date, Depth) %>%
      summarize(mean_Ts_mean = mean(Ts_mean, na.rm = TRUE),
                mean_Ts_min = mean(Ts_min,  na.rm = TRUE),
                mean_Ts_max = mean(Ts_max,  na.rm = TRUE))
    
    temp_irig <- irig %>%
      filter(Summer == input$Summer,
             date >= input$date_range_selector[1],
             date <= input$date_range_selector[2])

    ratio1 <- max(temp_irig$irrigation_mm,  na.rm = TRUE) / 
      max(temp_soilT$mean_Ts_max, na.rm = TRUE)
    
    fig_a <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm/ratio1),
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
                                             name = "Irrigation (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 week") +
      theme_bw(base_size = 16) +
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
     
     temp_irig <- irig %>%
       filter(Summer == input$Summer,
              date >= input$date_range_selector[1],
              date <= input$date_range_selector[2])
     
     ratio2 <- max(temp_irig$irrigation_mm,  na.rm = TRUE) / 
       max(temp_WC$mean_WC_max, na.rm = TRUE)
     
     fig_b <- ggplot() +
       geom_bar(data = temp_irig,
                aes(x = date, y = irrigation_mm/ratio2),
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
                                              name = "Irrigation (mm)")) +
       scale_x_date(date_labels = "%b %d",
                    date_breaks = "1 week") + 
       theme_bw(base_size = 16) +
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
  
  output$WPWC_ts <- renderPlot({
    temp_WPWC <- WPWC_daily %>%
      filter(Summer == input$Summer2,
             date >= input$date_range_selector2[1],
             date <= input$date_range_selector2[2]) %>%
      mutate(Depth = as.factor(Depth))
    
    temp_irig <- irig %>%
      filter(Summer == input$Summer2,
    date >= input$date_range_selector2[1],
    date <= input$date_range_selector2[2])
    
    ratio1 <- min(temp_WPWC$WP_min, na.rm = TRUE) / 
      max(temp_irig$irrigation_mm,  na.rm = TRUE)
    
    ratio2 <- max(temp_WPWC$WC_max, na.rm = TRUE) / 
      max(temp_irig$irrigation_mm,  na.rm = TRUE)

    fig_WP <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm*ratio1),
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
      scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                         limits = c(NA, 0),
                         sec.axis = sec_axis(~./ratio1,
                                             name = "Irrigation (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 month") +
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("5 cm", "25 cm", "75 cm"),
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    fig_WC <- ggplot() +
      geom_bar(data = temp_irig,
               aes(x = date, y = irrigation_mm*ratio2),
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
      scale_y_continuous(expression(paste(Theta[soil], "( ", m^3, " ", m^-3, ")")),
                         limits = c(0, NA),
                         sec.axis = sec_axis(~./ratio2,
                                             name = "Irrigation (mm)")) +
      scale_x_date(date_labels = "%b %d",
                   date_breaks = "1 month") +
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"),
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    plot_grid(fig_WP, fig_WC,
              ncol = 1,
              align = "v")
  })

  output$WPWC_scatter <- renderPlot({
    
    WPWC_daily %>%
      filter(Summer == input$Summer2,
             date >= input$date_range_selector2[1],
             date <= input$date_range_selector2[2]) %>%
      mutate(depth.label = case_when(Depth == 1 ~ "shallow",
                                     Depth == 2 ~ "25 cm",
                                     Depth == 3 ~ "75 cm"),
             depth.label = factor(depth.label, levels = c("shallow", "25 cm", "75 cm"))) %>%
      ggplot(mapping = aes(x = WC_mean, y = WP_mean, color = as.factor(Depth))) +
      geom_point(size = 1) +
      geom_errorbar(aes(ymin = WP_min, ymax = WP_max), width = 0, alpha = 0.2) +
      geom_errorbarh(aes(xmin = WC_min, xmax = WC_max), height = 0, alpha = 0.2) +
      scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
      scale_x_continuous(expression(paste(Theta, " (", m^3, " ", m^-3, ")"))) +
      facet_wrap(~depth.label,
                 ncol = 3) +
      theme_bw(base_size = 16) +
      scale_color_manual(values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(color = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
