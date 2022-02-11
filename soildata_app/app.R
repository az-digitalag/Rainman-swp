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
load("soilTempDaily.Rdata")
load("soilWaterContentDaily.Rdata")
load("soilWaterPotentialDaily.Rdata")
load("soilWC-WPDaily.Rdata")

# Define UI for application with two tabs
ui <- navbarPage("RainManSR",
  tabPanel("Daily time series",
           titlePanel("Daily Soil variables"),
           # Sidebar with drop down input for House number
           sidebarLayout(
             sidebarPanel(
               # Select House
               selectInput(inputId = "House",
                           label = "Select House",
                           choices = levels(Ts_daily$House)),
               # Select Plot
               selectInput(inputId = "Plot",
                           label = "Select House", 
                           choices = ""),
               # Select range of dates
               sliderInput("slider1", label = h3("Date Range"), 
                           min = min(Ts_daily$date), 
                           max = max(Ts_daily$date), 
                           value = range(Ts_daily$date)
               ),
             ),
             # Show a size plot for selected species
             mainPanel(
               fluidRow(plotOutput("soilTempTimeseries", width = "100%", height = "250px")),
               fluidRow(plotOutput("soilVWCTimeseries", width = "100%", height = "250px")),
               fluidRow(plotOutput("soilWPTimeseries", width = "100%", height = "250px"))
             )
           )),
  tabPanel("Comparison SWP-SWC",
           titlePanel("House 3"),
           sidebarLayout(
             sidebarPanel(
               # Select Plot from House 3
               selectInput(inputId = "Plot1",
                           label = "Select Plot",
                           choices = unique(SW_long$Plot)),
               # Select range of dates
               sliderInput("slider2", label = h3("Date Range"), 
                           min = min(SW_long$date), 
                           max = max(SW_long$date), 
                           value = range(SW_long$date)
               ),
             ),
             
             # Show a size plot for selected species
             mainPanel(
               fluidRow(plotOutput("SWCSWP_ts", width = "100%", height = "500px")),
               fluidRow(plotOutput("SWCSWP_scatter", width = "100%", height = "250px"))
             )
           ))
  
)

# Create timeseries plot for swc and swp on same panel
server <- function(input, output) {
  
  observeEvent(input$House,{  
    temp <- as.vector(unlist(unique(Ts_daily[Ts_daily$House==input$House,"Plot"])))
    updateSelectInput(inputId = "Plot",
                      choices = temp)
  }
  )
  
  output$soilTempTimeseries <- renderPlot({
    Ts_daily %>%
      filter(House == input$House,
             Plot == input$Plot,
             date >= input$slider1[1],
             date <= input$slider1[2]) %>%
      ggplot(mapping = aes(x = date, y = Ts_mean, color = Depth)) +
      geom_errorbar(aes(ymin = Ts_min, ymax = Ts_max), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(T[soil], " (", degree, "C)"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") +
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#8C510A", "#BF812D", "#DFC27D")) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  output$soilVWCTimeseries <- renderPlot({
    WC_daily %>%
      filter(House == input$House,
             Plot == input$Plot,
             date >= input$slider1[1],
             date <= input$slider1[2]) %>%
      ggplot(mapping = aes(x = date, y = WC_mean, color = Depth)) +
      geom_errorbar(aes(ymin = WC_min, ymax = WC_max), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(Theta[soil], "( ", m^3, " ", m^-3, ")"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#9E0142", "#D53E4F", "#F46D43")) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  output$soilWPTimeseries <- renderPlot({
    WP_daily %>%
      filter(House == input$House,
             Plot == input$Plot,
             date >= input$slider1[1],
             date <= input$slider1[2]) %>%
      ggplot(mapping = aes(x = date, y = WP_mean/1000, color = Depth)) +
      geom_errorbar(aes(ymin = WP_min/1000, ymax = WP_max/1000), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  
  output$SWCSWP_ts <- renderPlot({
    SW_long %>%
      filter(Plot == input$Plot1,
             date >= input$slider2[1],
             date <= input$slider2[2]) %>%
      ggplot(mapping = aes(x = date, y = mean, color = Depth)) +
      geom_errorbar(aes(ymin = min, ymax = max), width = 0, alpha = 0.2) +
      geom_point(size = 1) +
      facet_wrap(~var, 
                 labeller = labeller(var = label_parsed),
                 scales = "free_y",
                 ncol = 1) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  output$SWCSWP_scatter <- renderPlot({
    SW_long %>%
      filter(Plot == input$Plot1,
             date >= input$slider2[1],
             date <= input$slider2[2]) %>%
      select(-var) %>%
      tidyr::pivot_wider(names_from = Variable,
                         values_from = 8:10) %>%
      ggplot(mapping = aes(x = mean_WC, y = mean_WP, color = Depth)) +
      geom_point(size = 1) +
      geom_errorbar(aes(ymin = min_WP, ymax = max_WP), width = 0, alpha = 0.2) +
      geom_errorbarh(aes(xmin = min_WC, xmax = max_WC), height = 0, alpha = 0.2) +
      scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
      scale_x_continuous(expression(paste(Theta, " (", m^3, " ", m^-3, ")"))) +
      facet_wrap(~Depth, 
                 scales = "free",
                 ncol = 3) +
      theme_bw(base_size = 16) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
