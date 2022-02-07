#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
load("../data/soilTempDaily.Rdata")
load("../data/soilWaterContentDaily.Rdata")
load("../data/soilWaterPotentialDaily.Rdata")
load("../data/soilWC-WPDaily.Rdata")

# Define UI for application with two tabs
ui <- navbarPage("RainManSR",
  tabPanel("Daily time series",
           titlePanel("Daily Soil variables"),
           # Sidebar with drop down input for House number
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "House",
                           label = "Select House",
                           choices = levels(Ts_daily$House)),
               selectInput(inputId = "Plot",
                           label = "Select Plot",
                           choices = levels(Ts_daily$Plot))
             ),
             # Show a size plot for selected species
             mainPanel(
               fluidRow(plotOutput("soilTempTimeseries", width = "800px", height = "150px")),
               fluidRow(plotOutput("soilVWCTimeseries", width = "800px", height = "150px")),
               fluidRow(plotOutput("soilWPTimeseries", width = "800px", height = "150px"))
             )
           )),
  tabPanel("Comparison SWP-SWC",
           titlePanel("House 3"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "Plot1",
                           label = "Select Plot",
                           choices = unique(SW_long$Plot))
             ),
             # Show a size plot for selected species
             mainPanel(
               fluidRow(plotOutput("SWCSWP_ts", width = "800px", height = "300px")),
               fluidRow(plotOutput("SWCSWP_scatter", width = "750px", height = "250px"))
             )
           ))
  
)

# Create timeseries plot for swc and swp on same panel
server <- function(input, output) {
  
  output$soilTempTimeseries <- renderPlot({
    Ts_daily %>%
      filter(House == input$House,
             Plot == input$Plot) %>%
      ggplot(mapping = aes(x = date, y = Ts_mean, color = Depth)) +
      geom_errorbar(aes(ymin = Ts_min, ymax = Ts_max), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(T[soil], " (", degree, "C)"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") +
      theme_bw(base_size = 12) +
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
             Plot == input$Plot) %>%
      ggplot(mapping = aes(x = date, y = WC_mean, color = Depth)) +
      geom_errorbar(aes(ymin = WC_min, ymax = WC_max), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(Theta[soil], "( ", m^3, " ", m^-3, ")"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      theme_bw(base_size = 12) +
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
             Plot == input$Plot) %>%
      ggplot(mapping = aes(x = date, y = WP_mean/1000, color = Depth)) +
      geom_errorbar(aes(ymin = WP_min/1000, ymax = WP_max/1000), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      theme_bw(base_size = 12) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  
  output$SWCSWP_ts <- renderPlot({
    SW_long %>%
      filter(Plot == input$Plot1) %>%
      ggplot(mapping = aes(x = date, y = mean, color = Depth)) +
      geom_errorbar(aes(ymin = min, ymax = max), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      facet_wrap(~var, 
                 labeller = labeller(var = label_parsed),
                 scales = "free_y",
                 ncol = 1) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      theme_bw(base_size = 12) +
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
      filter(Plot == input$Plot1) %>%
      select(-var) %>%
      tidyr::pivot_wider(names_from = Variable,
                         values_from = 8:10) %>%
      ggplot(mapping = aes(x = mean_WC, y = mean_WP, color = Depth)) +
      geom_point(size = 0.25) +
      geom_errorbar(aes(ymin = min_WP, ymax = max_WP), width = 0, alpha = 0.2) +
      geom_errorbarh(aes(xmin = min_WC, xmax = max_WC), height = 0, alpha = 0.2) +
      scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
      scale_x_continuous(expression(paste(Theta, " (", m^3, " ", m^-3, ")"))) +
      facet_wrap(~Depth, 
                 scales = "free",
                 ncol = 3) +
      theme_bw(base_size = 12) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#5E4FA2", "#3288BD", "#66C2A5")) +
      theme(strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
