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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Soil temperature"),
  
  # Sidebar with dropdown input for House number
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "House",
                  label = "Select House",
                  choices = unique(Ts_daily$House))
    ),
    
    # Show a size plot for selected species
    mainPanel(
      plotOutput("soilTempTimeseries")
    )
  )
)

# Create timeseries plot for swc and swp on same panel
server <- function(input, output) {
  
  output$soilTempTimeseries <- renderPlot({
    ggplot(data = Ts_daily[Ts_daily$House == input$House, ], 
           mapping = aes(x = date, y = Ts_mean, color = Depth)) +
      geom_errorbar(aes(ymin = Ts_min, ymax = Ts_max), width = 0, alpha = 0.2) +
      geom_point(size = 0.25) +
      scale_y_continuous(expression(paste(T[soil], " (", degree, "C)"))) +
      scale_x_date(date_labels = "%b",
                   date_breaks = "2 months") + 
      facet_wrap(~Plot, ncol = 1) + 
      theme_bw(base_size = 12) +
      scale_color_manual(labels = c("0-12 cm", "25 cm", "75 cm"), 
                         values = c("#8C510A", "#BF812D", "#DFC27D")) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
