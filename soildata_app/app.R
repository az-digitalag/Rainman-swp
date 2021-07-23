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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SWC-SWP relationships"),
  
  # Sidebar with text input for penguin species 
  sidebarLayout(
    sidebarPanel(
      # textInput(inputId = "species",
      #           label = "Species",
      #           value = "Gentoo")
      selectInput(inputId = "species",
                  label = "Select species",
                  choices = unique(penguins$species))
    ),
    
    # Show a size plot for selected species
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("statsTable")
    )
  )
)

# Create timeseries plot for swc and swp on same panel
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    ggplot(data = penguins[penguins$species == input$species, ], 
           mapping = aes(x = bill_length_mm, 
                         y = bill_depth_mm,
                         color = sex)) +
      geom_point()
  })
  # Add results of linear model to output
  output$statsTable <- renderTable({
    # Create the linear model
    model <- lm(formula = bill_depth_mm ~ bill_length_mm,
                data = penguins[penguins$species == input$species, ])
    
    # Extract summary statistics
    model_summary <- summary(model)
    
    # Print coefficients table
    model_summary$coefficients
    
  }, rownames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
