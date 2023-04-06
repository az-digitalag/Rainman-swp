# UI

# Define UI for application with three tabs
shinyUI(navbarPage("RainManSR",
                   tabPanel("Explore time series",
                            titlePanel("Soil and air variables by treatment"),
                            # Sidebar with drop down input for treatments, times, and date ranges
                            sidebarLayout(
                              sidebarPanel( width = 3,
                                # Select Summer treatment
                                selectInput(inputId = "Summer",
                                            label = "Select summer treatment(s):",
                                            multiple = TRUE,
                                            choices = levels(Ts_daily$Summer),
                                            selected = "S1"),
                                # # Select Winter treatment
                                selectInput(inputId = "Winter",
                                            label = "Select winter treatment(s):",
                                            choices = unique(Ts_daily$Winter),
                                            multiple = TRUE,
                                            selected = unique(Ts_daily$Winter)),
                                # Select Year
                                selectInput(inputId = "Year",
                                            label = "Select hydrological year:", 
                                            choices = unique(season$Year),
                                            selected = 2022), 
                                # Select Season
                                uiOutput("dyn_season")
                              ),
                              # Show a size plot for selected species
                              mainPanel(
                                h3("Mean by treatment(s)"),
                                fluidRow(
                                  # Select range of dates
                                  uiOutput("dyn_slider"),
                                  # Plot time series
                                  plotOutput("seasonal_ts",
                                             width = "100%", height = "600px")),
                                h5("Top two panels: error bars represent the range from the mean minimum to the mean maximum across plots."),
                                h5("Bottom panel: error bars represent the daily range in varaiables. ")
                              )
                            )),
                   tabPanel("Compare treatments",
                            titlePanel("VWC by treatment"),
                            # Sidebar with drop down input for treatments, times, and date ranges
                            sidebarLayout(
                              sidebarPanel(width = 3, 
                                # Select treatment
                                selectInput(inputId = "Treatment",
                                            label = "Select treatment type:",
                                            choices = c("Summer", "Winter")),
                                # Select Year
                                selectInput(inputId = "Year1",
                                            label = "Select hydrological year:", 
                                            choices = unique(season$Year),
                                            selected = 2022)
                              ),
                              # Show a size plot for selected species
                              mainPanel(
                                # Select range of dates
                                uiOutput("dyn_slider1"),
                                fluidRow(plotOutput("treatment_ts", width = "100%", height = "800px"))
                              )
                            )),
                   tabPanel("Compare SWP and VWC",
                            titlePanel("Empirical time series"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                # Select Plot from House 3
                                selectInput(inputId = "Summer2",
                                            label = "Select summer treatment:",
                                            choices = unique(WPWC_daily$Summer)),
                                # Select Year
                                selectInput(inputId = "Year2",
                                            label = "Select hydrological year:", 
                                            choices = unique(season$Year),
                                            selected = 2022)
                              ),
                              mainPanel(
                                # Select range of dates
                                uiOutput("dyn_slider2"),
                                fluidRow(plotOutput("WPWC_ts", width = "100%", height = "400px"))
                                # h4('Click on point to obtain values. '),
                                # fluidRow(plotOutput("WPWC_scatter", 
                                #                     width = "100%", 
                                #                     height = "300px",
                                #                     click = clickOpts("plot_click"))),
                                # uiOutput("click_info")
                              )
                              
                  
                            ))
)
)