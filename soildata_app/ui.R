# UI

# Define UI for application with three tabs
shinyUI(navbarPage("RainManSR",
                   tabPanel("Daily time series",
                            titlePanel("Explore by treatment and season"),
                            # Sidebar with drop down input for treatments, times, and date ranges
                            sidebarLayout(
                              sidebarPanel(
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
                                uiOutput("dyn_season"),
                                # Select range of dates
                                uiOutput("dyn_slider")
                              ),
                              # Show a size plot for selected species
                              mainPanel(
                                h3("Mean by treatment(s)"),
                                fluidRow(plotOutput("seasonal_ts", 
                                                    width = "100%", height = "800px")),
                                h5("Top two panels: error bars represent the range from the mean minimum to the mean maximum across plots."),
                                h5("Bottom panel: error bars represent the daily range in varaiables. ")
                              )
                            )),
                   tabPanel("Compare treatments",
                            titlePanel("VWC by treatment type and year"),
                            # Sidebar with drop down input for treatments, times, and date ranges
                            sidebarLayout(
                              sidebarPanel(
                                # Select treatment
                                selectInput(inputId = "Treatment",
                                            label = "Select treatment type:",
                                            choices = c("Summer", "Winter")),
                                # Select Year
                                selectInput(inputId = "Year1",
                                            label = "Select hydrological year:", 
                                            choices = unique(season$Year),
                                            selected = 2022),
                                # Select range of dates
                                uiOutput("dyn_slider1")
                              ),
                              # Show a size plot for selected species
                              mainPanel(
                                fluidRow(plotOutput("treatment_ts", width = "100%", height = "800px"))
                              )
                            )),
                   tabPanel("Compare SWP and VWC",
                            titlePanel("Relationships by treatment and year"),
                            sidebarLayout(
                              sidebarPanel(
                                # Select Plot from House 3
                                selectInput(inputId = "Summer2",
                                            label = "Select summer treatment:",
                                            choices = unique(WPWC_daily$Summer)),
                                # Select Year
                                selectInput(inputId = "Year2",
                                            label = "Select hydrological year:", 
                                            choices = unique(season$Year),
                                            selected = 2022), 
                                # Select range of dates
                                uiOutput("dyn_slider2")
                              ),
                              
                              # Show a size plot for selected species
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Timeseries", 
                                                     fluidRow(plotOutput("WPWC_ts", width = "100%", height = "500px"))
                                                     ),
                                            tabPanel("Bivariate",
                                                     h5('Click on point to obtain values. '),
                                                     fluidRow(plotOutput("WPWC_scatter", 
                                                                         width = "100%", 
                                                                         height = "300px",
                                                                         click = clickOpts("plot_click"))),
                                                     uiOutput("click_info")))
                                
                                
                              )
                            ))
)
)