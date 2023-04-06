# global

#packages needed
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

# Third tab: Compare SWP and VWC by plot and depth
load("soilWPWCDaily.Rdata")

# initialize global variable to record clicked rows
# selected_points <- WPWC_daily[0,]