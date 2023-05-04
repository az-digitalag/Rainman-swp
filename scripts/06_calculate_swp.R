# Use existing function from source to predict soil moisture
# Summarize half-hourly to daily
# Write as .csv
# Create soilWaterPotentialDaily.Rdata object in soildata_app/
library(tidyverse)

# Load function
source("source/swc2swp.R")

# Read in treatments for each house/plot combination
treats <- read.csv("data/treatments.csv") %>%
  mutate(Summer = factor(Summer, levels = c("S1", "S2", "S3", "S4")),
         Winter = factor(Winter, levels = c("W1", "W2", "W3")))

# Load half-hourly VWC data
# Read in half-hourly volumetric water content (WC), pivot, and remove SWP
WC <- read.csv("data/Halfhourly_VWC_SWP_Weather_010123.csv") %>%
  dplyr::select(-T_inside, -RH_inside, -T_outside, -RH_outside) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP),
         date = as.Date(TIMESTAMP)) %>%
  rename(dt = TIMESTAMP) %>%
  tidyr::pivot_longer(!c(dt, date), 
                      names_to = c("Variable", "Plot", "Depth", "House"), 
                      names_pattern = "(.*)_P(.*)_(.*)_H(.*)") %>%
  filter(Variable == "VWC") %>%
  mutate(House = as.numeric(House),
         Depth = as.numeric(Depth),
         Plot = as.numeric(Plot),
         ID = paste0(Variable, "_P", Plot, "_", Depth, "_H", House)) %>%
  arrange(ID, dt) %>%
  left_join(treats, by = c("House", "Plot"))

# Apply swc2swp

WC <- WC %>%
  mutate()


