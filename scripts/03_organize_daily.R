# Data manipulation for app
# Daily summaries of VWC (exclude SWP), soil T, VPD, and irrigation


library(readr)
library(dplyr)
library(tidyr)
library(plantecophys)
library(fuzzyjoin)

# Read in treatments for each house/plot combination
treats <- read.csv("data/treatments.csv") %>%
  mutate(Summer = factor(Summer, levels = c("S1", "S2", "S3", "S4")),
         Winter = factor(Winter, levels = c("W1", "W2", "W3")))

# Label with Hydrologic year and season
season <- data.frame(Year = c(rep(2020, 2),
                              rep(2021, 4)),
                     Season = c(c("Premonsoon", "Growing"),
                                rep(c("Winter", "Spring", "Premonsoon", "Growing"), 1)),
                     st = c(c("05-01", "07-01"),
                            rep(c("11-01", "02-01", "05-01", "07-01"), 1)),
                     en = c(c("06-30", "10-31"),
                            rep(c("01-31", "04-30", "06-30", "10-31"), 1))) %>%
  mutate(en = as.Date(paste0(Year, "-", en)),
         st = case_when(Season == "Winter" ~ as.Date(paste0(as.numeric(Year) - 1, "-", st)),
                        Season %in% c("Spring", "Premonsoon", "Growing") ~ as.Date(paste0(Year, "-", st))),
         Season = factor(Season, levels = c("Winter", "Spring", "Premonsoon", "Growing"))) %>%
  as_tibble()


# Read in half-hourly soil temperature (Ts) 
Ts <- read.csv("data/Halfhourly_TS_112721.csv") %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP),
         date = as.Date(TIMESTAMP)) %>%
  rename(dt = TIMESTAMP) %>%
  tidyr::pivot_longer(!c(dt, date), names_to = c("Plot", "Depth", "House"), 
                      names_pattern = "TS_P(.*)_(.*)_H(.*)") %>%
  mutate(House = as.numeric(House),
         Depth = as.numeric(Depth),
         Plot = as.numeric(Plot),
         ID = paste0("P", Plot, "_", Depth, "_H", House)) %>%
  arrange(ID, dt) %>%
  left_join(treats, by = c("House", "Plot"))

# Read in half-hourly volumetric water content (WC) 
WC <- read.csv("data/Halfhourly_VWC_SWP_Weather_112721.csv") %>%
  dplyr::select(-T_inside, -RH_inside, -T_outside, -RH_outside) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP),
         date = as.Date(TIMESTAMP)) %>%
  rename(dt = TIMESTAMP) %>%
  tidyr::pivot_longer(!c(dt, date), 
                      names_to = c("Variable", "Plot", "Depth", "House"), 
                      names_pattern = "(.*)_P(.*)_(.*)_H(.*)") %>%
  mutate(House = as.numeric(House),
         Depth = as.numeric(Depth),
         Plot = as.numeric(Plot),
         ID = paste0(Variable, "_P", Plot, "_", Depth, "_H", House)) %>%
  arrange(ID, dt) %>%
  left_join(treats, by = c("House", "Plot"))

# Read in half-hourly atmospheric variables (Ta)
Ta <- read.csv("data/Halfhourly_VWC_Weather_030621.csv") %>%
  dplyr::select(TIMESTAMP, T_inside, RH_inside, T_outside, RH_outside) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP),
         date = as.Date(TIMESTAMP),
         T_inside = ifelse(T_inside < -10, NA, T_inside),
         T_outside = ifelse(T_outside < -10, NA, T_outside),
         D_inside = RHtoVPD(RH_inside, T_inside),
         D_outside = RHtoVPD(RH_outside, T_outside)) %>%
  rename(dt = TIMESTAMP) %>%
  tidyr::pivot_longer(!c(dt, date), 
                      names_to = c("Variable", "Location"), 
                      names_pattern = "(.*)_(.*)") %>% 
  arrange(Variable, Location, dt)

# Read in irrigation dates
irig <- read_csv("data/irrigation.csv") %>%
  tidyr::pivot_longer(-date, 
                      names_to = "Summer", 
                      values_to = "irrigation_mm") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))


# Summarize half-hourly variables to daily
Ts_daily <- Ts %>%
  group_by(ID, date) %>%
  summarise(Ts_mean = mean(value, na.rm = T),
            Ts_min = min(value, na.rm = T),
            Ts_max = max(value, na.rm = T),
            House = unique(House),
            Depth = unique(Depth),
            Plot = unique(Plot),
            Summer = unique(Summer),
            Winter = unique(Winter)) %>%
  mutate(plotID = paste0(House, "_", Plot)) %>%
  ungroup() %>%
  fuzzy_left_join(season, 
                  by = c("date" = "st",
                         "date" = "en"),
                  match_fun = list(`>=`, `<=`)) %>%
  select(-st, -en) %>%
  filter(!is.na(Season))

WC_daily <- WC %>%
  filter(Variable == "VWC") %>%
  group_by(ID, date) %>%
  summarise(WC_mean = mean(value, na.rm = TRUE),
            WC_min = min(value, na.rm = TRUE),
            WC_max = max(value, na.rm = TRUE),
            House = unique(House),
            Depth = unique(Depth),
            Plot = unique(Plot),
            Summer = unique(Summer),
            Winter = unique(Winter)) %>%
  mutate(plotID = paste0(House, "_", Plot),
         WC_min = ifelse(!is.finite(WC_min), NA, WC_min),
         WC_max = ifelse(!is.finite(WC_max), NA, WC_max)) %>%
  ungroup()%>%
  fuzzy_left_join(season, 
                  by = c("date" = "st",
                         "date" = "en"),
                  match_fun = list(`>=`, `<=`)) %>%
  select(-st, -en)%>%
  filter(!is.na(Season))


Ta_daily <- Ta %>%
  group_by(Variable, Location, date) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(min = ifelse(!is.finite(min), NA, min),
         max = ifelse(!is.finite(max), NA, max),
         var = case_when(Variable == "T" ~ "T~(~degree~C)",
                         Variable == "RH" ~ "RH~('%')",
                         Variable == "D" ~ "D~(kPa)"),
         Location = factor(Location, levels = c("outside", "inside")))%>%
  fuzzy_left_join(season, 
                  by = c("date" = "st",
                         "date" = "en"),
                  match_fun = list(`>=`, `<=`)) %>%
  select(-st, -en) %>%
  filter(!is.na(Season))


# Write out daily .Rdata files to soildata_app/ folder
# Intended for first panel of app
save(Ts_daily, file = "soildata_app/soilTempDaily.Rdata")
save(WC_daily, file = "soildata_app/soilWaterContentDaily.Rdata")
save(Ta_daily, file = "soildata_app/airTempDaily.Rdata")
save(irig, file = "soildata_app/irrigationDaily.Rdata")
save(season, file = "soildata_app/season.Rdata")
