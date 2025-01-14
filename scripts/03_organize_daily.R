# Data manipulation for app
# Daily summaries of VWC (exclude SWP), soil T, VPD, and irrigation


library(readr)
library(dplyr)
library(tidyr)
library(udunits2)
library(purrr)
library(plantecophys)
library(fuzzyjoin)

# Read in irrigation dates
irig <- read_csv("data/irrigation.csv") %>%
  tidyr::pivot_longer(-date, 
                      names_to = "Treatment", 
                      values_to = "irrigation_mm") %>%
  drop_na() %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))



# No roof prior to 5/20/2020, so need to merge with precipitation
# Precip record only to 4/30/2020, but according to SRER records, there was May precip in 2020
ppt <- read_csv("data/Daily_Raingage_02262022.csv") %>%
  mutate(date = as.Date(TIMESTAMP, format = "%m/%d/%Y"),
         ppt_mm = ud.convert(`Rainfall (inch)`, "in", "mm")) %>%
  select(date, ppt_mm) %>%
  filter(date <= as.Date("2020-05-20"), 
         ppt_mm != 0)

add_trt <- function(trt, df) {
  df$Treatment <- trt
  df
}
ppt_trt <- map_dfr(c("S1", "S2", "S3", "S4", "W1", "W2", "W3"),
                    add_trt,
                    df = ppt) %>%
  arrange(date)

combo <- full_join(irig, ppt_trt, by = c("date", "Treatment")) %>%
  arrange(date, Treatment)


# Read in treatments for each house/plot combination
treats <- read.csv("data/treatments.csv") %>%
  mutate(Summer = factor(Summer, levels = c("S1", "S2", "S3", "S4")),
         Winter = factor(Winter, levels = c("W1", "W2", "W3")))

# Label with Hydrologic year and season
season <- data.frame(Year = c(2019,                              
                              rep(2020, 4),
                              rep(2021, 4),
                              rep(2022, 4),
                              rep(2023, 4)),
                     Season = c("Growing",
                                rep(c("Winter", "Spring", "Premonsoon", "Growing"), 4)),
                     st = c("07-01",
                            rep(c("11-01", "02-01", "05-01", "07-01"), 4)),
                     en = c("10-31",
                            rep(c("01-31", "04-30", "06-30", "10-31"), 4))) %>%
  mutate(en = as.Date(paste0(Year, "-", en)),
         st = case_when(Season == "Winter" ~ as.Date(paste0(as.numeric(Year) - 1, "-", st)),
                        Season %in% c("Spring", "Premonsoon", "Growing") ~ as.Date(paste0(Year, "-", st))),
         Season = factor(Season, levels = c("Winter", "Spring", "Premonsoon", "Growing"))) %>%
  as_tibble()


# Read in half-hourly soil temperature (Ts) 
Ts <- read.csv("data/Halfhourly_TS_010123.csv") %>%
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
WC <- read.csv("data/Halfhourly_VWC_SWP_Weather_010123.csv") %>%
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
Ta <- read.csv("data/Halfhourly_VWC_SWP_Weather_010123.csv") %>%
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
  filter(is.finite(Ts_mean)) %>%
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
save(combo, file = "soildata_app/comboDaily.Rdata")
save(season, file = "soildata_app/season.Rdata")


##### Match SWP to SWC #####


WP_daily <- WC %>%
  filter(Variable == "SWP") %>%
  group_by(ID, date) %>%
  summarise(WP_mean = mean(value/1000, na.rm = TRUE),
            WP_min = min(value/1000, na.rm = TRUE),
            WP_max = max(value/1000, na.rm = TRUE),
            House = unique(House),
            Depth = unique(Depth),
            Plot = unique(Plot),
            Summer = unique(Summer),
            Winter = unique(Winter)) %>%
  mutate(plotID = paste0(House, "_", Plot),
         WP_min = ifelse(!is.finite(WP_min), NA, WP_min),
         WP_max = ifelse(!is.finite(WP_max), NA, WP_max)) %>%
  ungroup()%>%
  fuzzy_left_join(season, 
                  by = c("date" = "st",
                         "date" = "en"),
                  match_fun = list(`>=`, `<=`)) %>%
  select(-st, -en)%>%
  filter(!is.na(Season))

WPWC_daily <- WP_daily %>%
  left_join(select(WC_daily, -ID))

# Write out wide .Rdata files to soildata_app/ folder
# Intended for second panel of app
save(WPWC_daily, file = "soildata_app/soilWPWCDaily.Rdata")
