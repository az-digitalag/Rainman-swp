# To develop moisture release curves
# Extract daily SWC and SWP 
# Remove outliers as needed by depth and pulse

library(dplyr)
library(fuzzyjoin)
library(ggplot2)

# Load irrigation + soil + season data
load("soildata_app/irrigationDaily.Rdata")
load("soildata_app/soilWPWCDaily.Rdata")
load("soildata_app/season.Rdata")

WPWC_daily <- WPWC_daily %>%
  left_join(irig) 

# Medium depth is refilled continuously and can be checked without filtering
WPWC_daily %>%
  filter(Year != 2019,
         Depth == 2) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = Season)) +
  facet_grid(cols = vars(Summer),
             rows = vars(Year)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())

# Isolate S1
S1 <- WPWC_daily %>%
  filter(Summer == "S1")

# Remove day of and 1 day after irrigation
# Day of irrigation + 1
rem_date <- irig %>%
  filter(Summer == "S1",
         irrigation_mm != 0) %>%
  mutate(date_1 = date + 1) %>%
  select(-Summer, -irrigation_mm) %>%
  tidyr::pivot_longer(date:date_1, 
                      names_to = "type",
                      values_to = "date")

# Remove day and day + 1 of irrigation
S1_rem <- S1 %>%
  anti_join(rem_date) %>%
  mutate(year_season = paste0(Year, "_", Season)) 

# Plot
S1_rem %>%
  filter(Depth == 1,
         year_season %in% c("2020_Growing",
                            "2021_Spring", 
                            "2021_Growing")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point() +
  # facet_wrap(~year_season) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())

# Isolate S4
S4 <- WPWC_daily %>%
  filter(Summer == "S4")

# Medium depth is refilled continuously and can be checked without filtering
S4 %>%
  filter(Depth == 2) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = Season)) 

# Shallow depth dries out, particularly prior to initial monsoon treatment
# filter out day of and day after irrigation
# Check whether differs by pulse during growing season
# Day of irrigation + 2
rem_date <- irig %>%
  filter(Summer == "S4",
         irrigation_mm != 0) %>%
  mutate(date_1 = date + 1,
         date_2 = date + 2) %>%
  select(-Summer, -irrigation_mm) %>%
  tidyr::pivot_longer(date:date_2, 
                      names_to = "type",
                      values_to = "date")

# Remove day and day + 2 of irrigation
S4_rem <- S4 %>%
  anti_join(rem_date) %>%
  mutate(year_season = paste0(Year, "_", Season)) 

# Plot
S4_rem %>%
  filter(Depth == 1,
         year_season %in% c("2020_Growing",
                            "2021_Spring", 
                            "2021_Growing")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point() +
  facet_wrap(~year_season)

### Add pulse labels to each growing season irrigation event
pulse_ends <- as.Date(c("2020-08-02",
                        "2020-08-23",
                        "2020-09-13",
                        "2020-10-28",
                        "2021-07-25",
                        "2021-08-15",
                        "2021-09-05",
                        "2021-10-31"))

pulse <- irig %>%
  filter(Summer == "S4",
         irrigation_mm != 0) %>%
  fuzzy_left_join(season, 
                by = c("date" = "st",
                       "date" = "en"),
                match_fun = list(`>=`, `<=`)) %>%
  select(-st, -en) %>%
  filter(!is.na(Season),
         Season == "Growing",
         irrigation_mm != 30) %>% # Remove first winter rewater
  mutate(en = pulse_ends,
         pulse = cur_group_rows()) %>%
  rename(st = date) %>%
  select(st, en, pulse)

S4_rem_pulse <- S4_rem %>%
  fuzzy_left_join(pulse, 
                  by = c("date" = "st",
                         "date" = "en"),
                  match_fun = list(`>=`, `<=`)) %>%
  select(-st, -en) 

S4_rem_pulse %>%
  filter(Depth == 1,
         !is.na(pulse)) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = as.factor(pulse))) +
  facet_wrap(~pulse, nrow = 2) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())

S4_rem_pulse %>%
  filter(Depth == 1,
         !is.na(pulse)) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = as.factor(pulse))) +
  facet_wrap(~Year) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())
  