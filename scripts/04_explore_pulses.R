# To develop moisture release curves
# Extract daily SWC and SWP 
# Remove outliers as needed by depth and pulse

library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(cowplot)

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
  filter(Summer == "S1")%>%
  mutate(year_season = paste0(Year, "_", Season))

# Remove day of and 1 day after irrigation
rem_date <- irig %>%
  filter(Summer == "S1",
         irrigation_mm != 0) %>%
  mutate(date_1 = date + 1) %>%
  select(-Summer, -irrigation_mm) %>%
  tidyr::pivot_longer(date:date_1, 
                      names_to = "type",
                      values_to = "date")

S1_rem <- S1 %>%
  anti_join(rem_date) %>%
  mutate(year_season = paste0(Year, "_", Season)) 

S1_rem %>%
  filter(Depth == 1,
         year_season %in% c("2020_Growing",
                            "2021_Spring", 
                            "2021_Growing")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point() +
  facet_wrap(~year_season) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())


# Label all days of irrigation
irig_date_S1 <- irig %>%
  filter(Summer == "S1",
         irrigation_mm != 0) %>%
  mutate(date_1 = date + 1,
         date_2 = date + 2) %>%
  select(-Summer, -irrigation_mm) %>%
  tidyr::pivot_longer(date:date_2, 
                      names_to = "type",
                      values_to = "date") %>%
  mutate(irig_label = case_when(type == "date" ~ 0,
                           type != "date" ~ as.numeric(substr(type, 6, 6))))

S1_irig <- S1 %>%
  left_join(irig_date_S1, by = "date") %>%
  mutate(year_season = paste0(Year, "_", Season),
         irig_label = ifelse(date >= as.Date("2020-07-14") &
                               date <= as.Date("2020-10-08") &
                               is.na(irig_label), 3, irig_label),
         # irig_label = ifelse(date >= as.Date("2021-02-03") &
         #                       date <= as.Date("2021-05-06") &
         #                       is.na(irig_label), 3, irig_label),
         irig_label = ifelse(date >= as.Date("2021-07-05") &
                               date <= as.Date("2021-09-23") &
                               is.na(irig_label), 3, irig_label)) 


S1_irig %>%
  filter(!is.na(irig_label),
         Depth == 1,
         year_season %in% c("2020_Growing",
                            "2021_Spring", 
                            "2021_Growing")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = as.factor(irig_label))) +
  facet_grid(rows = vars(year_season),
             cols = vars(irig_label)) +
  theme_bw(base_size = 12)

S1_irig %>%
  filter(!is.na(irig_label),
         Depth == 1,
         year_season %in% c("2020_Growing",
                            "2021_Spring", 
                            "2021_Growing")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = as.factor(irig_label))) +
  scale_y_continuous(limits = c(-2, 0)) +
  facet_wrap(~year_season) +
  theme_bw(base_size = 12)

fig1a <- S1_irig %>%
  filter(Depth == 1,
         year_season %in% c("2020_Growing"),
         date <= as.Date("2020-10-28")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = date)) +
  scale_x_continuous(expression(paste(Theta, " ", m^3, " ", m^-3)),
                     limits = c(0.02, 0.125)) +
  scale_y_continuous(expression(paste(Psi, " (MPa)")),
                     limits = c(-5.5, 0)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

fig1b <- S1_irig %>%
  filter(Depth == 1,
         year_season %in% c("2021_Spring")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = date)) +
  scale_x_continuous(expression(paste(Theta, " ", m^3, " ", m^-3)),
                     limits = c(0.02, 0.125)) +
  scale_y_continuous(expression(paste(Psi, " (MPa)")),
                     limits = c(-5.5, 0)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

fig1c <- S1_irig %>%
  filter(Depth == 1,
         year_season %in% c("2021_Growing")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = date)) +
  scale_x_continuous(expression(paste(Theta, " ", m^3, " ", m^-3)),
                     limits = c(0.02, 0.125)) +
  scale_y_continuous(expression(paste(Psi, " (MPa)")),
                     limits = c(-5.5, 0)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())


plot_grid(fig1a, fig1b, fig1c,
          ncol = 1,
          labels = "AUTO",
          align = "v")

ggsave(filename = "plots/VG_curves/S1_shallow_byseason.png",
       width = 4,
       height = 8,
       units = "in")

# Isolate S4
S4 <- WPWC_daily %>%
  filter(Summer == "S4") %>%
  mutate(year_season = paste0(Year, "_", Season)) 

# Medium depth is refilled continuously and can be checked without filtering
S4 %>%
  filter(Depth == 2) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = Season)) +
  facet_wrap(~year_season)


S4 %>%
  filter(Depth == 2,
         year_season == "2020_Spring") %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = date)) 

# Shallow depth dries out, particularly prior to initial monsoon treatment
# Check all year_seasons
S4 %>%
  filter(Depth == 1) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = Season)) +
  facet_wrap(~year_season)

S4 %>%
  filter(Depth == 1,
         year_season == "2020_Growing", 
         date > as.Date("2020-08-01")) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = date)) 

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
         !is.na(pulse),
         pulse %in% c(2:4, 6:8)) %>%
  ggplot(aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = as.factor(pulse))) +
  # facet_wrap(~Year) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())
  

### Overlay all "good" periods

# S4, shallow, growing, no first pulse of season
S4_shallow_growing <- S4_rem_pulse %>%
  filter(Depth == 1,
         !is.na(pulse),
         pulse %in% c(2:4, 6:8))

# S1, shallow, early spring, growing, and fall
temp1 <- S1 %>%
  filter(year_season == "2020_Spring",
         date < as.Date("2020-03-18"),
         Depth == 1)
temp2 <- S1 %>%
  filter(year_season == "2021_Spring",
         date < as.Date("2021-03-22"),
         Depth == 1)
S1_shallow_spring <- rbind(temp1, temp2)

# S4 and S1, mid, spring 2020
both_mid_spring20 <- WPWC_daily %>%
  mutate(year_season = paste0(Year, "_", Season)) %>%
  filter(year_season == "2020_Spring",
         Depth == 2)

ggplot() +
  geom_point(data = S4_shallow_growing, 
             aes(x = WC_mean, y = WP_mean, color = "S4_shallow_growing_clean"),
             alpha = 0.25) +
  geom_point(data = S1_shallow_spring,
             aes(x = WC_mean, y = WP_mean, color = "S1_shallow_spring_early"),
             alpha = 0.25) +
  theme_bw(base_size = 12) +
  geom_point(data = filter(both_mid_spring20, Summer == "S1"),
             aes(x = WC_mean, y = WP_mean, color = "S1_mid_spring20"),
             alpha = 0.25) +
  geom_point(data = filter(both_mid_spring20, Summer == "S4"),
             aes(x = WC_mean, y = WP_mean, color = "S4_mid_spring20"),
             alpha = 0.25) 

ggplot() +
  geom_point(data = both_mid_spring20,
             aes(x = WC_mean, y = WP_mean, color = Summer),
             alpha = 0.25) 
