# To develop moisture release curves
# Extract daily SWC and SWP 
# Remove outliers as needed by depth and pulse

library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(cowplot)
library(udunits2)

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

# Extract S1 and S4 middle depths in 2020, winter and spring
mid_20_spring <- WPWC_daily %>%
  filter(Year == 2020,
         Depth == 2,
         Season %in% c("Winter", "Spring")) %>%
  filter(date >= as.Date("2019-12-01"))

ggplot(mid_20_spring, aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = Summer, shape = "25 cm"))

# For shallow depths, obtain S1 early spring and S4 cleaned growing

# S1
S1 <- WPWC_daily %>%
  filter(Summer == "S1")%>%
  mutate(year_season = paste0(Year, "_", Season))

temp1 <- S1 %>%
  filter(year_season == "2020_Spring",
         date < as.Date("2020-03-18"),
         Depth == 1)
temp2 <- S1 %>%
  filter(year_season == "2021_Spring",
         date < as.Date("2021-03-22"),
         Depth == 1)
S1_shallow_spring <- rbind(temp1, temp2)

ggplot(S1_shallow_spring, aes(x = WC_mean, y = WP_mean)) +
  geom_point(aes(color = year_season, shape = "5 cm"))

# S4
S4 <- WPWC_daily %>%
  filter(Summer == "S4") %>%
  mutate(year_season = paste0(Year, "_", Season)) 

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

# S4, shallow, growing, no first pulse of season
S4_shallow_growing <- S4_rem_pulse %>%
  filter(Depth == 1,
         !is.na(pulse),
         pulse %in% c(2:4, 6:8))


# Plot all together
ggplot() +
  geom_point(data = S4_shallow_growing, 
             aes(x = WC_mean, y = WP_mean, color = "S4_shallow_growing_clean"),
             alpha = 0.25) +
  geom_point(data = S1_shallow_spring,
             aes(x = WC_mean, y = WP_mean, color = "S1_shallow_spring_early"),
             alpha = 0.25) +
  theme_bw(base_size = 12) +
  geom_point(data = filter(mid_20_spring, Summer == "S1"),
             aes(x = WC_mean, y = WP_mean, color = "S1_mid_spring20"),
             alpha = 0.25) +
  geom_point(data = filter(mid_20_spring, Summer == "S4"),
             aes(x = WC_mean, y = WP_mean, color = "S4_mid_spring20"),
             alpha = 0.25) 


# Combine and save out

# Shallow S4 is growing season of 2020 and 2021
# removing day of pulse up to 2 days after
# and first pulse of season
dim(S4_shallow_growing) 

# Shallow S1 is spring season of 2020 and 2021
# from 2/1 to 3/17 (2020) or 3/21 (2021),
# when top layer is more uniformly wet
dim(S1_shallow_spring)

# Mid S1 and S4 are from the winter/spring season of 2020, which was wetter than in 2021
dim(mid_20_spring)

cnames <- intersect(intersect(colnames(S4_shallow_growing),
                              colnames(S1_shallow_spring)),
                    colnames(mid_20_spring))

out <- rbind(S4_shallow_growing[cnames],
             S1_shallow_spring[cnames],
             mid_20_spring[cnames])

save(out, file = "models/v1/out.Rdata")

# Plot in comparison to Fig. 3.4A from PNNL document
out <- out %>%
  mutate(pressure_head_cm = ud.convert(WP_mean, "MPa", "cm_H2O"),
         pressure_head_m = ud.convert(pressure_head_cm, "cm", "m"))

ggplot(out, aes(x = -pressure_head_m, y = WC_mean, 
                color = Summer)) +
  geom_point() +
  scale_x_log10(name = "Pressure Head (-m)",
                breaks = c(1, 10, 100)) +
  facet_grid(rows = vars(Depth)) +
  theme_bw()

ggplot(filter(out, Depth ==2)) +
  geom_histogram(aes(x = -pressure_head_m)) +
  scale_x_log10()
