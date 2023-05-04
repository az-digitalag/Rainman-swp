# Plot model fit and parameters

library(coda)
library(broom.mixed)
library(udunits2)
library(dplyr)
library(ggplot2)

# Load data 
load("models/v3/out.Rdata")

dat <- out %>%
  mutate(pressure_head_cm = ud.convert(WP_mean, "MPa", "cm_H2O"),
         pressure_head_m = ud.convert(pressure_head_cm, "cm", "m"),
         TRT = paste(Depth, Summer, sep = "_"),
         TRT = factor(TRT, levels = c("1_S1", "2_S1", "1_S4", "2_S4")))

# Load codas
load(file = "models/v3/coda/jm_coda.Rdata")
load(file = "models/v3/coda/jm_rep.Rdata")

# Tidy parameters
sum_param <- tidyMCMC(jm_coda, 
                      conf.int =  TRUE, 
                      conf.method = "HPDinterval",
                      conf.level = 0.95)

# Tidy replicated and add to original data
sum_rep <- tidyMCMC(jm_rep, 
                    conf.int =  TRUE, 
                    conf.method = "HPDinterval")

pred <- cbind.data.frame(dat, sum_rep)


#### Plot model fit ####
m1 <- lm(WC_mean ~ estimate, data = pred)
summary(m1) # R2 = 0.984

# Observed vs. fitted
pred %>%
  ggplot(aes(x = WC_mean, y = estimate)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high,
                      col = Summer,
                      shape = factor(Depth))) +
  facet_grid(cols = vars(Summer),
             rows = vars(Depth)) +
  theme_bw()

# VG curve observed and fitted
pred %>% 
  ggplot(aes(x = -pressure_head_m)) +
  geom_point(aes(y = WC_mean, 
                 color = "Observed")) +
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = "Predicted")) +
  facet_grid(cols = vars(Summer),
             rows = vars(Depth)) +
  # scale_color_manual() +
  scale_x_log10(name = "Pressure Head (-m)",
              breaks = c(0.01, 0.1, 1, 10, 100, 1000),
              labels = c(0.01, 0.1, 1, 10, 100, 1000)) +
  theme_bw()

#### Calculate R2, coverage, and bias for each TRT
pred2 <- pred %>%
  mutate(cover = if_else(WC_mean >= conf.low & WC_mean <= conf.high, 1, 0))

# total data frame
tot.df <- data.frame(type = "all",
                     R2 = summary(m1)$adj.r.squared,
                     coverage = mean(pred2$cover),
                     bias = summary(m1)$coef[2,1])

# for each TRT
trt.df <- data.frame(type = levels(pred2$TRT),
                     R2 = pred2 %>%
                       group_by(TRT) %>%
                       group_map(~ summary(lm(estimate ~ WC_mean, data = .x))$adj.r.squared) %>%
                       purrr:::simplify(),
                     coverage = pred2 %>%
                       group_by(TRT) %>%
                       summarize(coverage = mean(cover)) %>%
                       pull(coverage),
                     bias = pred2 %>%
                       group_by(TRT) %>%
                       group_map(~ summary(lm(estimate ~ WC_mean, data = .x))$coef[2,1]) %>%
                       purrr:::simplify())

all.df <- bind_rows(trt.df, tot.df)

#### Plot model parameters ####

pop <- sum_param %>%
  filter(grepl("E\\.", term)) %>%
  mutate(parameter = sub("\\[[0-9]\\]", "", term),
         parameter = sub("E\\.", "", parameter))

trt <- sum_param %>%
  filter(grepl("^alpha\\.cm", term) |
           grepl("^n\\[", term) |
           grepl("^theta.r" , term)) %>%
  mutate(parameter = sub("\\[[0-9]\\]", "", term),
         Treatment = case_when(grepl("1", term) ~ "S1",
                               grepl("2", term) ~ "S1",
                               grepl("3", term) ~ "S4",
                               grepl("4", term) ~ "S4"),
         Depth = case_when(grepl("1", term) ~ "1",
                           grepl("2", term) ~ "2",
                           grepl("3", term) ~ "1",
                           grepl("4", term) ~ "2"),
         TRT = case_when(grepl("1", term) ~ "1_S1",
                         grepl("2", term) ~ "2_S1",
                         grepl("3", term) ~ "1_S4",
                         grepl("4", term) ~ "2_S4"),
         parameter = factor(parameter, levels = c("theta.r", "alpha.cm", "n")))

ggplot() +
  geom_pointrange(data = trt,
                  aes(x = parameter,
                      y = estimate,
                      ymin = conf.low, 
                      ymax = conf.high,
                      color = TRT),
                  position = position_dodge(width = 1)) +
  geom_pointrange(data = pop,
                  aes(x = parameter,
                      y = estimate,
                      ymin = conf.low, 
                      ymax = conf.high)) +
  facet_wrap(~parameter, scales = "free") +
  theme_bw(base_size = 14) 

sum_param %>%
  mutate(parameter = sub("\\[[0-9]\\]", "", term),
         TRT = case_when(grepl("1", term) ~ "1_S1",
                         grepl("2", term) ~ "2_S1",
                         grepl("3", term) ~ "1_S4",
                         grepl("4", term) ~ "2_S4"),
         parameter = factor(parameter, levels = c("theta.r", "alpha.cm", "n"))) %>%
  filter(parameter %in% c("alpha.cm", "n", "theta.r")) %>%
  ggplot(aes(x = TRT, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high)) +
  scale_y_continuous("Posterior estimate") +
  facet_wrap(~parameter, scales = "free") +
  theme_bw(base_size = 14)  
  