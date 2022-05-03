# Plot model fit and parameters

library(broom.mixed)
library(dplyr)
library(ggplot2)

# Load data 
load("models/v1/out.Rdata")

dat <- filter(out, Depth == 2) %>%
  mutate(pressure_head_cm = ud.convert(WP_mean, "MPa", "cm_H2O"),
         pressure_head_m = ud.convert(pressure_head_cm, "cm", "m"))

# Load codas
load(file = "models/v1/coda/jm_coda.Rdata")
load(file = "models/v1/coda/jm_rep.Rdata")

# Tidy parameters
sum_param <- tidyMCMC(jm_coda, 
                      conf.int =  TRUE, 
                      conf.method = "quantile")

# Tidy replicated and add to original data
sum_rep <- tidyMCMC(jm_rep, 
                    conf.int =  TRUE, 
                    conf.method = "quantile")

pred <- cbind.data.frame(dat, sum_rep)


#### Plot model fit ####
m1 <- lm(WC_mean ~ estimate, data = pred)
summary(m1) # R2 = 0.98

# Observed vs. fitted
pred %>%
  ggplot(aes(x = WC_mean, y = estimate)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high,
                      col = Summer)) +
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
  facet_wrap(~Summer) +
  scale_color_manual()
scale_x_log10(name = "Pressure Head (-m)",
              breaks = c(0.01, 0.1, 1, 10, 100, 1000),
              labels = c(0.01, 0.1, 1, 10, 100, 1000)) +
  theme_bw()

#### Plot model parameters ####

sum_param %>%
  filter() %>%
  