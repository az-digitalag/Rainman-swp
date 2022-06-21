# Invert VG equation
# Run through with range of SWC and posterior parameter sets
library(dplyr)
library(coda)
library(broom.mixed)

# Define inverse function
# Input SWC is [theta.r, theta.s]
# Oputputu is SWP in MPa
SWC_to_SWP <- function(SWC, alpha.cm, n, theta.r) {
  # Applicable specifically to loamy sand with theta.s of 0.44
  num <- log((0.44 - theta.r) / (SWC - theta.r)) / (1 - 1/n)
  under <- (exp(num) - 1) / (alpha.cm^n)
  SWP_cm <- under^(1/n)
  SWP_MPa = ud.convert(SWP_cm, "cm_H2O", "MPa")
  return(SWP_MPa)
  suppressWarnings()
}

# Load parameters
load("models/v2/coda/jm_coda.Rdata")

# Calculate posterior means
sum_param <- tidyMCMC(jm_coda, 
                      conf.int =  TRUE, 
                      conf.method = "HPDinterval",
                      conf.level = 0.95)
pop <- sum_param %>%
  filter(grepl("E\\.", term)) %>%
  mutate(parameter = sub("\\[[0-9]\\]", "", term),
         parameter = sub("E\\.", "", parameter))

trt <- sum_param %>%
  filter(grepl("^alpha\\.cm", term) |
           grepl("^n\\[", term) |
           grepl("^theta.r" , term)) %>%
  mutate(parameter = sub("\\[[0-9]\\]", "", term),
         Summer = case_when(grepl("1", term) ~ "S1",
                            grepl("2", term) ~ "S4"))

# Test single curve with posterior means
test <- data.frame(swc_in = seq(0.04, 0.44, .0001))
test$out_ph_MPa <- SWC_to_SWP(test$swc_in, pop$estimate[1], pop$estimate[2], pop$estimate[3])

ggplot(test, aes(x = out_ph_MPa,
                 y = swc_in)) +
  geom_point() +
  scale_x_log10(name = "Pressure Head (-MPa)",
                            breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                            labels = c(0.01, 0.1, 1, 10, 100, 1000, 10000))

# Apply for each of 3000
# Assemble site-level and treatment-level parameters
coda_param <- rbind.data.frame(jm_coda[[1]], jm_coda[[2]], jm_coda[[3]]) %>%
  select(starts_with("E"))

coda_param_S1 <- rbind.data.frame(jm_coda[[1]], jm_coda[[2]], jm_coda[[3]]) %>%
  select(filter(trt, Summer == "S1")$term)

coda_param_S4 <- rbind.data.frame(jm_coda[[1]], jm_coda[[2]], jm_coda[[3]]) %>%
  select(filter(trt, Summer == "S4")$term)

# Custom functions
swc_apply <- function(vec, SWC) { # vector of parameters, (alpha.cm, n, theta.r)
  # Applicable specifically to loamy sand with theta.s of 0.44
  num <- log((0.44 - vec[3]) / (SWC - vec[3])) / (1 - 1/vec[2])
  under <- (exp(num) - 1) / (vec[1]^vec[2])
  SWP_cm <- under^(1/vec[2])
  SWP_MPa = ud.convert(SWP_cm, "cm_H2O", "MPa")
  return(SWP_MPa)
  suppressWarnings()
}
cnt <- function(x) {sum(!is.na(x))}

# Calculate swp for all sets of parameters
out <- apply(coda_param, MARGIN = 1, FUN = swc_apply,
             SWC = seq(0.04, 0.44, .0001))

out_S1 <- apply(coda_param_S1, MARGIN = 1, FUN = swc_apply,
             SWC = seq(0.04, 0.44, .0001))

out_S4 <- apply(coda_param_S4, MARGIN = 1, FUN = swc_apply,
                SWC = seq(0.04, 0.44, .0001))

# Summarize to number, median, and central 50th percentile
out_df <- cbind.data.frame(SWC  = seq(0.04, 0.44, .0001),
                           n = apply(out, 1, FUN = cnt),
                           SWP_MPa_50 = apply(out, 1, FUN = median, na.rm = TRUE),
                           SWP_MPa_25 = apply(out, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           SWP_MPa_75 = apply(out, 1, FUN = quantile, probs = 0.75, na.rm = TRUE),
                           S1_MPa_50 = apply(out_S1, 1, FUN = median, na.rm = TRUE),
                           S1_MPa_25 = apply(out_S1, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           S1_MPa_75 = apply(out_S1, 1, FUN = quantile, probs = 0.75, na.rm = TRUE),
                           S4_MPa_50 = apply(out_S4, 1, FUN = median, na.rm = TRUE),
                           S4_MPa_25 = apply(out_S4, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           S4_MPa_75 = apply(out_S4, 1, FUN = quantile, probs = 0.75, na.rm = TRUE)) %>%
  filter(SWC < 0.44)

# Plot with error
out_df %>%
  filter(n > 1000) %>%
  ggplot() +
  geom_errorbar(aes(x = SWC,
                    ymin = SWP_MPa_25,
                    ymax = SWP_MPa_75),
                width = 0,
                alpha = 0.05,
                color = "forestgreen") +
  geom_point(aes(x = SWC,
                 y = SWP_MPa_50),
             color = "forestgreen") +
  # geom_point(data = pred,
  #            aes(x = WC_mean,
  #                y = -WP_mean),
  #            color = "black") +
  scale_y_log10(name = "SWP (-MPa)") +
  theme_bw(base_size = 14)

#### Create lookup table based on predictions with >= 1000 observations
# Include the median and central 50th percentile

lookup <- out_df %>%
  mutate(SWP_MPa_25 = case_when(n < 1000 ~ as.numeric(NA),
                                n >= 1000 ~ -1 * SWP_MPa_25),
         SWP_MPa_50 = case_when(n < 1000 ~ as.numeric(NA),
                                n >= 1000 ~ -1 * SWP_MPa_50),
         SWP_MPa_75 = case_when(n < 1000 ~ as.numeric(NA),
                                n >= 1000 ~ -1 * SWP_MPa_75),
         S1_MPa_50 = case_when(n < 1000 ~ as.numeric(NA),
                               n >= 1000 ~ -1 * S1_MPa_50),
         S1_MPa_25 = case_when(n < 1000 ~ as.numeric(NA),
                               n >= 1000 ~ -1 * S1_MPa_25),
         S1_MPa_75 = case_when(n < 1000 ~ as.numeric(NA),
                               n >= 1000 ~ -1 * S1_MPa_75),
         S4_MPa_50 = case_when(n < 1000 ~ as.numeric(NA),
                               n >= 1000 ~ -1 * S4_MPa_50),
         S4_MPa_25 = case_when(n < 1000 ~ as.numeric(NA),
                               n >= 1000 ~ -1 * S4_MPa_25),
         S4_MPa_75 = case_when(n < 1000 ~ as.numeric(NA),
                               n >= 1000 ~ -1 * S4_MPa_75))

# Save
save(lookup, file = "models/v2/source/lookup.Rdata")

  
# Load original input data
load("models/v2/out.Rdata")

dat <- filter(out, Depth == 2) %>%
  mutate(pressure_head_cm = ud.convert(WP_mean, "MPa", "cm_H2O"),
         pressure_head_m = ud.convert(pressure_head_cm, "cm", "m"))

foo <- head(dat$WC_mean, 1)
lookup$SWP_MPa_25[which.min(abs(lookup$SWC - foo))]




get_SWP_MPa <- function(SWC, param = "site", stat = "median") {
  if(min(SWC) < 0.0439 | max(SWC) >= 0.44){ 
    print("SWC out of range")
  } 
  
  if(param == "site") {
    SWP <- lookup[get_inds(SWC), 3:5]
  } else if (param == "S1") {
    SWP <- lookup[get_inds(SWC), 6:8]
  } else if (param == "S4") {
    SWP <- lookup[get_inds(SWC), 9:11]
  }

  if(stat == "median") {
    return(SWP[,1])
  } else if (stat == "lower") {
    return(SWP[,2])
  } else if (stat == "upper") {
    return(SWP[,3])
  }
  
  get_ind <- function(swc) {
    which.min(abs(lookup$SWC - swc))
  }
  get_inds <- Vectorize(get_ind)
}

# Test performance of site-level predictions
dat$test_WP <- get_SWP_MPa(SWC = dat$WC_mean, stat = "median")
dat$test_WP_25 <- get_SWP_MPa(SWC = dat$WC_mean, stat = "lower")
dat$test_WP_75 <- get_SWP_MPa(SWC = dat$WC_mean, stat = "upper")

dat %>%
  ggplot(aes(y = WC_mean)) +
  geom_point(aes(x = abs(test_WP),
                 col = "predicted")) +
  geom_errorbarh(aes(xmin = abs(test_WP_25),
                     xmax = abs(test_WP_75),
                     col = "predicted"),
                 alpha = 0.25) +
  geom_point(aes(x = abs(WP_mean),
                 col = "observed",
                 shape = Summer)) +
  scale_x_log10(name = "Soil WP (-MPa)") +
  theme_bw(base_size = 14)

# Test performance of treatment-level predictions
for(i in 1:nrow(dat)) {
  dat$test_WP[i] <- get_SWP_MPa(SWC = dat$WC_mean[i], param = dat$Summer[i], stat = "median")
  dat$test_WP_25[i] <- get_SWP_MPa(SWC = dat$WC_mean[i], param = dat$Summer[i], stat = "lower")
  dat$test_WP_75[i] <- get_SWP_MPa(SWC = dat$WC_mean[i], param = dat$Summer[i], stat = "upper")
}

dat %>%
  ggplot(aes(y = WC_mean)) +
  geom_point(aes(x = abs(test_WP),
                 col = "predicted")) +
  geom_errorbarh(aes(xmin = abs(test_WP_25),
                     xmax = abs(test_WP_75),
                     col = "predicted"),
                 alpha = 0.5) +
  geom_point(aes(x = abs(WP_mean),
                 col = "observed")) +
  scale_x_log10(name = "Soil WP (-MPa)") +
  facet_wrap(~Summer) +
  theme_bw(base_size = 14)


ggplot(dat, aes(x = abs(WP_mean), y = abs(test_WP))) +
  geom_point(aes(color = Summer)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_equal()+
  scale_x_log10(name = "Observed WP (-MPa)") +
  scale_y_log10(name = "Predicted WP (-MPa)") 

