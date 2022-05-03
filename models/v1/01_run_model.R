# Take mid-depth SWC and SWP data and fit to standard VG curve

library(dplyr)
library(udunits2)
library(ggplot2)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)
library(broom.mixed)

# Load data
load("models/v1/out.Rdata")

dat <- filter(out, Depth == 2) %>%
  mutate(pressure_head_cm = ud.convert(WP_mean, "MPa", "cm_H2O"),
         pressure_head_m = ud.convert(pressure_head_cm, "cm", "m"))

ggplot(dat, aes(x = -pressure_head_m, 
                y = WC_mean)) +
  geom_point(aes(color = Summer)) +
  scale_x_log10(name = "Pressure Head (-m)",
                breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                labels = c(0.01, 0.1, 1, 10, 100, 1000)) +
  theme_bw()

dat_list <- list(theta = dat$WC_mean,
                 Nobs = nrow(dat),
                 theta.s = 0.44, # porosity of loamy sand
                 h.cm = -1*dat$pressure_head_cm,
                 trt = as.numeric(as.factor(dat$Summer)), # 1 = S1, 2 = S4
                 Ntrt = length(unique(dat$Summer))) 

# Function for initial values
inits <- function() {
  list(alpha.cm = runif(2, 0, 50),
       n = runif(2, 0.5, 3),
       theta.r = runif(2, 0.01, 0.05),
       sig = runif(1, 0, 10))
}
inits_list <- list(inits(), inits(), inits())

# Or, load previous saved state
load("models/v1/inits/inits.Rdata")

# Intialize model
jm <- jags.model(file = "models/v1/VG.jags",
                 data = dat_list,
                 inits = saved_state[[2]],
                 n.chains = 3)

# Update
update(jm, 1000)

# Monitor coda samples
jm_coda <- coda.samples(jm, 
                        variable.names = c("deviance",
                                           "alpha.cm", "n", "theta.r", "sig",
                                           "tau"),
                        n.iter = 150000, thin = 50)

save(jm_coda, file = "models/v1/coda/jm_coda.Rdata")

# Visualize chains
mcmcplot(jm_coda, parms = c("deviance",
                            "alpha.cm", "n", "theta.r", "sig"))

# Save state
newinits <- initfind(jm_coda, OpenBUGS = FALSE)
newinits[[1]]
saved_state <- removevars(initsin = newinits, 
                          variables = c(4))
saved_state[[1]]
save(saved_state, file = "models/v1/inits/inits.Rdata") #for local

# Check convergence
gel <- gelman.diag(jm_coda)
gel

# Plot 
caterplot(jm_coda, parms = "alpha.cm", reorder = FALSE)
caterplot(jm_coda, parms = "n", reorder = FALSE)
caterplot(jm_coda, parms = "theta.r", reorder = FALSE)


# Monitor replicated samples
jm_rep <- coda.samples(jm,
                       variable.names = "theta.rep",
                       n.iter = 150000, thin = 50)

save(jm_rep, file = "models/v1/coda/jm_rep.Rdata")
