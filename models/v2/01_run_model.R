# Take mid-depth SWC and SWP data and fit to standard VG curve
# Hierarchical model to account for both treatments

library(dplyr)
library(udunits2)
library(ggplot2)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)
library(broom.mixed)

# Load data
load("models/v2/out.Rdata")

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
                 Ntrt = length(unique(dat$Summer)),
                 Salpha = 0.5,
                 Sn = 0.5,
                 Stheta.r = 0.1) 

# Function for initial values
inits <- function() {
  list(mu.log.alpha = runif(1, -5, 0),
       mu.log.n = runif(1, -1, 2),
       mu.log.theta.r = runif(1, -5, -2),
       tau = runif(1, 0, 1),
       tau.eps.alpha = runif(1, 0, 50),
       tau.eps.n = runif(1, 0, 20),
       tau.eps.theta.r = runif(1, 0, 10000))
}
inits_list <- list(inits(), inits(), inits())

# Or, load previous saved state
load("models/v2/inits/inits.Rdata")

# Make new 
# mean(jm_coda[[1]][,4])
# mean(jm_coda[[2]][,4])
# mean(jm_coda[[3]][,4])
# inits_list <- list(saved_state[[2]][[1]],
#                    saved_state[[2]][[3]],
#                    saved_state[[2]][[3]])

# Intialize model
jm <- jags.model(file = "models/v2/VG_hierarchy.jags",
                 data = dat_list,
                 inits = saved_state[[2]],
                 n.chains = 3)

# Update
# update(jm, 1000)

# Monitor coda samples
jm_coda <- coda.samples(jm, 
                        variable.names = c("deviance",
                                           "E.alpha.cm", "E.n", "E.theta.r", # population-level parameters on normal scale
                                           "mu.log.alpha", "mu.log.n", "mu.log.theta.r", # populatio-level parameters on log scale, needed to reinitialize
                                           "sig", "tau", # sample variation, sig needed to reinitialize
                                           "sig.alpha", "sig.n", "sig.theta.r", # sigma on log scale
                                           "tau.eps.alpha", "tau.eps.n", "tau.eps.theta.r", # needed to reinitialize
                                           "alpha.cm", "n", "theta.r"), # treatment-level parameters, normal scale
                        n.iter = 150000, thin = 150)

# save(jm_coda, file = "models/v2/coda/jm_coda.Rdata")

# Visualize chains
mcmcplot(jm_coda, parms = c("deviance",
                            "sig", 
                            "sig.alpha", "sig.n", "sig.theta.r",
                            "E.alpha.cm", "mu.log.alpha",
                            "E.n", "mu.log.n",
                            "E.theta.r", "mu.log.theta.r"))
caterplot(jm_coda, parms = c("sig.alpha",
                             "sig.n",
                             "sig.theta.r"), reorder = FALSE)
caterplot(jm_coda, parms = c("mu.log.alpha",
                             "mu.log.n",
                             "mu.log.theta.r"), reorder = FALSE)
caterplot(jm_coda, parms = c("E.alpha.cm",
                             "alpha.cm"), reorder = FALSE)
caterplot(jm_coda, parms = c("E.n",
                             "n"), reorder = FALSE)
caterplot(jm_coda, parms = c("E.theta.r",
                             "theta.r"), reorder = FALSE)
# Save state
newinits <- initfind(jm_coda, OpenBUGS = FALSE)
newinits[[1]]
saved_state <- removevars(initsin = newinits, 
                          variables = c(1:4, 8:15, 20))
saved_state[[1]]
save(saved_state, file = "models/v2/inits/inits.Rdata") #for local

# Check convergence
gel <- gelman.diag(jm_coda, multivariate = FALSE)
gel

# Monitor replicated samples
jm_rep <- coda.samples(jm,
                       variable.names = "theta.rep",
                       n.iter = 150000, thin = 150)

save(jm_rep, file = "models/v2/coda/jm_rep.Rdata")
