### Function to convert SWC into SWP via a lookup table
# Lookup table encodes the inverted Van Genuchten equation with parameters alpha, n, and theta.residual
# Inputs are SWC, param, and stat
# SWC should be a scalar or vector of volumetric soil water content expressed as proportion (0,1)
# param indicates which VG parameters should be used: "site", "S1", or "S4"
# stat indicates which SWP should be output: "median", "lower", or "upper"
# Note: "lower" and "upper" denote the central 50th percentile
# Note: function will return "SWC out of range" message and NA as output if SWC < 0.0439 or SWC >= 0.44
# Predictions are unstable at the lower end as theta.residual was estimated from the model
# Used criteria of at least 1000 predictions at that SWC value

# Load lookup table
if(file.exists("source/lookup.Rdata")) {
  load("source/lookup.Rdata")
} else {
  load("../../source/lookup.Rdata")
}


swc2swp <- function(SWC, param = "site", stat = "median") {
  if(min(SWC) < 0.0439 | max(SWC) >= 0.44){ # based on n > = 1000
    warning("SWC out of range")
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
}

get_ind <- function(swc) {
  which.min(abs(lookup$SWC - swc))
}
get_inds <- Vectorize(get_ind)
