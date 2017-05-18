# Packages
library(tidyverse)

# R Function for Task 1
# Derive the mode in a stata friendly fashion
statamode <- function(x) {
  z <- table(as.vector(x))
  m <- suppressMessages(suppressWarnings(names(z)[z == max(z)]))
  if(length(m)==1){
    return(m)
  }
  return(".")
}

# distinct values function
nvals <- function(x){
  length(unique(x))
}

# Replace all missing values in a vector with a numeric 0
zeroNA <- function(x){
  x[is.na(x)] <- 0
  return(x)
}

# Cluster standard errors
get_CL_vcov <- function(model, cluster){
  # cluster is an actual vector of clusters from data passed to model
  # from: http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  cluster <- as.character(cluster)
  # calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  # calculate the uj's
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  # use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

# Convert SATtoACT
# x is a vector of act scores
ACTtoSAT <- function(x){
  x[is.na(x)] <- 400
  x[x  <  11] <- 400
  x[x == 11] <- 530
  x[x == 12] <- 590
  x[x == 13] <- 640
  x[x == 14] <- 690
  x[x == 15] <- 740
  x[x == 16] <- 790
  x[x == 17] <- 830
  x[x == 18] <- 870
  x[x == 19] <- 910
  x[x == 20] <- 950
  x[x == 21] <- 990
  x[x == 22] <- 1030
  x[x == 23] <- 1070
  x[x == 24] <- 1110
  x[x == 25] <- 1150
  x[x == 26] <- 1190
  x[x == 27] <- 1220
  x[x == 28] <- 1260
  x[x == 29] <- 1300
  x[x == 30] <- 1340
  x[x == 31] <- 1340
  x[x == 32] <- 1420
  x[x == 33] <- 1460
  x[x == 34] <- 1510
  x[x == 35] <- 1560
  x[x == 36] <- 1600
  return(x)
}

pkgTest <- function(x){
  if(x != "OpenSDPsynthR"){
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  } else{
    if (!require(x,character.only = TRUE))
    {
      devtools::install_github("opensdp/OpenSDPsynthR")
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
}