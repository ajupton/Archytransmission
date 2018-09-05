# Functions for implementing the Eerkens and Bettinger (2008) Cultural transmission model
# in no particular order

# Morphological attribute data must be in tidy format and include a variable named "Site"
# or spatially/temporally bounded location of artifact provencance. This variable is used
# to group the artifacts. 

# Refer to the "Networks of Interaction through cultural transmission" rmarkdown document to 
# see how these functions are applied. 

library(tidyverse)

# Length with removing missing values
my_length <- function(x){
  sum(!is.na(x))
}

# Number of vessels
n_vessels <- function(x){
  x %>%
    summarise_all(my_length)
}

# Function to calculate number of vessels by site
v_vessels_by_site <- function(x){
  x %>% group_by(Site) %>% 
    summarise_all(my_length)
}

# Function to return the critical.r
critical_r <- function(n, alpha = 0.05) {
  df <- n - 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt((critical.t^2) / ( (critical.t^2) + df))
  return(critical.r)
}

# Function to check if site-attribute combination reached n-crit or replace with 0
crit_ifelse <- function(x){
  sum(ifelse(x >= 6, 1, 0))
}

# Unbiased estimator of coefficient of variation
my_cv <- function(x){
  (sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)) * (1 + (1/(4*length(x[!is.na(x)]))))
}

# Standard Deviation to remove missing values
my_sd <- function(x){
  sd(x, na.rm = TRUE)
}

# Mean function to remove missing values
my_mean <- function(x){
  mean(x, na.rm = TRUE)
}

# Variation of Variation (VOV)
VOV <- function(x){x %>%
    group_by(Site) %>%
    summarise_all(my_cv) %>%
    summarise_all(my_cv)
}

# Variation of the mean (VOM)
VOM <- function(x){x %>%
    group_by(Site) %>%
    summarise_all(my_mean) %>%
    summarise_all(my_cv)
}

# Average variation (AV)
AV <- function(x){x %>% 
    group_by(Site) %>%
    summarise_all(my_cv) %>%
    summarise_all(my_mean)
}
