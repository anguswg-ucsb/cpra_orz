# generate AOC layers

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(terra)

source("utils/utils.R")

# ---- AOC HSI model ----

# read in all MP2023 files
ov_files <- list.files("data/oyster_viability/", pattern = ".tif")

cv       <- raster::stack("data/commercial_viability/commercial_viability.tif")


# Loop through Oyster viability rasters and calculate/save AOC layers for all 10 years of MP data
for (i in 1:length(ov_files)) {
  logger::log_info("Calculating AOC - year {i}")

  # read in MP2023 year
  ov <- raster::stack(paste0("data/oyster_viability/", ov_files[i]))

  aoc <- get_aoc(
    shallow_cv   = cv$shallow_cv, 
    deep_cv      = cv$deep_cv, 
    combine_cv   = cv$cv,
    ov           = ov$si_ov
  )

  
  writeRaster(
            aoc,
            paste0("data/aoc/aoc_", substr(ov_files[i], 18, 19), ".tif"),
            overwrite = T
            )

  # save RDS
  # saveRDS(
  #   stack(aoc),
  #   paste0("C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/aoc_", substr(ov_files[i], 18, 19), ".rds")
  # )
}

aoc_files <- paste0("data/aoc/", list.files("data/aoc/"))

# empty lists to iteratively add to 
shallow_lst  <- list()
deep_lst     <- list()
combine_lst  <- list()

for (i in 1:length(aoc_files)) {
  
  logger::log_info("reading {aoc_files[i]}")
  
  r <- raster::stack(aoc_files[i])
  
  shallow_lst[[i]]  <- r$aoc_shallow
  deep_lst[[i]]     <- r$aoc_deep
  combine_lst[[i]]  <- r$aoc_combine
}

# Calculate 10 year mean AOC 
shallow_mean <- stack(shallow_lst)%>%
  setNames(c(paste0("aoc_shallow_", 1:10))) %>% 
  calc(mean) %>%
  setNames(c("shallow_aoc_mean"))

deep_mean <- stack(deep_lst)%>%
  setNames(c(paste0("aoc_deep_", 1:10))) %>% 
  calc(mean) %>%
  setNames(c("deep_aoc_mean"))

combine_mean <- stack(combine_lst)%>%
  setNames(c(paste0("aoc_combine_", 1:10))) %>% 
  calc(mean) %>%
  setNames(c("combine_aoc_mean"))

# Standard deviation 
shallow_sd <- stack(shallow_lst)%>%
  setNames(c(paste0("aoc_shallow_", 1:10))) %>% 
  calc(sd) %>%
  setNames(c("shallow_aoc_sd"))

deep_sd <- stack(deep_lst)%>%
  setNames(c(paste0("aoc_deep_", 1:10))) %>% 
  calc(sd) %>%
  setNames(c("deep_aoc_sd"))

combine_sd <- stack(combine_lst)%>%
  setNames(c(paste0("aoc_combine_", 1:10))) %>% 
  calc(sd) %>%
  setNames(c("combine_aoc_sd"))

# Raster stack of AOC means
mean_st <- raster::stack(shallow_mean, deep_mean, combine_mean) %>%
  # setNames(c("aoc_shallow_mean", "aoc_deep_mean")) %>% 
  terra::rast()


# Raster stack of AOC standard deviations
sd_st   <- raster::stack(shallow_sd, deep_sd, combine_sd) %>%
  # setNames(c("aoc_shallow_sd", "aoc_deep_sd")) %>% 
  terra::rast()


# writeRaster(mean_st, "data/oyster_resource_suitability/aoc_mean.tif", overwrite = T)
# writeRaster(sd_st, "data/oyster_resource_suitability/aoc_standard_dev.tif", overwrite = T)
# saveRDS(stack(mean_st), "C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/aoc_mean.rds")
# saveRDS(stack(sd_st), "C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/aoc_sd.rds")


rm(combine_st, aoc_lst, aoc, shallow_st, ov, r, cv, deep_st, combine_mean, deep_mean, shallow_mean, shallow_sd, deep_sd, combine_sd, mean_st, sd_st, shallow_lst, deep_lst, combine_lst, aoc_files, i, ov_files)










