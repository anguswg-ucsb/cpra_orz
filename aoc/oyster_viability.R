# create oyster viability raster

library(dplyr)
library(sf)
library(raster)
library(logger)

source("utils/utils.R")

# water raster for masking
water        <-  readRDS("data/masks/water_raster.rds")

# land polygon for masking
land         <- readRDS("data/masks/land_polygon.rds")

# read in all MP2023 files
mp_files <- list.files("data/mp2023/raster/")


#  loop though MP2023 years and calculate salinity SI for AOC and save to disk
for (i in 1:length(mp_files)) {
  
  logger::log_info("Year {mp_files[i]}")

  # read in single MP year
  mp_data <- raster::stack(paste0("data/mp2023/raster/", mp_files[i]))
  
  # calculate Oyster viability, output is raster with OV components and OV final
  ov <- get_ov(
              mp_data = mp_data, 
              land    = land, 
              mask    = TRUE
            )

  # save RDS
  saveRDS(
    ov,
    paste0("data/oyster_viability/oyster_viability_",substr(mp_files[i], 19, 20), ".rds")
  )

  # save tif
  writeRaster(
    terra::rast(ov),
    paste0("data/oyster_viability/oyster_viability_",substr(mp_files[i], 19, 20), ".tif"),
    overwrite = TRUE
  )
  # final_data_path <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"
  # writeRaster(
  #   terra::rast(ov), 
  #   paste0(final_data_path, "oyster_viability_", substr(mp_files[i], 19, 20), ".tif"),
  #   overwrite = T
  #             )
  
  # save RDS
  # saveRDS(
  #   ov,
  #   paste0("C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/oyster_viability_", substr(mp_files[3], 19, 20), ".rds")
  # )
}
 