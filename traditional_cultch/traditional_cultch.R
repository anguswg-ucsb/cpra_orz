# Angus Watters
# Lynker
# Traditional Cultch Model for Oyster Resource Zones
# TRADITIONAL CULTCH MODEL

# The traditional cultch model to be used in development of oyster resource zones generally follows the same structure as the eastern oyster habitat suitability index model used for the 2023 Coastal Master plan with the exception that the cultch variable has been removed. See the Habitat Suitability Index Model Improvements technical report available at https://coastal.la.gov/wp-content/uploads/2021/04/HSI-Model-Improvements_April2021.pdf for more details. The specific relationships for each variable have been extracted from the above report.

# HSI = (SI2 x SI3 x SI4 x SI5 x SI6)^1/5

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)

source("utils/utils.R")

crs <- CRS('+init=EPSG:26915')

# read in all MP2023 files
mp_files     <- paste0("data/mp2023/raster/",list.files("data/mp2023/raster/"))

# Sediment Deposition 10 year mean
sed_dep      <- readRDS("data/sediment_dep/sediment_deposition.rds")

# land polygon for masking
land         <- readRDS("data/masks/land_polygon.rds")

# Iterate through each MP year and calculate Traditional Cultch model, then save to disk
for (i in 1:length(mp_files)) {
  
  logger::log_info("Calculating traditional cultch year {i}")

  # Load data for a MP2023 year
  mp      <- raster::stack(mp_files[i])
  
  # Calculate HSI for MP year and return a raster stack with SI components and HSI
  mp_hsi  <- get_trad_cultch(
    mp      = mp,
    sed_dep = sed_dep
  )

  mp_hsi <-  raster::stack(mp_hsi) %>% 
    mask(land, inverse = T) %>% 
    round(4) %>%
    terra::rast()
  
  # save raster
  writeRaster(
    mp_hsi,
    paste0("data/traditional_cultch/traditional_cultch_", substr(mp_files[i], 38, 39), ".tif"),
    overwrite = T
  )
  
}

trad_cultch_files <- paste0("data/traditional_cultch/", list.files("data/traditional_cultch/"))

# empty lists to iteratively add to 
tc_lst  <- list()

for (i in 1:length(trad_cultch_files)) {
  
  logger::log_info("reading {trad_cultch_files[i]}")
  
  r <- raster::stack(trad_cultch_files[i])
  
  tc_lst[[i]]  <- r$hsi
 
}

# Traditional cultch 10 year HSI mean
trad_cultch_mean <- raster::stack(tc_lst) %>% 
  calc(mean) %>%
  setNames(c("trad_cultch_mean"))

# Traditional cultch 10 year HSI standard deviation
trad_cultch_sd <- raster::stack(tc_lst) %>% 
  calc(sd) %>%
  setNames(c("trad_cultch_sd"))

# save ORS mean
writeRaster(
  trad_cultch_mean,
  "data/oyster_resource_suitability/traditional_cultch_mean.tif",
  overwrite = T
)

# save ORS standard deviation
writeRaster(
  trad_cultch_sd,
  "data/oyster_resource_suitability/traditional_cultch_standard_dev.tif",
  overwrite = T
)


rm(mp, mp_hsi, sed_dep)









