# Angus Watters
# Lynker
# Traditional Cultch Model for Oyster Resource Zones
# TRADITIONAL CULTCH MODEL
#
# The traditional cultch model to be used in development of oyster resource zones generally follows the same structure as the eastern oyster habitat suitability index model used for the 2023 Coastal Master plan with the exception that the cultch variable has been removed. See the Habitat Suitability Index Model Improvements technical report available at https://coastal.la.gov/wp-content/uploads/2021/04/HSI-Model-Improvements_April2021.pdf for more details. The specific relationships for each variable have been extracted from the above report.

# HSI = (SI2 x SI3 x SI4 x SI5 x SI6)^1/5
# SI2 = Mean salinity during the spawning season, April through November

# SI2 = 0.0, when V2 < 5 ppt
#
# (0.06*V2) – 0.3, when 5 ≤ V2 < 10
#
# (0.07*V2) – 0.4, when 10 ≤ V2 < 15
#
# (0.1167*V2) – 1.1, when 15 ≤ V2 < 18
#
# 1.0, when 18 ≤ V2 < 22
#
# (-0.0875*V2) + 2.925, when 22 ≤ V2 < 30
#
# (-0.04*V2) + 1.5, when 30 ≤ V2 < 35
#
# (-0.02*V2) + 0.8, when 35 ≤ V2 <

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(progress)
library(leaflet)
library(viridisLite)

source("utils/utils.R")

crs <- CRS('+init=EPSG:26915')


# read in all MP2023 files
mp_files <- paste0("data/mp2023/raster/",list.files("data/mp2023/raster/"))

# Sediment Deposition 10 year mean
sed_dep <- readRDS("data/sediment_dep/sediment_deposition.rds")

# land polygon for masking
land         <- readRDS("data/masks/land_polygon.rds")
# land_r         <- readRDS("data/masks/land_raster.rds")
# resamp_r <- raster(
#   nrows = 452,
#   ncols = 1051,
#   crs = CRS('+init=EPSG:26915'),
#   ext = extent(405220, 909700, 3199570, 3416530)
# )
# 
# # resample depth values
# land_r   <- resample(land_r, resamp_r)
# plot(mp_hsi2$hsi)
# mp      <- raster::stack(mp_files[1])

# mp_hsi <- get_trad_cultch(mp = mp, sed_dep = sed_dep)

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
  
  # final_data_path <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"
  # writeRaster(
  #   mp_hsi, 
  #   paste0(final_data_path, "traditional_cultch_", substr(mp_files[i], 38, 39), ".tif"),
  #   overwrite = T
  #   )
  
  # save RDS
  # saveRDS(
  #   stack(mp_hsi),
  #   paste0("C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/traditional_cultch_", substr(mp_files[i], 38, 39), ".rds")
  # )
  
}

trad_cultch_files <- paste0("data/traditional_cultch/", list.files("data/traditional_cultch/"))

# empty lists to iteratively add to 
tc_lst  <- list()

for (i in 1:length(trad_cultch_files)) {
  
  logger::log_info("reading {trad_cultch_files[i]}")
  
  r <- raster::stack(trad_cultch_files[i])
  
  tc_lst[[i]]  <- r$hsi
 
}

trad_cultch_mean <- raster::stack(tc_lst) %>% 
  calc(mean) %>%
  setNames(c("trad_cultch_mean"))

trad_cultch_sd <- raster::stack(tc_lst) %>% 
  calc(sd) %>%
  setNames(c("trad_cultch_sd"))

# save rasters to project
writeRaster(
  trad_cultch_mean,
  "data/oyster_resource_suitability/traditional_cultch_mean.tif",
  overwrite = T
)


writeRaster(
  trad_cultch_sd,
  "data/oyster_resource_suitability/traditional_cultch_standard_dev.tif",
  overwrite = T
)

# # save RDS
# saveRDS(
#   trad_cultch_mean,
#   paste0("C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/traditional_cultch_mean.rds")
# )
# 
# # save RDS
# saveRDS(
#   trad_cultch_sd,
#   paste0("C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/traditional_cultch_standard_dev.rds")
# )

rm(mp, mp_hsi, sed_dep)









