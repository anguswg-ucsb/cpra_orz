# Create commercial viability layers

library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

source("utils/utils.R")

# water raster for masking
water        <-  readRDS("data/masks/water_raster.rds")

# land polygon for masking
land         <- readRDS("data/masks/land_polygon.rds")

# Distance to roads
roads        <- readRDS("data/roads/road_buffer_raster.rds")

# Fetch
fetch        <- readRDS("data/fetch/fetch_raster.rds")

# 10 year Sediment Deposition mean
sed_dep      <- readRDS("data/sediment_dep/sediment_deposition.rds")

# Depth 
depth        <- readRDS("data/depth/depth_inundation.rds")


commercial_viability <- get_cv(
                                water   = water,
                                land    = land,
                                roads   = roads,
                                fetch   = fetch,
                                sed_dep = sed_dep, 
                                depth   = depth
                              )


# save output
writeRaster(commercial_viability, "data/commercial_viability/commercial_viability.tif", overwrite = T)
saveRDS(stack(commercial_viability),"data/commercial_viability/commercial_viability.rds")

# final_data_path <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"
# writeRaster(commercial_viability, paste0(final_data_path, "commercial_viability.tif"), overwrite = T)
# saveRDS(stack(commercial_viability),"C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/commercial_viability.rds")
# writeRaster(commercial_viability,"C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/commercial_viability.tif", overwrite = T)













