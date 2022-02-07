# Angus Watters
# Lynker
# Reclassify depth inundation raster into too shallow, shallow, deep bins

library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

remove(list = ls())  # clear all workspace variables

# depth inundation raster
depth <- raster("data/depth/MP2023_S07_G500_C000_U00_V00_SLA_O_01_01_W_inun.tif")

# Coordinate ref system
crs <-  CRS('+init=EPSG:26915')

# Assign CRS
crs(depth) <- crs

# aggregate depth to 480m resolution
depth <- raster::aggregate(depth, fact = 16)

# convert values to feet
depth_feet <- depth*3.281

# Depth relationship 
depth_func <- function(x, ...) {
  ifelse(x > 0 & x < 2, 1,
         ifelse(x >= 2 & x <= 5, 2,
                ifelse(x > 5, 3, NA))
         )
}
# reclassify depth values
depth_reclass        <- calc(depth_feet, depth_func)

# resample raster to grid 
resamp_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)

# resample depth values
depth_reclass   <- resample(depth_reclass, resamp_r)

# save
saveRDS(depth_reclass, "data/depth/depth_inundation.rds")
writeRaster(depth_reclass, "data/depth/depth_inundation.tif", overwrite = T)


