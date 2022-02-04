library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(logger)

# ---- Land water raster -----
landwater <- readRDS( "data/raw/MP2023_S07_G500_C000_U00_V00_SLA_O_03_03_W_lndtyp.rds")
# landwater <- raster::raster("data/raw/MP2023_S07_G500_C000_U00_V00_SLA_O_03_03_W_lndtyp.tif")



# Coordinate ref system
crs <-  CRS('+init=EPSG:26915')

# Assign CRS
crs(landwater) <- crs

# resample raster to this grid 
resamp_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)

# **********************************
# ----  Binary landwater raster ----
# **********************************

# (land = 0, water = 1)
landwater_binary <- setValues(
                              landwater, ifelse(getValues(landwater) == 2 | getValues(landwater) == -9999, 1, 0)
                            )

# resample
land_r3 <- resample(land_r4, resamp_r)

plot(land)

# aggregate land water to 480m resolution
landwater_binary <- raster::aggregate(landwater_binary, fact = 16)

# resample
landwater_binary <- resample(landwater_binary, resamp_r)

# save land water raster
raster::writeRaster(
  landwater_binary,
  "data/masks/landwater_raster.tif",
  overwrite = T
)
saveRDS(landwater_binary, "data/masks/landwater_raster.rds")


# **********************
# ----  Land raster ----
# **********************
# land raster
# land             <- setValues(
#            landwater, ifelse(getValues(landwater) == 1 , 1, NA)
#                             )

# make land values = 2
land <- setValues(
  landwater, ifelse(getValues(landwater) == 1 | getValues(landwater) == 3 | getValues(landwater) == 4 | getValues(landwater) == 5 , 2, NA)
)

# aggregate to 480m and 120m resolution
land_480   <- raster::aggregate(land, fact = 16)
land_120   <- raster::aggregate(land, fact = 4)

# resample
land_480   <- resample(land_480, resamp_r)


# save
saveRDS(land_480, "data/masks/land_raster.rds")
raster::writeRaster(land_480, "data/masks/land_raster.tif", overwrite = TRUE)


# ***********************
# ----  Land polygon ----
# ***********************

# create land polygon, simplify geometries 
land_poly <- land_120 %>% 
  terra::rast() %>%
  terra::as.polygons() %>% 
  sf::st_as_sf()  %>% 
  rmapshaper::ms_simplify(keep = 0.03) %>% 
  st_simplify(dTolerance = 35)

# make valid geometry, cast
land_poly <- land_poly %>%
  st_make_valid() %>%
  st_cast() %>% 
  slice(n = 1) %>% 
  setNames(c("land", "geometry")) %>% 
  st_transform(26915)

rm(crs, landwater, land_poly2)

# save land shape
saveRDS(land_poly, "data/masks/land_polygon.rds")

# ***********************
# ----  water raster ----
# ***********************

# make land values = NA
water <- setValues(
  landwater, ifelse(getValues(landwater) == 2 , 1, NA)
)

# set CRS
crs(water) <- crs

# lower resolution to 480m 
water <- aggregate(water, fact = 16)

# resample
water <- resample(water, resamp_r)


saveRDS(water, "data/masks/water_raster.rds")
raster::writeRaster(water, "data/masks/water_raster.tif", overwrite = TRUE)


rm(land, land_120, land_480, land_poly, landwater, resamp_r,crs, water)

