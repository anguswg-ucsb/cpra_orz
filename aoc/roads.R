# Angus Watters
# Lynker
# Create raster layer for roads buffer polygon

library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

# *****************************************
# ---- rasterize roads buffers to grid ----
# *****************************************

# Binary landwater raster (land = 0, water = 1)
landwater <- readRDS("data/masks/landwater_raster.rds")

# Coordinate ref system
crs <-  CRS('+init=EPSG:26915')

# Road buffer polygon
road_shp <- readRDS("data/roads/road_buffer.rds")
here::here("data/roads/road_buffer.rds")
# empty raster templates for polygon to raster conversion
empty_r <- raster(
  ext = extent(landwater),
  res = c(480, 480), 
  crs = crs
  )

# Rasterize road buffers to raster layers
r_road_2km   <- fasterize::fasterize(road_shp[1,], raster = empty_r, field = "buffer")
r_road_5km   <- fasterize::fasterize(road_shp[2,], raster = empty_r, field = "buffer")
r_road_10km  <- fasterize::fasterize(road_shp[3,], raster = empty_r, field = "buffer")
r_road_20km  <- fasterize::fasterize(road_shp[4,], raster = empty_r, field = "buffer")

# stack 4 roads buffers into raster stack
stk_road     <- raster::stack(r_road_2km, r_road_5km, r_road_10km, r_road_20km)%>%
  setNames(c(
    "road_buffer_2km",
    "road_buffer_5km",
    "road_buffer_10km",
    "road_buffer_20km")
  )

# combine road shp geometries into one
road_shp <- road_shp %>% 
  group_by(buffer) %>%
  summarize(geometry =   st_combine(geometry))

# mask larger buffers by smaller buffer
road_mask5  <- mask(stk_road$road_buffer_5km, road_shp[1,], inverse = T)
road_mask10 <- mask(stk_road$road_buffer_10km, road_shp[2,], inverse = T)
road_mask20 <- mask(stk_road$road_buffer_20km, road_shp[3,], inverse = T)

# overlay buffers into single raster
cover_roads <- cover(road_mask10, road_mask20, identity = T )
cover_roads <- cover(road_mask5, cover_roads, identity = T)
cover_roads <- cover(stk_road$road_buffer_2km, cover_roads, identity = T)
mapview(cover_roads$layer)

# Extend 20km road buffer out 
road_extend  <- setValues(
  cover_roads, 
  ifelse(
    is.na(getValues(cover_roads)) == TRUE, 25, getValues(cover_roads)
    # is.na(getValues(cover_roads)) == TRUE, 20, getValues(cover_roads)
  )
)

# resample to raster
resamp_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)

# resample
road_extend   <- resample(road_extend, resamp_r)
plot(road_extend)

saveRDS(road_extend, "data/roads/road_buffer_raster.rds")
raster::writeRaster(road_extend, "data/roads/road_buffer_raster.tif", overwrite = T)

rm(cover_roads, cover_roads2, cover_roads3, road_mask10, road_mask20, road_mask5, empty_r, landwater, road_extend, road_extend2,
   r_road_10km, r_road_20km, r_road_2km, r_road_5km, road_shp, tmp, stk_road, roads, grid_ext, crs, resamp_r)
