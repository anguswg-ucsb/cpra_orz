# Angus Watters
# Lynker
# Buffer Navigation channels and save to raster

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)

# ---- Buffer navigation channels ----
# navigatable waterways
waterways <- sf::read_sf("data/waterways/Waterway_Network.shp") %>%
  st_transform(26915)

# land polygon
land <- readRDS('data/masks/land_polygon.rds') %>%
  st_transform(26915)

# bounding box for crop
bb <- st_bbox(land) %>%
  st_as_sfc()  %>%
  st_sf()

# crop wways to AOI and union all lines to one way
waterways <- waterways %>% 
  st_intersection(bb) %>%
  st_union() %>%
  st_sf()

# 1500 ft buffer
waterways_buff <- waterways %>%
  st_buffer(457.2) %>%
  mutate(val = 5)

saveRDS(waterways_buff, "data/waterways/navigation_channels.rds")

# ---- Rasterize waterways poly ----
# resample raster to this grid 
empty_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)


waterways_r <- fasterize::fasterize(
                                   sf      = waterways_buff, 
                                   raster  = empty_r,
                                   field   = "val", 
                                   fun     = "sum"
                                  )
# make water ways values = 5, else = NA
waterways_r <- setValues(
  waterways_r, ifelse(getValues(waterways_r) == 5, 5, NA)
)

# save waterways buffer
raster::writeRaster(waterways_r, "data/waterways/navigation_channels_raster.tif", overwrite = T)
saveRDS(waterways_r, "data/waterways/navigation_channels_raster.rds")

rm(waterways_buff, r, empty_r, bb, land,waterways_r, waterways)


