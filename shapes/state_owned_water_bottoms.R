
# Rasterize state owned water bottoms polygon

library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

#
# State owned water bottoms
sowb <- readRDS("data/state_owned_water_bottoms/slowaterbottoms_utm83_20210720.rds")


sowb <- sowb %>%
  filter(STATECLAIM == "Y")

# land polygon
land <- readRDS("data/masks/land_polygon.rds") %>%
  st_transform(26915)

# bounding box
bb <- land %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_sf()

mapview(bb)
sowb_crop <- sowb %>%
  st_crop(bb)

sowb_union <- sowb_crop %>%
  st_union() %>%
  st_sf() %>%
  mutate(val = 1)


sowb_simple <- sowb_union %>%
  rmapshaper::ms_simplify(keep = 0.5) %>%
  mutate(val = 1)


# ---- Rasterize state owned water bottoms ----
# resample raster to this grid 
empty_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)

sowb_r <- fasterize::fasterize(
  sf      = sowb_simple, 
  raster  = empty_r,
  field   = "val", 
  fun     = "sum"
)


# save waterways buffer
raster::writeRaster(sowb_r, "data/state_owned_water_bottoms/state_owned_water_bottoms_raster.tif", overwrite = T)
saveRDS(sowb_r, "data/state_owned_water_bottoms/state_owned_water_bottoms_raster.rds")
# saveRDS(sowb_r, "C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/state_owned_water_bottoms_raster.rds")


rm(sowb, sowb_crop, sowb_r, sowb_union)








