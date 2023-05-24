# Angus Watters
# Lynker
# Clean Oyster harvest areas polygon and rasterize


library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)


# *********************************************
# ---- Rasterize oyster harvest areas poly ----
# *********************************************

oyster_harvest_areas <- readRDS("data/oyster_harvest_areas/oyster_harvest_areas.rds")

# resample raster to this grid 
empty_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)


oyster_harvest_areas_r <- fasterize::fasterize(
                                    sf      = oyster_harvest_areas, 
                                    raster  = empty_r,
                                    field   = "raster_value", 
                                    fun     = "sum"
                                  )


# save oyster harvest areas raster
raster::writeRaster(
                  oyster_harvest_areas_r,
                  "data/oyster_harvest_areas/oyster_harvest_areas_raster.tif",
                  overwrite = T
                  )
# saveRDS(oyster_harvest_areas_r, "data/oyster_harvest_areas/oyster_harvest_areas_raster.rds")

# tmp <- raster::raster("data/oyster_harvest_areas/oyster_harvest_areas_raster.tif") %>% 
#   setNames(c("oyster_harvest_areas_classification"))
# 
# tmp2 <- ratify(tmp)
# lvls              <- levels(tmp2)[[1]]
# lvls$status       <- c("Closed", "Intermediate", "Open")
# levels(tmp2)      <- lvls

rm(oyster_harvest_areas_r,oyster_harvest_areas, crs, ldh_interm, ldh, ldh_original, ldh_new, empty_r, land)











