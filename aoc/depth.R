# Angus Watters
# Lynker
# Reclassify depth inundation raster into too shallow, shallow, deep bins

rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

remove(list = ls())  # clear all workspace variables

# depth inundation rasters
depth_files <- paste0("data/depth/", list.files("data/depth/", pattern = "W_inun.tif"))
# depth <- raster("data/depth/MP2023_S07_G500_C000_U00_V00_SLA_O_01_01_W_inun.tif")

for (i in 1:length(depth_files)) {
  
  run  <- substr(depth_files[i], 19, 21)
  year <- substr(depth_files[i], 47, 51)
  
  logger::log_info("Processing depth {run} year {year}")

  depth <- raster(depth_files[i])
  
  # Coordinate ref system
  crs   <-  CRS('+init=EPSG:26915')
  
  # Assign CRS
  crs(depth) <- crs
  
  logger::log_info("Aggregating to 480m grid")
  
  # aggregate depth to 480m resolution
  depth <- raster::aggregate(depth, fact = 16)
  
  logger::log_info("converting meters to feet")
  
  # convert values to feet
  depth_feet <- depth*3.281
  
  # resample raster to grid 
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs   = CRS('+init=EPSG:26915'),
    ext   = extent(405220, 909700, 3199570, 3416530)
  )
  
  logger::log_info("Resampling")
  
  # resample depth values
  depth_resample   <- resample(depth_feet, resamp_r)
  
  # Depth relationship 
  depth_func <- function(x, ...) {
    ifelse(x > 0 & x < 2, 1,
           ifelse(x >= 2 & x <= 5, 2,
                  ifelse(x > 5, 3, NA))
    )
  }
  
  logger::log_info("Categorizing depths into bins")
  
  # reclassify depth values
  depth_reclass        <- calc(depth_resample, depth_func) %>% 
    setNames(c(paste0("water_depth_", run, "_", year)))
  
  logger::log_info("Saving to data/depth/")
  
  rds_path <- paste0("data/depth/water_depth_", run, "_", year, ".rds")
  tif_path <- paste0("data/depth/water_depth_", run, "_", year, ".tif")
  
  logger::log_info("Saving: \n{rds_path}\n{tif_path}")  
  
  # save
  saveRDS(depth_reclass, paste0("data/depth/water_depth_", run, "_", year, ".rds"))
  
  raster::writeRaster(
    depth_reclass, 
    paste0("data/depth/water_depth_", run, "_", year, ".tif")
    )
  
  rm(depth_agg, depth, depth_feet,depth_resample)
  
}
# depth inundation rasters
depth_files <- 
  grep(
    ".tif",
    paste0("data/depth/", list.files("data/depth/", pattern = "water_depth_S0")), 
    value = T
  )

land_files <- paste0("data/masks/", list.files("data/masks/", pattern = "landwater_S0"))

for (i in 1:length())
tmp <- raster::stack("data/oyster_viability/oyster_viability_S08_31_31.tif")
library(leaflet)
library(leaflet.extras)
leaflet() %>%
  setView(0, 0, 2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addDrawToolbar(
    targetGroup = "draw",
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  )  %>%
  addLayersControl(
    overlayGroups = c("draw"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addStyleEditor()

## for more examples see
# browseURL(system.file("examples/draw.R", package = "leaflet.extras"))
  # writeRaster(depth_reclass, "data/depth/depth_inundation.tif", overwrite = T)

# reclassification levels
# reclass_df <- c(
#   0,    1.5,  1,
#   1.5,  2.75, 2,
#   2.75, 3,    3
#   )
# 
# # reclassification matrix
# reclass_m <- matrix(
#   reclass_df,
#   ncol  = 3,
#   byrow = TRUE
#   )
# 
# # reclassify depth by matrix 
# depth_reclass2 <- depth_reclass %>% 
#   reclassify(reclass_m)%>% 
#   setNames(c("depth_inundation"))

# saveRDS(depth_reclass2, "data/depth/water_depth.rds")
# writeRaster(depth_reclass2, "data/depth/water_depth.tif", overwrite = T)



