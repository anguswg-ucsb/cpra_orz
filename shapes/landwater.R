# Angus Watters
# Lynker
# Create land water rasters and shapefiles
rm(list = ls())
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(logger)
library(terra)
library(raster)
# ---- Land water raster -----
# landwater <- readRDS( "data/raw/MP2023_S07_G500_C000_U00_V00_SLA_O_03_03_W_lndtyp.rds")
# landwater <- raster::raster("data/raw/MP2023_S07_G500_C000_U00_V00_SLA_O_03_03_W_lndtyp.tif")

landwater_files <- list.files("data/raw/", pattern = ".tif")

for (i in 1:length(landwater_files)) {
  
  run  <- substr(landwater_files[i], 8, 10)
  year <- substr(landwater_files[i], 36, 40)
  
  logger::log_info("Processing land type {run} year {year} \n File name: {landwater_files[i]}")

  landwater <- raster::raster(paste0("data/raw/", landwater_files[i]))
  
  # Coordinate ref system
  crs <-  CRS('+init=EPSG:26915')
  
  # Assign CRS
  crs(landwater) <- crs
  
  # resample raster to this grid 
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs   = CRS('+init=EPSG:26915'),
    ext   = extent(405220, 909700, 3199570, 3416530)
  )
  
  # remove -9999 values
  landwater_clamp <- clamp(landwater, lower = 0, useValues = F)
  
  logger::log_info("aggregating data from 30m to 480m grid")
  
  # aggregate to 480m grid
  landwater_agg <- raster::aggregate(landwater_clamp, fact = 16)

  # remove objects
  # rm(landwater_clamp, landwater)
  
  # resample
  landwater_resample <- resample(
    landwater_agg, 
    resamp_r,
    method = "ngb"
  )
  
  logger::log_info("Creating landwater binary raster")
  
  # make land values = 0, water = 1
  land_types <- setValues(
    landwater_resample, ifelse(getValues(landwater_resample) == 2, 1, 0)
  )
  
  logger::log_info("Creating land raster")
  
  # land raster, land = 2
  land <- setValues(
    landwater_resample, ifelse(getValues(landwater_resample) != 2, 2, NA)
  ) %>%
    setNames(c(paste0("land_", run, "_", year)))
      
  logger::log_info("Creating water raster")
  
  # water raster, land = 1
  water <- setValues(
    landwater_resample, ifelse(getValues(landwater_resample) == 2, 1, NA)
    ) %>% 
    setNames(c(paste0("water_", run, "_", year)))

  logger::log_info("Creating land polygon")
  
  # create land polygon, simplify geometries 
  land_shp <- land %>% 
    terra::rast() %>%
    terra::as.polygons() %>% 
    sf::st_as_sf()  %>%
    st_make_valid() %>%
    st_cast() %>% 
    dplyr::slice(n = 1) %>% 
    setNames(c("land", "geometry")) %>% 
    st_transform(26915)
  
  logger::log_info("Saving land type raster to data/masks/")
  
  # save land raster
  saveRDS(landwater_resample, paste0("data/masks/landtype_", run, "_", year, ".rds"))
  # raster::writeRaster(landwater_resample, paste0("data/masks/landtype_", run, "_", year, ".tif"), overwrite = TRUE)
    
  logger::log_info("Saving landwater binary raster to data/masks/")
  
  # save land raster
  saveRDS(land_types, paste0("data/masks/landwater_", run, "_", year, ".rds"))
  # raster::writeRaster(land_types, paste0("data/masks/landwater_", run, "_", year, ".tif"), overwrite = TRUE)
  
  logger::log_info("Saving land raster to data/masks/")
  
  # save land raster
  saveRDS(land, paste0("data/masks/land_", run, "_", year, ".rds"))
  # raster::writeRaster(land, paste0("data/masks/land_", run, "_", year, ".tif"), overwrite = TRUE)

  logger::log_info("Saving water raster to data/masks/")
  
  # save water raster
  saveRDS(water, paste0("data/masks/water_", run, "_", year, ".rds"))
  # raster::writeRaster(water, paste0("data/masks/water_", run, "_", year, ".tif"), overwrite = TRUE)
  
  logger::log_info("Saving land polygon to data/masks/")
  
  # save land polygon
  saveRDS(land_shp, paste0("data/masks/land_polygon_", run, "_", year, ".rds"))
  
  rm(land, land_shp, water, landwater_agg, landwater_resample)
  
}

# ************************
# ---- Plot Land diff ----
# ************************

fetch_files <- 
  grep(
    ".tif",
    paste0("data/fetch/", list.files("data/fetch/", pattern = "fetch_raster_S0")),
    value = T
  )

land_files     <- paste0("data/masks/", list.files("data/masks/", pattern = "landwater_S0"))
land_poly_file <- paste0("data/masks/", list.files("data/masks/", pattern = "land_polygon_S0"))
  
# crop to extent
ext <- extent(c(404980 , 909940, 3199810 , 3348000))
  
# current land
land_current <- readRDS(land_files[1]) %>% 
  crop(ext)

# future land
land_future  <- readRDS(land_files[2]) %>% 
  crop(ext)

# coordinate reference system
crs <- CRS('+init=EPSG:26915')

crs(land_future)   <- crs
crs(land_current)  <- crs

# Land difference map
land_diff    <- land_current - land_future
# 1  is land, -1 is water

# categorical labels
land_diff_cat <- raster::ratify(land_diff)

lvls              <- levels(land_diff_cat)[[1]]
class(lvls)
lvls$status            <- c("Land turned to water",
                            'No change',
                            'Water turned to land')
levels(land_diff_cat)  <- lvls

# Land polygon
land_simple  <- readRDS(land_poly_file[1]) %>%
    st_transform(26915) %>%
    st_crop(ext)

# create xy dataframes of rasters for plotting w/ ggplot2
land_diff_df <- land_diff_cat %>%
  as.data.frame(xy = TRUE) %>%
  setNames(c("x", "y", "layer_status")) %>%
  setNames(c("long","lat", "layer_status")) %>% 
  mutate(
    title        =  "Change"
  )

land_diff_df$layer_status <- factor(land_diff_df$layer_status,
                                levels = c("Land turned to water",
                                           'No change',
                                           'Water turned to land'))
  
# plot Land difference 
land_plot <-
    ggplot() +
      geom_sf(
        data  = land_simple,
        fill  = "transparent",
        color = "transparent",
        alpha = 0.1) +
      geom_tile(
        data = na.omit(land_diff_df),
        aes(
          x    = long,
          y    = lat,
          fill = layer_status
        ),
        alpha = 0.8) +
      ggspatial::annotation_scale(
        text_cex    = 1.1,
        pad_x       = unit(2, "cm"),
        pad_y       = unit(0.1, "cm"),
        line_width  = 2,
        text_face   = "bold",
        tick_height = 0.8,
        height      = unit(0.2, "cm"),
        aes(
          location  = "br",
          style     = "ticks")
      ) + 
      # scale_fill_manual(values = c("cyan", "#440154FF",  "hotpink"))
      # scale_fill_manual(values = c("#440154FF", "#2A788EFF",  "#FDE725FF")) 
      scale_fill_manual(values = c("cyan", "#440154FF",  "#FDE725FF")) +
      # limits = c(-1, 1)fill  = "transparent",
      # ) 
      labs(
        title     = "Land difference (S07 03_03 Land - S07 27_27 Land)",
        fill      = "",
        y         = "",
        x         = ""
      ) +
      # ggthemes::theme_few() +  # theme_classic() +
      ggthemes::theme_map() +
      theme(
        axis.ticks      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        plot.title      = element_text(size = 20, face = "bold"),
        plot.subtitle   = element_text(size = 16),
        plot.caption    = element_text(size = 12),
        legend.title    = element_text(size = 14, face = "bold"),
        legend.text     = element_text(size = 14)
      )
    
    
    # fetch_plot
    ggsave(
      filename  = "plots/land_difference.png",
      plot      = land_plot,
      width     = 14,
      height    = 8
    )
  



landwater <- raster::raster(paste0("data/raw/", landwater_files[2]))
landwater <- raster::raster("data/raw/MP2023_S07_G500_C000_U00_V00_SLA_O_27_27_W_lndtyp.tif")


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

# remove -9999 values
landwater_clamp <- clamp(landwater, lower = 0, useValues = F)
# plot(landwater_clamp)

# aggregate to 480m grid
landwater_agg <- raster::aggregate(landwater_clamp, fact = 16)
# landwater_agg_120 <- raster::aggregate(landwater_clamp, fact = 4)

# plot(landwater_agg)


# resample
landwater_resample <- resample(
                            landwater_agg, 
                            resamp_r,
                            method = "ngb"
                            )

# plot(landwater_resample)

# Save 1-5 categorized land type, resampled and regridded to 480m grid
raster::writeRaster(
  landwater_resample,
  "data/masks/land_type_07.tif",
  overwrite = T
)
saveRDS(landwater_resample, "data/masks/land_type_07.rds")

# **********************************
# ----  Binary landwater raster ----
# **********************************

# 5 level categorical land type raster
# landwater_resample <- raster::raster("data/masks/land_type_07.tif")
# plot(landwater_resample)


# land raster, land = 2
land <- setValues(
  landwater_resample, ifelse(getValues(landwater_resample) != 2, 2, NA)
) %>%
  setNames(c("land_07"))
plot(land)


# save
saveRDS(land, "data/masks/land_raster_07.rds")
raster::writeRaster(land, "data/masks/land_raster_07.tif", overwrite = TRUE)


# water raster, land = 1
water <- setValues(
  landwater_resample, ifelse(getValues(landwater_resample) == 2, 1, NA)
) %>% 
  setNames(c("water_07"))
plot(water)

# save
saveRDS(water, "data/masks/water_raster_07.rds")
raster::writeRaster(water, "data/masks/water_raster_07.tif", overwrite = TRUE)

# ***********************
# ----  Land polygon ----
# ***********************

# create land polygon, simplify geometries 
land_shp <- land %>% 
  terra::rast() %>%
  terra::as.polygons() %>% 
  sf::st_as_sf() 
  # rmapshaper::ms_simplify(keep = 0.03) %>%
  # st_simplify(dTolerance = 35)

# check number of total points in geometry
mapview::npts(land_shp)
plot(land_shp)

# make valid geometry, cast
land_shp <- land_shp %>%
  st_make_valid() %>%
  st_cast() %>% 
  dplyr::slice(n = 1) %>% 
  setNames(c("land", "geometry")) %>% 
  st_transform(26915)

rm(crs, landwater, land_shp2)

# save land shape
saveRDS(land_shp, "data/masks/land_polygon_07.rds")

# plot(landwater_binary_agg)
# mapview(landwater_binary_agg) + landwater_binary
# # aggregate land water to 480m resolution
# landwater_binary_agg <- raster::aggregate(landwater_binary, fact = 16)
# 
# # resample
# landwater_binary_resample <- resample(landwater_binary_agg, resamp_r)
# plot(landwater_binary_resample)
# save land water raster
raster::writeRaster(
  landwater_binary,
  "data/masks/landwater_raster_07.tif",
  overwrite = T
)
saveRDS(landwater_binary, "data/masks/landwater_raster_07.rds")

# **********************
# ----  Land raster ----
# **********************
# land raster
# land             <- setValues(
#            landwater, ifelse(getValues(landwater) == 1 , 1, NA)
#                             )
landwater_binary <- readRDS("data/masks/landwater_raster_07.rds")
plot(landwater_binary)
landwater_resample <- raster::raster("data/masks/land_type_07.tif")

# make land values = 2
land_types <- setValues(
  landwater_resample, ifelse(getValues(landwater_resample) != 0, getValues(landwater_resample), NA)
)
plot(land_types)
# Land raster, land values = 2
land <- setValues(
  landwater_binary, ifelse(getValues(landwater_binary) != 1, 2, NA)
  
  # land_types, ifelse(getValues(land_types) != 2 & getValues(land_types) >= 1, 2, NA)
  # landwater_resample, ifelse(getValues(landwater_resample) == 1 | getValues(landwater_resample) == 3 | getValues(landwater_resample) == 4 | getValues(landwater_resample) == 5 , 2, NA   )
)
plot(land)
water <- setValues(
  landwater_binary, ifelse(getValues(landwater_binary) == 1, 2, NA)
  
  # land_types, ifelse(getValues(land_types) != 2 & getValues(land_types) >= 1, 2, NA)
  # landwater_resample, ifelse(getValues(landwater_resample) == 1 | getValues(landwater_resample) == 3 | getValues(landwater_resample) == 4 | getValues(landwater_resample) == 5 , 2, NA   )
)

# water raster, water values = 2
water <- setValues(
  land_types, ifelse(getValues(land_types) == 2, 1, NA)
)
plot(water)
# plot(tmp$MP2023_S07_G500_C000_U00_V00_SLA_O_03_03_W_lndtyp)
# tmp <- raster("data/masks/land_raster.tif")
# plot(tmp)
# aggregate to 480m and 120m resolution

land_480   <- raster::aggregate(land, fact = 16)
land_120   <- raster::aggregate(land, fact = 4)

# resample
land_480   <- resample(land_480, resamp_r)

land <- land %>% setNames(c("land_07"))

# save
saveRDS(land, "data/masks/land_raster_07.rds")
raster::writeRaster(land, "data/masks/land_raster_07.tif", overwrite = TRUE)


# ***********************
# ----  Land polygon ----
# ***********************
library(terra)
install.packages("terra")
land_r <- terra::rast("data/masks/land_raster_07.tif")

# create land polygon, simplify geometries 
land_shp <- land_r %>% 
  # terra::rast() %>%
  terra::as.polygons() %>% 
  sf::st_as_sf() 
  # rmapshaper::ms_simplify(keep = 0.03) %>% 
  # st_simplify(dTolerance = 35)
mapview::npts(land_shp)
plot(land_shp)

# make valid geometry, cast
land_shp <- land_shp %>%
  st_make_valid() %>%
  st_cast() %>% 
  dplyr::slice(n = 1) %>% 
  setNames(c("land", "geometry")) %>% 
  st_transform(26915)

rm(crs, landwater, land_shp2)

# save land shape
saveRDS(land_shp, "data/masks/land_polygon_07.rds")

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

# save
saveRDS(water, "data/masks/water_raster.rds")
raster::writeRaster(water, "data/masks/water_raster.tif", overwrite = TRUE)


rm(land, land_120, land_480, land_shp, landwater, resamp_r,crs, water)


# Depth relationship 
# lw_func <- function(x, ...) {
#   ifelse(x >= 0 & x < 1, 1,
#          ifelse(x >= 1, 2, NA))
#   #               ifelse(x > 5, 3, NA))
#   # )
# }

# # reclassify depth values
# lw_reclass        <- calc(landwater, lw_func)











