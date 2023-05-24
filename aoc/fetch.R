# Angus Watters
# Lynker
# Generate fetch data and convert to raster

rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(waver)
library(foreach)
library(doParallel)

# # Binary landwater raster (land = 0, water = 1)
# landwater <- readRDS("data/masks/landwater_raster.rds")
# 
# # Coordinate ref system
# crs <-  CRS('+init=EPSG:26915')
# 
# crs(landwater) <- crs

# land file paths
land_files      <- paste0("data/masks/",list.files("data/masks/", pattern = "land_polygon_S0"))

# water file paths
water_files     <- grep(
                        "/water_S0",
                        paste0("data/masks/",list.files("data/masks/", pattern = "water_S0")),
                        value = T
                        )

land_sp_lst  <- list()
water_sf_lst <- list()
water_sp_lst <- list()

for (i in 1:length(land_files)) {
  
  run  <- substr(land_files[i], 25, 27)
  year <- substr(land_files[i], 29, 33)
  
  logger::log_info("Processing land & water rasters for fetch calc - {run} year {year}")
  
  # Coordinate ref system
  crs <-  CRS('+init=EPSG:26915')
  
  # land polygon
  land_poly <-  readRDS(land_files[i]) %>% 
    st_transform(26915) %>% 
    mutate(
      run  = run,
      year = year
    ) %>% 
    dplyr::relocate(land, run, year, geometry)
  
  # land_poly <-  readRDS("data/masks/land_polygon_07.rds") %>% 
  #   st_transform(26915)
  # 
  
  # convert land  polygon to SP for fetch function
  land_sp <- land_poly %>%
    as("Spatial") %>%
    as("SpatialPolygonsDataFrame") %>%
    spTransform(CRS('+init=EPSG:26915'))

  # ---- Create water points ----
  
  # make land values = NA
  # water <- setValues(
  #   landwater, ifelse(getValues(landwater) == 1, 1, NA)
  # )
  water <- readRDS(water_files[i])
  # water <- raster::raster("data/masks/water_raster_07.tif")
  
  # set CRS
  crs(water) <- crs
  
  # mask water raster by land polygon
  water <- mask(water, land_poly, inverse = T)
  
  # convert water mask to points, add lat/lon columns
  water_pts <- water %>%
    raster::rasterToPoints() %>%
    data.frame() %>%
    st_as_sf(
      coords =  c("x", "y"),
      crs    =  CRS('+init=EPSG:26915')
    ) %>%
    st_transform(26915) %>%
    mutate(
      lng    =  st_coordinates(.)[,1],
      lat    =  st_coordinates(.)[,2],
      run    =  run,
      year   =  year
    ) %>% 
    setNames(c("value", "geometry", "lng", "lat", "run", "year")) %>% 
    dplyr::relocate(run, year, value, lng, lat, geometry)
  
  
  # find points NOT intersecting w/ land polygon
  pt_intersect <- sapply(
                        st_intersects(water_pts, land_poly), function(x){length(x) == 0}
                       )
  
  # filter water points for those not intersecting polygon
  water_fetch_pts <- water_pts[pt_intersect,]
  
  
  # rm(water, water_pts, landwater, land_poly, pt_intersect)
  
  water_fetch_pts_sp <- water_fetch_pts %>% 
    as("Spatial") %>%
    as("SpatialPoints") %>%
    spTransform(CRS('+init=EPSG:26915'))
  
  land_sp_lst[[i]]  <- land_sp
  water_sf_lst[[i]] <- water_fetch_pts
  water_sp_lst[[i]] <- water_fetch_pts_sp

 }
rm(crs, land_poly, land_sp_lst, water_pts, water_sf_lst, water_sp_lst)
# empty list to add fetch calculated points
fetch_lst <- list()

# loop over each water point and calculate the fetch
# for (i in 1:nrow(water_fetch_pts)) {

# fetch_pts <- tmp %>%
#   as("Spatial") %>%
#   as("SpatialPoints") %>%
#   spTransform(CRS('+init=EPSG:26915'))
# system.time(
# fetch_calc <- waver::fetch_len_multi(
#   p           = fetch_pts,
#   bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
#   shoreline   = land_sp,
#   dmax        = 20000,
#   spread      = c(0, 0, 0),
#   projected   = TRUE
# )
# )


cores = detectCores()
cl    <- makeCluster(cores[1]-3) #not to overload your computer
registerDoParallel(cl)

# rm(pt_lat, pt_lng, i, pt_intersect, land_files, year, water_files, run)
# for (k in 1:length(districts)) {
fetch_loop <- foreach(i = 1:length(water_fetch_pts_sp), .combine = "rbind", .packages = c("dplyr", "waver", "sf")) %dopar% {
  
  fetch_pts <- water_fetch_pts_sp[i]
  
  pt_lat    <- water_fetch_pts[i,] 
  pt_lat    <- pt_lat$lat
  pt_lng    <- water_fetch_pts[i,] 
  pt_lng    <- pt_lng$lng
  
  
  fetch_df <- waver::fetch_len(
    p           = fetch_pts,
    bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
    shoreline   = land_sp,
    dmax        = 20000,
    spread      = c(0, 0, 0),
    projected   = TRUE
  ) %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    setNames(c("bearing", "fetch")) %>%
    mutate(
      id  = i,
      lng = pt_lng,
      lat = pt_lat
    ) %>% 
    st_as_sf(coords = c("lng", "lat"), crs = 26915) %>% 
    # st_transform(26915) %>% 
    # st_as_sf(coords = c("lng", "lat"), crs = 5070) %>% 
    # st_transform(26915) %>% 
    mutate(
      lng = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2]
    )  %>% 
    st_drop_geometry() %>% 
    group_by(id, lat, lng) %>%
    summarize(fetch = mean(fetch)) %>%    # summarize mean of 3 max distances
    ungroup() 
  
  fetch_df
}

  # stop extra workers
doParallel::stopImplicitCluster()
stopCluster(cl)


# save
saveRDS(fetch_loop, "data/fetch/fetch_pts_S08_27_27.rds")
# fetch_loop2 <- readRDS("data/fetch/fetch_pts_07_v2.rds")

fetch_ext <- fetch_loop %>%
  st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
  extent()

fetch_ext

# summarize all bearings to one point each
fetch_summarize <- fetch_loop %>%
  dplyr::select(x = lng, y = lat, z = fetch) 

# empty raster for fetch points
empty_r <- raster(
  ext    = extent(fetch_ext),
  res    = c(480, 480),
  crs    = crs
)

# rasterize fetch points
fetch_r <- raster::rasterize(
  x      = fetch_summarize[,1:2],
  y      = empty_r,
  field  = fetch_summarize[,3],
  fun    = mean
)

# set CRS
crs(fetch_r) <- crs

# resample raster to grid 
resamp_r <- raster(
  nrows   = 452,
  ncols   = 1051,
  crs     = CRS('+init=EPSG:26915'),
  ext     = extent(405220, 909700, 3199570, 3416530)
  )


# resample to fit to grid
fetch_r   <- raster::resample(fetch_r, resamp_r)
plot(fetch_r)
# save fetch raster RDS
saveRDS(
  fetch_r,
  "data/fetch/fetch_raster_S08_27_27.rds"
)

# save fetch raster TIF
writeRaster(
  fetch_r,
  "data/fetch/fetch_raster_S08_27_27.tif",
  overwrite = TRUE
)

# ********************
# ---- Plot fetch ----
# ********************

fetch_files <- 
  grep(
    ".tif",
    paste0("data/fetch/", list.files("data/fetch/", pattern = "fetch_raster_S0")),
    value = T
    )

land_files <- paste0("data/masks/", list.files("data/masks/", pattern = "land_polygon_S0"))

for (i in 1:length(fetch_files)) {
  
  run <- substr(fetch_files[i], 25, 33)
  
  logger::log_info("Plotting {fetch_files[i]}")
  
  # crop to extent
  ext <- extent(c(404980 , 909940, 3199810 , 3348000))
  
  land_simple <- readRDS(land_files[i]) %>%
    st_transform(26915) %>%
    st_crop(ext)
  
  fetch  <- fetch_files[i] %>% 
    raster::raster() %>%
    crop(ext)
  
  # coordinate reference system
  crs <- CRS('+init=EPSG:26915')
  
  crs(fetch)  <- crs
  
  # create xy dataframes of rasters for plotting w/ ggplot2
  fetch_df <- fetch %>%
    as.data.frame(xy = TRUE) %>%
    setNames(c("x", "y", "layer_status")) %>%
    setNames(c("long","lat", "layer_status"))
  
  # plot Roads buffer
  fetch_plot <-
    ggplot() +
      geom_sf(
        data  = land_simple,
        fill  = "black",
        alpha = 0.1,
        size  = 0.1) +
      geom_tile(
        # data = fetch_df,
        data = na.omit(fetch_df),
        aes(
          x    = long,
          y    = lat,
          fill = layer_status
        ),
        alpha = 0.8) +
    # geom_sf(
    #   data  = land_simple,
    #   fill  = "black",
    #   alpha = 0.2,
    #   size  = 0.1) +
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
      viridis::scale_fill_viridis(direction = -1, option = "H"
                                  # limits = c(0, 1.0)
      ) +
      labs(
        title     = paste0("Fetch ", run),
        fill      = "Distance (m)",
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
  
  save_path <- paste0("plots/fetch_", run, ".png")
  
  logger::log_info("Saving plot: \n{save_path}")
  
  # fetch_plot
  ggsave(
    filename  = save_path,
    plot      = fetch_plot,
    width     = 14,
    height    = 8
  )
  
}

















