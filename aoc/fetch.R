# Generate fetch data and convert to raster

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(waver)

# Binary landwater raster (land = 0, water = 1)
landwater <- readRDS("data/masks/landwater_raster.rds")

# Coordinate ref system
crs <-  CRS('+init=EPSG:26915')

crs(landwater) <- crs

# land polygon
land_poly <-  readRDS("data/masks/land_polygon.rds") %>% 
  st_transform(26915)
plot(land_poly$geometry)
# convert land  polygon to SP for fetch function
land_sp <- land_poly %>%
  as("Spatial") %>%
  as("SpatialPolygonsDataFrame") %>%
  spTransform(CRS('+init=EPSG:26915'))

# ---- Create water points ----

# make land values = NA
water <- setValues(
  landwater, ifelse(getValues(landwater) == 1, 1, NA)
)


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
    lat    =  st_coordinates(.)[,2]
  )

# find points NOT intersecting w/ land polygon
pt_intersect <- sapply(
                      st_intersects(water_pts, land_poly), function(x){length(x) == 0}
                     )

# filter water points for those not intersecting polygon
water_fetch_pts <- water_pts[pt_intersect,]

rm(water, water_pts, landwater, land_poly, pt_intersect)
# tmp <- water_fetch_pts[1:1500,]

# empty list to add fetch calculated points
fetch_lst <- list()

# loop over each water point and calculate the fetch
for (i in 1:nrow(water_fetch_pts)) {
    
    fetch_txt <- paste0(i, "/", nrow(water_fetch_pts ))

    logger::log_info("calculating fetch for point - {fetch_txt}")

    pt <- water_fetch_pts[i,]
    
    fetch_pts <- pt %>%
      as("Spatial") %>%
      as("SpatialPoints") %>%
      spTransform(CRS('+init=EPSG:26915'))
    
    fetch_calc <- waver::fetch_len(
      p           = fetch_pts,
      bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
      shoreline   = land_sp,
      dmax        = 20000,
      spread      = c(0, 0, 0),
      projected   = TRUE
    )
    
    fetch_df <- fetch_calc %>%
      data.frame() %>%
      tibble::rownames_to_column() %>%
      # pivot_wider(names_from = "rowname", values_from = ".") %>%
      setNames(c("bearing", "fetch")) %>%
      mutate(
        id  = i,
        lng = pt$lng,
        lat = pt$lat
      ) %>% 
      st_as_sf(coords = c("lng", "lat"), crs = 5070) %>% 
      st_transform(26915) %>% 
      # st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
      mutate(
        lng = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
      )  %>% 
      st_drop_geometry() 
    
    # add fetch df to list
    fetch_lst[[i]] <- fetch_df
    
  }

# bind rows
fetch_df <- bind_rows(fetch_lst)

# save fetch point data
saveRDS(
  fetch_df,
  "commercial_viability/fetch_pts.rds"
)

# fetch_df <-readRDS("data/fetch/fetch_pts.rds")

# fetch_sf <- fetch_df %>%
#   # group_by(id, lat, lng) %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 5070) %>%
#   st_transform(26915) %>%
#   mutate(
#     lng = st_coordinates(.)[,1],
#     lat = st_coordinates(.)[,2]
#   )

# fetch_df2 <- fetch_sf %>%
#   st_drop_geometry()

fetch_ext <- fetch_df %>%
  st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
  extent()


# summarize all bearings to one point each
fetch_summarize <- fetch_df %>%
  group_by(id, lat, lng) %>%
  summarize(fetch = mean(fetch)) %>%    # summarize mean of 3 max distances
  ungroup() %>% 
  dplyr::select(x = lng, y = lat, z = fetch) 

# empty raster for fetch points
empty_r <- raster(
                   ext    = extent(fetch_ext),
                  # ext    = extent(water_fetch_pts),
                  # ext    = extent(tmp),
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
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)

# resample to fit to grid
fetch_r   <- resample(fetch_r, resamp_r)

# save fetch raster
saveRDS(
        fetch_r, 
        "data/fetch/fetch_raster.rds"
      )

writeRaster(
        fetch_r, 
        "data/fetch/fetch_raster.tif",
        overwrite = TRUE
          )































