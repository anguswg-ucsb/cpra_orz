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
# saveRDS(landwater_binary, "data/masks/landwater_raster.rds")
# lw   <-  readRDS("data/masks/landwater_raster.rds")


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
rm(land_480)
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

# ****************************************
# water points
water_r <- setValues(
  tmp_r, ifelse(getValues(tmp_r) == 0 , 1, NA)
)

# aggregate water raster to 180m res
water_r <- raster::aggregate(water_r, fact = 2)

water_pts <- water_r %>%
  raster::rasterToPoints() %>%
  data.frame() %>%
  st_as_sf(
    coords = c("x", "y"),
    crs = crs(water_pts)
  ) %>%
  st_transform(5070) %>%
  mutate(
    lng = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# convert SF polygon to SP
poly_sp <- land_poly %>%
  as("Spatial") %>%
  as("SpatialPolygonsDataFrame") %>%
  spTransform(CRS("+init=epsg:5070"))

# convert SF polygon to SP
poly_sp_union <- land_sf %>%
  as("Spatial") %>%
  as("SpatialPolygonsDataFrame") %>%
  spTransform(CRS("+init=epsg:5070"))

# convert SF polygon to SP
land_line <- land_poly %>%
  st_cast("MULTILINESTRING")

line_sp <- land_line %>%
  as("Spatial") %>%
  spTransform(CRS("+init=epsg:5070"))


water_pts2 <- head(water_pts, 25)

# progress bar used in For loop
pb <- progress_bar$new(
  format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || Est time remaining: :eta || current: :current]",
  total  = nrow(water_pts2), complete = "=",  incomplete = "-",  current = ">",  clear = FALSE,  width = 200)

land_sf <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/land_type/shp/land_polygon_simple_v4.rds")
rm(fetch_calc,fetch_df, fetch_lst, fetch_pts)

fetch_lst <- list()

mapview(pt) +poly_sp

# loop over each water point and calculate the fetch
for (i in 1:nrow(water_pts2)) {
  pb$tick()
  
  pt <- water_pts2[5,]
  
  fetch_pts <- pt %>%
    as("Spatial") %>%
    as("SpatialPoints") %>%
    spTransform(CRS("+init=epsg:5070"))
  system.time(
    fetch_calc <- fetch_len(
      p           = fetch_pts,
      bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
      shoreline   = poly_sp,
      dmax        = 10000,
      spread      = c(-10, 0, 10),
      projected   = TRUE
    )
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
    )
  
  # add fetch df to list
  fetch_lst[[i]] <- fetch_df
  
}
fetch_bind <- bind_rows(fetch_lst)
# test point
pt <- pts_seq[129,]


# for loop for calculating fetch w/ gridDist
tmp <- water_pts2 %>% head(100)

fetch_pts <- tmp %>%
  as("Spatial") %>%
  as("SpatialPoints") %>%
  spTransform(CRS("+init=epsg:5070"))
system.time(
  fetch_dist_multi <- waver::fetch_len_multi(
    pts           = fetch_pts,
    bearings      = c(0, 45, 90, 135, 180, 225, 270, 315),
    shoreline     = poly_sp,
    dmax          = 10000,
    spread        = c(-10, 0, 10),
    projected     = TRUE
  )
)
plot(water_pts[c(300:55000),])
plot(land_poly, add = T)

plot(rt2[2,])
rm(cl1, cS, lw_binary, landwater)
#world lon/lat raster


#UTM small area
crs(r) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

agg_lst <- list()
for (i in 1:length(lst)) {
  agg <- raster::aggregate(lst[[i]], fact = 10)
  agg_lst[[i]] <- agg
}
# landtype raster, land = 2, water = 1
landwater <- raster::raster("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/land_type/landwater_binary_mp2023.tif")

rasterVis::levelplot(landwater)

# make land values = 2, water = 1
lw_binary <- setValues(
  landwater, ifelse(getValues(landwater) == 2 , 1, NA)
)

# aggregate raster to lower Res
lw_binary <- raster::aggregate(lw_binary, fact = 3)

# create coast polygon
rt <- terra::rast(lw_binary) %>%
  terra::as.polygons() %>%
  sf::st_as_sf()
beep("coin")

# Simplify geometries tq
rt2 <- sf::st_simplify(rt, dTolerance = 30)
rt2 <- rmapshaper::ms_simplify(rt2, keep = 0.15)
beep("coin")
mapview::npts(rt)
mapview::npts(land_sf)
mapview::npts(rt3_land)
mapview(land_sf) + rt3_land
rm(rt, rt2, landwater)

# make valid geometry, cast
rt3 <- rt2 %>%
  st_make_valid() %>%
  st_cast()

mapview(rt3_land$geometry[1]) +rt3_land$geometry[2]
rt3_land <- rt3[3,]

nbpt
saveRDS(rt3_land,paste0(path, "land_type/shp/land_polygon_simple_v5.rds"))