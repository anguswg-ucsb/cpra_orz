# Angus Watters
# Lynker
# Clean and buffer coastal use permits polygon and create raster

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)

# crs <- CRS('+init=EPSG:26915')
ext <- extent(c(404980, 903580, 3206050, 3431410))

# Coastal Use Permit Polygon
cup_poly  <- sf::read_sf("data/coastal_use_permits/Coastal_Permits_Polygons.shp") %>%
  st_transform(26915)

# cup_lines <- sf::read_sf("data/coastal_use_permits/Coastal_Permits_Lines.shp") %>%
#   st_transform(26915)

cup_poly <- cup_poly %>%
  st_crop(ext)

# empty list to add to in For Loop
buffer_lst <- list()
# rm(buff, buff_df, tmp)

# For loop iterating through each CUP polygon and buffering 1500 ft
for (i in 1:nrow(cup_poly)) {
  cup_txt <- paste0(i, "/", nrow(cup_poly))
  logger::log_info("Buffering coastal use permit {cup_txt}")

  buff <- cup_poly[i,] %>%
    st_buffer(457.2)
  
  buffer_lst[[i]] <- buff
}

# bind rows of buffer list
buff_df <- bind_rows(buffer_lst)
# saveRDS(buff_df, paste0(path, "cup/cup_buffer.rds"))

# union all CUP buffered polygons
cup_union <- buff_df %>%
  st_union() %>%
  st_sf() %>%
  mutate(id = 1) %>%
  relocate(id, geometry)

# simplify CUP buffer geoms
cup_simple <- cup_union %>%
  rmapshaper::ms_simplify(keep = 0.2)


saveRDS(cup_simple,   "data/coastal_use_permits/coastal_use_permits.rds")


# ---- Rasterize coastal use permits poly ----

# resample raster to this grid 
empty_r <- raster(
                  nrows = 452,
                  ncols = 1051,
                  crs = CRS('+init=EPSG:26915'),
                  ext = extent(405220, 909700, 3199570, 3416530)
                )


cup_r <- fasterize::fasterize(
                              sf      = cup_simple, 
                              raster  = empty_r,
                              field   = "id", 
                              fun     = "sum"
                            )


raster::writeRaster(
                    cup_r, 
                    "data/coastal_use_permits/coastal_use_permits_raster.tif", 
                    overwrite = T
                  )
saveRDS(cup_r, "data/coastal_use_permits/coastal_use_permits_raster.rds") 

rm(buffer_lst, buff, cup_path, cup_poly, cup_r, cup_union, cup_simple, ext, empty_r, buff_df, crs)









