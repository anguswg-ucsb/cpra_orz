# Angus Watters
# Lynker
# Clean and buffer CPRA restoration and FWOA polygons and create raster

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(progress)

path <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/"
cpra_path <- "cpra_projects/project_poly/shapefile/"
dashboard_path <- "C:/Users/angus/OneDrive/Desktop/github/ORZ_dashboard/"

crs <- CRS('+init=EPSG:26915')
ext <- extent(c(405220, 909700, 3199570, 3416530))

list.files(paste0(path, cpra_path))


# **************************************
# ---- CPRA infrastructure polygons ----
# **************************************

# missing projects
out <- readr::read_csv("data/cpra_projects/cpra_polys_select_by_location.csv") %>%
  mutate(YEAR = as.character(YEAR))

# # missing project IDs
miss_id <- unique(out$PROJ_ID)

# CPRA infrastructure polygons
inf_poly <- sf::read_sf("data/cpra_projects/CPRA_Inf_Polys.shp") %>%
  st_transform(26915) %>%
  st_make_valid() # make valid polygons
# 
out_polys <- inf_poly %>%
  filter(PROJ_ID %in% miss_id) %>%
  janitor::clean_names()

# sf::write_sf(inf_poly_valid, paste0(path, cpra_path,  "CPRA_Inf_Polys_valid.shp"))

# filter constructed projects
inf_poly <- inf_poly %>%
  filter(STRUC_STAT == "Constructed") %>%
  janitor::clean_names()

# join Constructed Inf projects & missing projects
inf_poly <- bind_rows(inf_poly, out_polys)

# **************************************
# ----- CPRA infrastructure lines ------
# **************************************

# read in data
inf_line <- sf::read_sf("data/cpra_projects/CPRA_Inf_Lines.shp") %>%
  st_transform(26915)

out_line <- inf_line %>%
  filter(PROJ_ID %in% miss_id) %>%
  janitor::clean_names()

# filter constructed projects
inf_line <- inf_line %>%
  filter(STRUC_STAT == "Constructed") %>%
  janitor::clean_names()

# join Constructed Inf proj lines & missing proj lines
inf_line <- bind_rows(inf_line, out_line)


# **************************************
# ----- CPRA infrastructure point ------
# **************************************

inf_pts <- sf::read_sf("data/cpra_projects/CPRA_Inf_Points.shp")  %>%
  st_transform(26915)

out_pts <- inf_pts %>%
  filter(PROJ_ID %in% miss_id) %>%
  janitor::clean_names()

# filter constructed projects
inf_pts <- inf_pts %>%
  filter(STRUC_STAT == "Constructed") %>%
  janitor::clean_names()

# join Constructed Inf proj lines & missing proj lines
inf_pts <- bind_rows(inf_pts, out_pts)


# *****************************
# ---- buffer CPRA projets ----
# *****************************

# buffer infrastructure polygon geom by 1500 ft
inf_poly_buffer <- inf_poly %>%
  st_buffer(1500/3.281) %>%
  mutate(type = "poly") %>%
  dplyr::select(proj_id:const_date, type)  %>%
  st_cast("MULTIPOLYGON")

# buffer infrastructure lines geom by 1500 ft
inf_line_buffer <- inf_line %>%
  st_buffer(1500/3.281) %>%
  mutate(type = "line") %>%
  dplyr::select(proj_id:const_date, type)  %>%
  st_cast("MULTIPOLYGON")

# buffer infrastructure points geom by 1500 ft
inf_pts_buffer <- inf_pts %>%
  st_buffer(1500/3.281) %>%
  mutate(type = "point") %>%
  dplyr::select(proj_id:const_date, type) %>%
  st_cast("MULTIPOLYGON")


# combine Polygons, lines, point buffers
inf_buffer <- bind_rows(inf_line_buffer, inf_poly_buffer, inf_pts_buffer)

# union all geometries
inf_buffer_union <- inf_buffer %>%
  group_by(proj_id) %>%
  summarize(geometry = st_union(geometry))


tmp <- inf_buffer %>% st_drop_geometry()
unique(tmp$struc_stat)

proj_names <- inf_buffer %>%
  st_drop_geometry() %>%
  # dplyr::select(proj_name, proj_id, struc_clas, struc_stat, const_date) %>%
  group_by(proj_id) %>%
  slice(n = 1)

inf_buffer_union <- inf_buffer_union %>%
  left_join(proj_names, by = "proj_id")


# ********************************
# ----- MP2023 FWOA projects -----
# ********************************



ogrListLayers("data/cpra_projects/MP23FWOAprojects20210811.gdb.zip")

fwoa_poly <- rgdal::readOGR("data/cpra_projects/MP23FWOAprojects20210811.gdb.zip", layer = "FWOA_polygons") %>%
  st_as_sf() %>%
  janitor::clean_names() %>%
  dplyr::select(proj_id, proj_name, program, sponsor,  struc_clas, struc_stat)

fwoa_line <- rgdal::readOGR("data/cpra_projects/MP23FWOAprojects20210811.gdb.zip", layer = "FWOA_lines") %>%
  st_as_sf() %>%
  janitor::clean_names() %>%
  dplyr::select(proj_id, proj_name, program, sponsor,  struc_clas, struc_stat)

fwoa_pt <- rgdal::readOGR("data/cpra_projects/MP23FWOAprojects20210811.gdb.zip", layer = "FWOA_points") %>%
  st_as_sf() %>%
  janitor::clean_names() %>%
  dplyr::select(proj_id, proj_name, program, sponsor,  struc_clas, struc_stat)

fwoa_proj <- bind_rows(fwoa_poly, fwoa_line, fwoa_pt)

rm(fwoa_pt, fwoa_line, fwoa_poly, inf_line, inf_line_buffer, inf_poly, inf_poly_buffer, inf_pts_buffer, inf_pts)

fwoa_union <- fwoa_proj %>%
  group_by(proj_name) %>%
  summarise(geometry = st_union(geometry))

# extract all columns to join with unioned FWOA projects
fwoa_names <- fwoa_proj %>%
  st_drop_geometry() %>%
  group_by(proj_name) %>%
  slice(n = 1)

fwoa_union <- fwoa_union %>%
  left_join(fwoa_names, by = "proj_name") %>%
  dplyr::relocate(proj_id, proj_name, program, sponsor,  struc_clas, struc_stat, geometry)


# buffer FWOA polygons
fwoa_buffer <- fwoa_union %>%
  st_buffer(1500/3.281) %>%
  mutate(type = "fwoa") %>%
  # dplyr::select(proj_id:const_date, type)  %>%
  st_cast("MULTIPOLYGON")

# join FWOA and CPRA projs
fwoa <- fwoa_buffer %>%
  dplyr::select(proj_id, proj_name, type, struc_clas, struc_stat)

cpra_proj <- inf_buffer_union %>%
  dplyr::select(proj_id, proj_name, type, struc_clas, struc_stat)

# All projects (CPRA & FWOA)
all_proj <- bind_rows(fwoa, cpra_proj)

saveRDS(all_proj, "data/cpra_projects/cpra_fwoa_projects.rds")

# ***************************************
# ---- Rasterize CPRA projects layer ----
# ***************************************
cpra_projects <- readRDS("data/cpra_projects/cpra_fwoa_projects.rds") %>% 
  # st_cast("MULTIPOLYGON") %>%  
  summarise(geometry = st_union(geometry)) %>% 
  mutate(val = 1)

# resample raster to this grid 
empty_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)


cpra_projects_r <- fasterize::fasterize(
  sf      = cpra_projects, 
  raster  = empty_r,
  field   = "val", 
  fun     = "sum"
)
plot(cpra_projects_r$layer)

mapview(cpra_projects_r)

# save waterways buffer
raster::writeRaster(cpra_projects_r, "data/cpra_projects/cpra_fwoa_projects_raster.tif", overwrite = T)
saveRDS(cpra_projects_r, "data/cpra_projects/cpra_fwoa_projects_raster.rds")
saveRDS(cpra_projects_r, "C:/Users/angus/OneDrive/Desktop/github/cpra_dashboard/cpra_fwoa_projects_raster.rds")




























