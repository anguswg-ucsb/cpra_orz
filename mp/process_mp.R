# Angus Watters
# Lynker
# Read in MP CSVs and convert to raster (TIF files)

library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

source("utils/utils.R")

crs <- CRS('+init=EPSG:26915')

# read in grid
hsi_grid <- readRDS("data/mp2023/grid/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.rds") %>%
  rename(GridID = CELLID)

mp_files <- list.files("data/mp2023/csv/", pattern = ".rds")

mp_lst <- list()

# loop through and read in MP2023 CSVs and add to a list
for (i in 1:length(mp_files)) {
  
  # MP year being read in
  year <- substr(mp_files[i], 36, 40)
  
  logger::log_info("Reading MP CSV - year {year}")
  
  # Read MP CSV and add year column
  df <- readRDS(
              file      = paste0("data/mp2023/csv/", mp_files[i])
              ) %>%
    mutate(
      year = substr(mp_files[i], 36, 40)
      )
  
  # add to list of MP data
  mp_lst[[i]] <- df
}

rm(mp_data, mp_grid, mp_raster_lst, r_mp, r, rsmp_r, st_lst, mp_rsmp)

mp_raster_lst <- list()
# rm(mp_data, mp_grid, mp_raster_lst, r_mp, r, rsmp_r, st_lst)

# loop to create raster stack for each MP2023 year

# loops through each MP year
for (i in 1:length(mp_lst)) {
  logger::log_info("MP year {i}")
  
  mp_data     <- mp_lst[[i]]                              # load MP year CSV
  mp_grid     <- left_join(hsi_grid, mp_data, by = "GridID")  # Join year w/ MP grid polygon
  
  data_names  <- colnames(mp_grid)[3:8]                   # MP variables to rasterize
  
  st_lst <- list()                                        # empty list to add raster layers too
  
  # loops through 6 columns of MP data and rasterize
  for (z in 1:length(data_names)) {
    logger::log_info("raster layer {z}")
    
    # CRS
    crs <- CRS('+init=EPSG:26915')
    
    # Template raster to rasterize data onto
    r <- raster(                                          
      crs = crs,
      ext = extent(hsi_grid),
      res = c(480, 480)
    )
    
    # rasterize current variable
    r_mp <- fasterize::fasterize(mp_grid, r, field = data_names[z])

    # raster for resample to fit other data grids
    # rsmp_r <- raster(nrows = 452, ncols = 1052, crs = CRS('+init=EPSG:26915'),ext = extent(404980, 909940, 3199810, 3416770))
    
    # resample MP2023 data
    # mp_rsmp <- resample(r_mp, rsmp_r)
    
    # add variable to current years list of rasters
    st_lst[[z]] <- r_mp
  }
  
  # stack 6 year layers into raster stack
  stk <- raster::stack(st_lst)
  stk <- stk %>%
    setNames(data_names)     # add layer names
  
  mp_raster_lst[[i]] <- stk  # add raster stacks to final list of all years
}


# year labels for saving
yrs <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")

# iterate through years and save raster stacks to data/mp2023/raster/ 
for (i in 1:length(yrs)) {
  
  logger::log_info("saving MP2023 raster - year {yrs[i]}")

  # MP2023 year raster stack
  mp <- mp_raster_lst[[i]] 
  
  # resample 
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs = CRS('+init=EPSG:26915'),
    ext = extent(405220, 909700, 3199570, 3416530)
  )
  
  mp_resample   <- resample(mp, resamp_r) %>%
    terra::rast()
  
  # save raster stack
  writeRaster(
              mp_resample, 
              paste0("data/mp2023/raster/mp2023_oyster_hsi_", yrs[i], ".tif"),
              overwrite = T
            )
  rm(mp_resample, mp)
  # saveRDS(year_stk, paste0("data/mp2023/raster/mp2023_oyster_hsi_yr_", yrs[i], ".rds"))
}
