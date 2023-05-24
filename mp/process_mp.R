# Angus Watters
# Lynker
# Read in MP CSVs and convert to raster (TIF files)

rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

source("utils/utils.R")

crs <- CRS('+init=EPSG:26915')

mp_csv_files <- list.files("data/mp2023/csv/", pattern = ".csv")

# loop through and read in MP2023 CSVs and resave as RDS files to reduce space on disk
for (i in 1:length(mp_csv_files)) {
  
  file_name <- gsub(".csv", ".rds", mp_csv_files[i])
  
  logger::log_info("Reading MP CSV - mp_csv_files[i]")
  
  # Read MP CSV 
  csv <- read_csv(
    file      = paste0("data/mp2023/csv/", mp_csv_files[i])
  )
  
  # save CSV as RDS
  saveRDS(csv, paste0("data/mp2023/csv/", file_name))
}

# read in grid
hsi_grid <- readRDS("data/mp2023/grid/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.rds") %>%
  rename(GridID = CELLID)

mp_files <- list.files("data/mp2023/csv/", pattern = ".rds")

mp_lst <- list()

# loop through and read in MP2023 CSVs and add to a list
for (i in 1:length(mp_files)) {
  
  # MP run being read in
  run <- substr(mp_files[i], 8, 10)
  
  # MP year being read in
  year <- substr(mp_files[i], 36, 40)
  
  logger::log_info("Reading MP CSV - {run} year {year}")
  
  # Read MP CSV and add year column
  df <- readRDS(
              file      = paste0("data/mp2023/csv/", mp_files[i])
              ) %>%
    mutate(
      run  = run,
      year = year
      ) %>% 
    dplyr::relocate(GridID, HSI, s_spwn, smin_w, smin_c, s_mean, pct_land, sedim, run, year)
  
  # add to list of MP data
  mp_lst[[i]] <- df
}

rm(mp_data, mp_grid, mp_raster_lst, r_mp, r, rsmp_r, st_lst, mp_rsmp)

mp_raster_lst <- list()
# rm(mp_data, mp_grid, mp_raster_lst, r_mp, r, rsmp_r, st_lst)

# loop to create raster stack for each MP2023 year

# loops through each MP year
for (i in 1:length(mp_lst)) {

  # read in MP CSV
  mp_data     <- mp_lst[[i]]                              # load MP year CSV
  
  # modal run/year
  run         <- mp_data$run[1]
  year        <- mp_data$year[1]
  
  logger::log_info("MP {run} year {year}")
  
  mp_grid     <- left_join(                               # Join year w/ MP grid polygon
                        hsi_grid,
                        mp_data, 
                        by = "GridID"
                        ) 
  
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

# iterate through years and save raster stacks to data/mp2023/raster/ 
for (i in 1:length(mp_lst)) {
  # MP2023 year raster stack
  mp_r   <- mp_raster_lst[[i]] 
  
  mp_id  <- mp_lst[[i]]
  run    <- mp_id$run[1]
  year   <- mp_id$year[1]
  
  logger::log_info("saving MP2023 raster {run} {year}")
  
  # resample 
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs   = CRS('+init=EPSG:26915'),
    ext   = extent(405220, 909700, 3199570, 3416530)
  )
  
  mp_resample   <- resample(mp_r, resamp_r) %>%
    terra::rast()
  
  # save raster stack
  writeRaster(
              mp_resample, 
              paste0("data/mp2023/raster/mp2023_oyster_hsi_", run, "_", year, ".tif"),
              # paste0("data/mp2023/raster/mp2023_oyster_hsi_", yrs[i], ".tif"),
              overwrite = T
            )
  
  rm(mp_resample, mp_r, mp_id, resamp_r, run, year)
  # saveRDS(year_stk, paste0("data/mp2023/raster/mp2023_oyster_hsi_yr_", yrs[i], ".rds"))
}
