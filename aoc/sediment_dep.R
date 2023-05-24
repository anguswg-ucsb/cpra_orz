# Angus Watters
# Lynker
# Calculate mean sediment deposition over the 10 years of MP data
rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)

#

# ---- Calculate average Sediment Dep of  MP2023 10 years ----

# read in all MP2023 files
mp_files <- paste0("data/mp2023/raster/", list.files("data/mp2023/raster/", pattern = "S0"))

# dataframe of paths
path_df <- data.frame(
  mp_path = mp_files
) %>% 
  mutate(
    id = 1:n(),
    run_years = case_when(
      id <= 10           ~ "S07_01_10" ,
      id > 10 & id <= 21 ~ "S07_20_30",
      id > 21 & id <= 31 ~ "S08_01_10",
      id > 31 & id <= 42 ~ "S08_20_30"
    )
  ) %>% 
  group_by(run_years) %>% 
  group_split()

sed_dep_lst <- list()

# i <- 1
for (i in 1:length(path_df)) {
  
  run <- path_df[[i]]
  
  run_years <- run$run_years[1]
  
  sed_dep_lst <- list()
  
  logger::log_info("Processing {run_years}")
  
  # k <- 1
  for (k in 1:length(run$mp_path)) {
    
    # read in MP2023 year
    mp <- raster::stack(run$mp_path[k])
    # plot(mp)
    
    # mp$sedim
    # 
    # plot(mp$sedim)
    # rr <- raster::clamp(mp$sedim, upper = 4)
    # hist(values(rr), breaks = 30)
    # plot(rr)
    # MP run being read in
    run_txt  <- substr(run$mp_path[k], 38, 40)
    
    # MP year being read in
    year_txt <- substr(run$mp_path[k], 42, 46)
    
    
    logger::log_info("Converting sediment deposition to millimeters - {run_txt} year {year_txt}")
    
    # convert to millimeters
    sed_dep <- mp$sedim*10
    
    sed_dep_lst[[k]] <- sed_dep
    
  }
  
  # resample raster to grid 
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs   = CRS('+init=EPSG:26915'),
    ext   = extent(405220, 909700, 3199570, 3416530)
  )
  
  logger::log_info("Calculating mean sediment deposition {run_years}")
  logger::log_info("Resampling")
  
  # calculate mean sediment deposition
  sed_dep_mean <- sed_dep_lst %>% 
    stack() %>% 
    calc(mean) %>% 
    resample(resamp_r) %>% 
    setNames(c(paste0("sediment_deposition_", run_years)))
  
  
  save_path <- paste0("data/sediment_dep/sediment_deposition_", run_years, ".rds")
  
  logger::log_info("Saving: \n{save_path}")
  
  # save to save_path
  saveRDS(sed_dep_mean, save_path)
  
}



# #  loop though MP2023 years and calculate salinity SI for AOC and save to disk
# for (i in 1:length(mp_files)) {
#   
#   # MP run being read in
#   run <- substr(mp_files[i], 19, 21)
#   
#   # MP year being read in
#   year <- substr(mp_files[i], 23, 27)
#   
#   logger::log_info("MP {run} year {year}")
#   
#   # read in MP2023 year
#   mp <- raster::stack(paste0("data/mp2023/raster/", mp_files[i]))
#   
#   
#   # convert to millimeters
#   sed_dep <- mp$sedim*10
#   
#   sed_dep_lst[[i]] <- sed_dep
# }

# # stack rasters
# sed_dep_stk  <- raster::stack(sed_dep_lst)
# 
# # resample raster to grid 
# resamp_r <- raster(
#   nrows = 452,
#   ncols = 1051,
#   crs   = CRS('+init=EPSG:26915'),
#   ext   = extent(405220, 909700, 3199570, 3416530)
# )
# 
# # Mean sediement desposition S07 years 1-10
# sed_dep_s07_10 <- sed_dep_stk[[1:10]] %>% 
#   calc(mean) %>% 
#   resample(resamp_r)

# # save
# writeRaster(sed_dep_s07_10, "data/sediment_dep/sediment_deposition_s07_01_10.tif", overwrite = T)
# saveRDS(sed_dep_s07_10, "data/sediment_dep/sediment_deposition_s07_01_10.rds")
# 
# # Mean sediement desposition S07 years 20-30
# sed_dep_s07_20 <- sed_dep_stk[[11:21]] %>% 
#   calc(mean) %>% 
#   resample(resamp_r)
# 
# # save
# writeRaster(sed_dep_s07_20, "data/sediment_dep/sediment_deposition_s07_20_30.tif", overwrite = T)
# saveRDS(sed_dep_s07_20, "data/sediment_dep/sediment_deposition_s07_20_30.rds")
# 
# # Mean sediement desposition S08 years 1-10
# sed_dep_s08_10 <- sed_dep_stk[[22:31]] %>% 
#   calc(mean) %>% 
#   resample(resamp_r)
# 
# # save
# writeRaster(sed_dep_s08_10, "data/sediment_dep/sediment_deposition_s08_01_10.tif", overwrite = T)
# saveRDS(sed_dep_s08_10, "data/sediment_dep/sediment_deposition_s08_01_10.rds")
# 
# # Mean sediement desposition S08 years 20-30
# sed_dep_s08_20 <- sed_dep_stk[[32:42]] %>% 
#   calc(mean) %>% 
#   resample(resamp_r)
# 
# # save
# writeRaster(sed_dep_s08_20, "data/sediment_dep/sediment_deposition_s08_20_30.tif", overwrite = T)
# saveRDS(sed_dep_s08_20, "data/sediment_dep/sediment_deposition_s08_20_30.rds")
# 
# 
# 
# writeRaster(sed_dep_mean, "data/sediment_dep/sediment_deposition.tif", overwrite = T)
# saveRDS(sed_dep_mean, "data/sediment_dep/sediment_deposition.rds")

rm(mp, resamp_r, sed_dep, sed_dep_lst, sed_dep_mean, sed_dep_stk, i, mp_files)

