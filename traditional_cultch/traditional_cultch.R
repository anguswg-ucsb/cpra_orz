# Angus Watters
# Lynker
# Traditional Cultch Model for Oyster Resource Zones
# TRADITIONAL CULTCH MODEL

# The traditional cultch model to be used in development of oyster resource zones generally follows the same structure as the eastern oyster habitat suitability index model used for the 2023 Coastal Master plan with the exception that the cultch variable has been removed. See the Habitat Suitability Index Model Improvements technical report available at https://coastal.la.gov/wp-content/uploads/2021/04/HSI-Model-Improvements_April2021.pdf for more details. The specific relationships for each variable have been extracted from the above report.

# HSI = (SI2 x SI3 x SI4 x SI5 x SI6)^1/5

rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)

source("utils/utils.R")

crs <- CRS('+init=EPSG:26915')

# read in all MP2023 files
mp_files     <- paste0("data/mp2023/raster/", list.files("data/mp2023/raster/", pattern = "S0"))

# Sediment Deposition 10 year mean
# sed_dep      <- readRDS("data/sediment_dep/sediment_deposition.rds")

# land polygon for masking
land_files     <- paste0("data/masks/",list.files("data/masks/", pattern = "land_polygon_S0"))
# land         <- readRDS("data/masks/land_polygon_07.rds")

# dataframe of paths
path_df <- data.frame(
  mp_path = mp_files
  ) %>% 
  mutate(
    id = 1:n(),
    land_path = case_when(
      id <= 10           ~ land_files[1],
      id > 10 & id <= 21 ~ land_files[2],
      id > 21 & id <= 42 ~ land_files[3]
      )
    )

 # Iterate through each MP year and calculate Traditional Cultch model, then save to disk
for (i in 1:length(mp_files)) {
  
  year <- substr(mp_files[i], 42, 46)
  run  <- substr(mp_files[i], 38, 40)
  
  logger::log_info("Calculating traditional cultch {run} year {year}")
  
  if (run == "S07") {
    # Load data for a MP2023 year
    mp      <- raster::stack(mp_files[i])
    
    # Calculate HSI for MP year and return a raster stack with SI components and HSI
    mp_hsi  <- get_trad_cultch(
      mp = mp  # sed_dep = sed_dep
    )
    
    # path to land polygon
    land_path <- path_df[i,3]
    
    # read in land polygon for mask
    land      <- readRDS(land_path)
    
    mp_hsi    <-  mp_hsi %>% 
      raster::stack() %>%
      mask(land, inverse = T) %>%
      round(4) %>%
      terra::rast()
    
    # save raster
    writeRaster(
      mp_hsi,
      paste0("data/traditional_cultch/S07/traditional_cultch_", run, "_", year, ".tif"),
      # paste0("data/traditional_cultch/traditional_cultch_", substr(mp_files[i], 38, 39), ".tif"),
      overwrite = T
    )
  } else if(run == "S08") {
    
    # Load data for a MP2023 year
    mp      <- raster::stack(mp_files[i])
    
    # Calculate HSI for MP year and return a raster stack with SI components and HSI
    mp_hsi  <- get_trad_cultch(
      mp = mp  # sed_dep = sed_dep
    )
    
    # path to land polygon
    land_path <- path_df[i,3]
    
    # read in land polygon for mask
    land      <- readRDS(land_path)
    
    mp_hsi    <-  mp_hsi %>% 
      raster::stack() %>%
      mask(land, inverse = T) %>%
      round(4) %>%
      terra::rast()
    
    # save raster
    writeRaster(
      mp_hsi,
      paste0("data/traditional_cultch/S08/traditional_cultch_", run, "_", year, ".tif"),
      # paste0("data/traditional_cultch/traditional_cultch_", substr(mp_files[i], 38, 39), ".tif"),
      overwrite = T
    )
  }
  
  # year    <- substr(mp_files[[i]], 38, 39)
  # final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"
  # 
  # writeRaster(mp_hsi,
  #             filename  = paste0(
  #             final_save, "traditional_cultch/traditional_cultch_", names(mp_hsi), "_", year, ".tif"),
  #             bylayer   = TRUE,
  #             overwrite = T)
}

# ---- Calculate Mean and Standard Dev. ----

s07_files <- paste0(
  "data/traditional_cultch/S07/", 
  list.files(paste0("data/traditional_cultch/", 
                    list.files("data/traditional_cultch/"))[1])
  )

s08_files <- paste0(
  "data/traditional_cultch/S08/", 
  list.files(paste0("data/traditional_cultch/", 
                    list.files("data/traditional_cultch/"))[2])
  )
# trad_cultch_files <- paste0("data/traditional_cultch/", list.files("data/traditional_cultch/"))

# empty lists to iteratively add to 
s07_lst  <- list()

# S07
for (i in 1:length(s07_files)) {
  
  logger::log_info("reading {s07_files[i]}")
  
  r <- raster::stack(s07_files[i])
  
  s07_lst[[i]]  <- r$hsi
  
}
# empty lists to iteratively add to 
s08_lst  <- list()
 
# S08
for (i in 1:length(s08_files)) {
  
  logger::log_info("reading {s08_files[i]}")
  
  r <- raster::stack(s08_files[i])
  
  s08_lst[[i]]  <- r$hsi
 
}

# ---- S07 HSI layers ----
s07_stk   <- raster::stack(s07_lst)

# S07 Years 1-10
s07_1_10  <- s07_stk[[1:10]]

# S07 Years 20-30
s07_20_30 <- s07_stk[[11:21]]

# S07 HSI mean years 1-10
s07_1_10_mean <- s07_1_10 %>% 
  calc(mean) %>%
  setNames(c("traditional_cultch_mean_s07_01_10"))

# S07 HSI Standard dev. years 1-10
s07_1_10_sd <- s07_1_10 %>% 
  calc(sd) %>%
  setNames(c("traditional_cultch_sd_s07_01_10"))

# S07 1-10 mean and SD
# ors_s07_1_10 <- stack(s07_1_10_mean, s07_1_10_sd)

# S07 HSI mean years 20-30
s07_20_30_mean <- s07_20_30 %>% 
  calc(mean) %>%
  setNames(c("traditional_cultch_mean_s07_20_30"))

# S07 HSI Standard dev. years 20-30
s07_20_30_sd <- s07_20_30 %>% 
  calc(sd) %>%
  setNames(c("traditional_cultch_sd_s07_20_30"))

# S07  20-30 mean and SD
# ors_s07_20_30 <- stack(s07_20_30_mean, s07_20_30_sd)

# ---- S08 HSI layers ----
s08_stk   <- raster::stack(s08_lst)

# S08 Years 1-10
s08_1_10  <- s08_stk[[1:10]]

# S08 Years 20-30
s08_20_30 <- s08_stk[[11:21]]

# S08 HSI mean years 1-10
s08_1_10_mean <- s08_1_10 %>% 
  calc(mean) %>%
  setNames(c("traditional_cultch_mean_s08_01_10"))

# S08 HSI Standard dev. years 20-30
s08_1_10_sd <- s08_1_10 %>% 
  calc(sd) %>%
  setNames(c("traditional_cultch_sd_s08_01_10"))

# S08  1-10 mean and SD
# ors_s08_1_10 <- stack(s08_1_10_mean, s08_1_10_sd)

# S08 HSI mean years 20-30
s08_20_30_mean <- s08_20_30 %>% 
  calc(mean) %>%
  setNames(c("traditional_cultch_mean_s08_20_30"))

# S08 HSI Standard dev. years 20-30
s08_20_30_sd <- s08_20_30 %>% 
  calc(sd) %>%
  setNames(c("traditional_cultch_sd_s08_20_30"))

# S08 20-30 mean and SD
# ors_s08_20_30 <- stack(s08_20_30_mean, s08_20_30_sd)

# save ORS mean
writeRaster(
  s07_1_10_mean,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_mean_s07_01_10.tif",
  overwrite = T
)

# save ORS mean
writeRaster(
  s07_20_30_mean,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_mean_s07_20_30.tif",
  overwrite = T
)

writeRaster(
  s07_1_10_sd,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_sd_s07_01_10.tif",
  overwrite = T
)

# save ORS mean
writeRaster(
  s07_20_30_sd,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_sd_s07_20_30.tif",
  overwrite = T
)


# save ORS mean
writeRaster(
  s08_1_10_mean,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_mean_s08_01_10.tif",
  overwrite = T
)

# save ORS mean
writeRaster(
  s08_20_30_mean,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_mean_s08_20_30.tif",
  overwrite = T
)

# save ORS mean
writeRaster(
  s08_1_10_sd,
  "data/oyster_resource_suitability/traditional_cultch/traditional_cultch_sd_s08_01_10.tif",
  overwrite = T
)

# save ORS mean
writeRaster(
  s08_20_30_sd,
  "data/oyster_resource_suitability/traditional_cultch_sd_s08_20_30.tif",
  overwrite = T
)


# writeRaster(
#   trad_cultch_mean,
#   "data/oyster_resource_suitability/traditional_cultch_s07_1_10_sd.tif",
#   overwrite = T
# )
# 
# # save ORS standard deviation
# writeRaster(
#   trad_cultch_sd,
#   "data/oyster_resource_suitability/traditional_cultch_standard_dev.tif",
#   overwrite = T
# )


# ***********************************************************


# for (i in 1:length(trad_cultch_files)) {
#   
#   logger::log_info("reading {trad_cultch_files[i]}")
#   
#   r <- raster::stack(trad_cultch_files[i])
#   
#   tc_lst[[i]]  <- r$hsi
#   
# }


# # Traditional cultch 10 year HSI mean
# trad_cultch_mean <- raster::stack(tc_lst) %>% 
#   calc(mean) %>%
#   setNames(c("traditional_cultch_mean"))
# 
# # Traditional cultch 10 year HSI standard deviation
# trad_cultch_sd <- raster::stack(tc_lst) %>% 
#   calc(sd) %>%
#   setNames(c("traditional_cultch_standard_dev"))


# final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"
# 
# writeRaster(trad_cultch_mean,
#             filename  = paste0(
#               final_save,
#               "traditional_cultch/traditional_cultch_mean.tif"
#             ),
#             overwrite = T
# )
# 
# writeRaster(trad_cultch_sd,
#             filename  = paste0(
#               final_save,
#               "traditional_cultch/traditional_cultch_standard_dev.tif"
#             ),
#             overwrite = T
# )


# save ORS mean
writeRaster(
  trad_cultch_mean,
  "data/oyster_resource_suitability/traditional_cultch_mean.tif",
  overwrite = T
)

# save ORS standard deviation
writeRaster(
  trad_cultch_sd,
  "data/oyster_resource_suitability/traditional_cultch_standard_dev.tif",
  overwrite = T
)

trad_cultch_sd <- raster::raster("data/oyster_resource_suitability/traditional_cultch_standard_dev.tif")
trad_cultch_mean <- raster::raster(  "data/oyster_resource_suitability/traditional_cultch_mean.tif")

hsi <- raster::stack("data/traditional_cultch/traditional_cultch_08.tif")

hsi_norm <- (hsi$hsi - trad_cultch_mean)/trad_cultch_sd
mapview::mapview(hsi_norm) + hsi$hsi
rm(mp, mp_hsi, sed_dep)









