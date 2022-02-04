# Calculate mean sediment deposition over the 10 years of MP data

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
mp_files <- list.files("data/mp2023/raster/")

sed_dep_lst <- list()

#  loop though MP2023 years and calculate salinity SI for AOC and save to disk
for (i in 1:length(mp_files)) {
  
  logger::log_info("Year {i}")
  
  # read in MP2023 year
  mp <- raster::stack(paste0("data/mp2023/raster/", mp_files[i]))
  
  # convert to millimeters
  sed_dep <- mp$sedim*10
  
  sed_dep_lst[[i]] <- sed_dep
}

sed_dep_stk  <- raster::stack(sed_dep_lst)
sed_dep_mean <- calc(sed_dep_stk, mean)

# resample raster to grid 
resamp_r <- raster(
  nrows = 452,
  ncols = 1051,
  crs = CRS('+init=EPSG:26915'),
  ext = extent(405220, 909700, 3199570, 3416530)
)

# resample
sed_dep_mean <- resample(sed_dep_mean, resamp_r)

saveRDS(sed_dep_mean, "data/sediment_dep/sediment_deposition.rds")

rm(mp, resamp_r, sed_dep, sed_dep_lst, sed_dep_mean, sed_dep_stk, i, mp_files)

