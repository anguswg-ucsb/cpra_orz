# Angus Watters
# Lynker
# Create commercial viability layers
rm(list = ls())

library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)
library(XML)

source("utils/utils.R")


# water file paths
water_files     <- grep(
                    "/water_S0",
                    paste0("data/masks/",list.files("data/masks/", pattern = "water_S0")),
                    value = T
                  )

# land file paths
land_files      <- paste0("data/masks/",list.files("data/masks/", pattern = "land_polygon_S0"))

# Distance to roads
roads_files     <- paste0("data/roads/", list.files("data/roads/", pattern = "road_buffer_raster.rds"))

# Fetch files
fetch_files     <- paste0("data/fetch/", list.files("data/fetch/", pattern = ".tif"))

# Depth 
depth_files     <- grep(
                    ".rds",
                    paste0("data/depth/", list.files("data/depth/", pattern = "water_depth_S0")),
                    value = T
                  )

# Sed dep files
sed_dep_files   <- grep(
                    ".tif",
                    paste0("data/sediment_dep/", list.files("data/sediment_dep/", pattern = "sediment_deposition_S")),
                    value  = T
                  )

i <- 1
roads    <- readRDS(file_df$road_path[i])
file_df <- data.frame(
  water_path = water_files,
  land_path  = land_files,
  road_path  = roads_files,
  fetch_path = fetch_files,
  depth_path = depth_files
  )

file_df[4,] <- file_df[1,]

file_df <-
  file_df %>% 
  mutate(
    run   = c("S07", "S07", "S08", "S08"),
    range = c("01_10", "20_30", "20_30", "01_10"),
    id    = c(1, 2, 4, 3)
  ) %>% 
  arrange(id) %>% 
  mutate(
    sed_dep_path = sed_dep_files,
    mp_file      = c(
      "MP2023_S07_G500_C000_U000_V000_SLA_0_03_03_W_lndtyp.tif", 
      "MP2023_S07_G500_C000_U000_V000_SLA_0_27_27_W_lndtyp.tif",
      "MP2023_S07_G500_C000_U000_V000_SLA_0_03_03_W_lndtyp.tif", 
      "MP2023_S08_G500_C000_U000_V000_SLA_0_27_27_W_lndtyp.tif"
    ), 
    oyster_csv = c(
      "MP2023_S07_G500_C000_U00_V00_SLA_O_10_10_X_OYSTE.csv",
      "MP2023_S07_G500_C000_U00_V00_SLA_O_27_27_X_OYSTE.csv",
      "MP2023_S08_G500_C000_U00_V00_SLA_O_10_10_X_OYSTE.csv",
      "MP2023_S08_G500_C000_U00_V00_SLA_O_27_27_X_OYSTE.csv"
                   )
  ) %>% 
  dplyr::select(water_path, land_path, road_path, fetch_path, depth_path, sed_dep_path, mp_file, oyster_csv, run, range)


# i = 3
# for (i in 1:length(sed_dep_files)) {
for (i in 1:nrow(file_df)) {
  # run      <- substr(water_files[i], 18, 26)
  run <- paste0(file_df$run[i],"_", file_df$range[i])

  logger::log_info("Calculating Commercial Viability - {run}")

  water    <- readRDS(file_df$water_path[i])
  land     <- readRDS(file_df$land_path[i])
  roads    <- readRDS(file_df$road_path[i])
  fetch    <- raster::raster(file_df$fetch_path[i])
  depth    <- readRDS(file_df$depth_path[i])
  sed_dep  <- raster::raster(file_df$sed_dep_path[i])

  commercial_viability <- get_cv(
    water   = water,
    land    = land,
    roads   = roads,
    fetch   = fetch,
    sed_dep = sed_dep, 
    depth   = depth
  )
  
  rds_path <- paste0("data/commercial_viability/commercial_viability_", run, ".rds")
  tif_path <- paste0("data/commercial_viability/commercial_viability_", run, ".tif")
  
  # logger::log_info("Saving:\n{rds_path}\n{tif_path}")

  # # save output
  # saveRDS(
  #   stack(commercial_viability), 
  #   rds_path
  #   )
  # 
  # writeRaster(
  #   commercial_viability, 
  #   tif_path,
  #   overwrite = T)
  
  commercial_viability <- commercial_viability[[1:6]] %>% 
    stack()
  
  # save individual rasters
  final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"
  save_txt <- paste0(final_save, "commercial_viability/", names(commercial_viability), "_",  run,".tif" )
  
  logger::log_info("Saving:\n{save_txt}")


  
  writeRaster(commercial_viability,
              filename  = paste0(final_save, "commercial_viability/", names(commercial_viability), "_",  run,".tif" ),
              bylayer   = TRUE, 
              overwrite = T 
  )
  
  # Write meta data files 
  meta_path      <-  paste0("data/metadata/", names(commercial_viability), "_",  run, ".xml" )
  template_path  <- "data/metadata/template/metadata_template.xml"
  # meta_outline     <- data.frame(
  #   path_name = paste0("C:/Users/angus/OneDrive/Desktop/metadata/", list.files("C:/Users/angus/OneDrive/Desktop/metadata/")),
  #   file_name = list.files("C:/Users/angus/OneDrive/Desktop/metadata/")
  #   )
  #                                )
  # cv_txt <- meta_outline %>% 
  #   filter(file_name == "cv.xml")
  # temp_x     <- readLines(cv_txt$path_name)
  # temp_x[16]

  # Abstracts
  si_fetch_shallow_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in shallow water (assumes smaller boats without refrigeration capability) based on fetch distance. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft. Data was sourced from the ",   file_df$mp_file[i], " file from the CMP 2023 Outputs. Scenario ", substr(run, 2, 3),  " years ",
    substr(run, 5, 6), " - ",  substr(run, 8, 9), "</abstract>")
  
  si_fetch_deep_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in deep water (assumes larger boats with refrigeration capability) based on fetch distance. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft. Data was sourced from the ",  file_df$mp_file[i], " file from the CMP 2023 Outputs. Scenario ",
                             substr(run, 2, 3),  " years ", substr(run, 5, 6), " - ",  substr(run, 8, 9), "</abstract>")
  
  si_roads_abs  <- paste0(
    "<abstract>Straight line distance (km) to nearest road. Provides a metric for ease of access. The 2020 Census Tiger/Line shapefile for primary and secondary roads was used to estimate distance to roads (access)</abstract>")
  
  si_sed_dep_abs  <- paste0(
    "<abstract>Sediment deposition rate. Provides a measure of AOC level of effort to reduce effects of sedimentation. Data was sourced from the ", file_df$oyster_csv[i], " file from the CMP 2023 Outputs. Scenario ", substr(run, 2, 3),  " years ",
    substr(run, 5, 6), " - ",  substr(run, 8, 9), "</abstract>")
  
  cv_shallow_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in shallow water (assumes smaller boats without refrigeration capability) based on fetch and sediment deposition. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft. Scenario ", substr(run, 2, 3),  " years ",
    substr(run, 5, 6), " - ",  substr(run, 8, 9), "</abstract>")
  
  cv_deep_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in deep water (assumes larger boats with refrigeration capability) based on fetch, sediment deposition and distance to roads. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft. Scenario ", substr(run, 2, 3),  " years ",
    substr(run, 5, 6), " - ",  substr(run, 8, 9), "</abstract>")
  
  # Titles
  layer_titles <- paste0("<title>", names(commercial_viability), "_", run, "</title>")
  
  # Abstract/titles to iterate through layers
  meta_df <- data.frame(
    layer1        = si_fetch_shallow_abs,
    layer2        = si_fetch_deep_abs,
    layer3        = si_roads_abs,
    layer4        = si_sed_dep_abs,
    layer5        = cv_shallow_abs,
    layer6        = cv_deep_abs
  ) %>% 
    pivot_longer(
      cols      = everything(),
      names_to  = "id", 
      values_to = "abstract"
    ) %>% 
    mutate(title = layer_titles)
 
  for (j in 1:length(meta_path)) {
    
    logger::log_info("Generating ESRI metadata - {meta_path[j]}")
    abstract <- meta_df$abstract[j]
    title    <- meta_df$title[j]
    
    temp_x     <- readLines(template_path)
    temp_x[12] <- abstract
    temp_x[7]  <- title
    
    xml <- as.character(temp_x)
    
    code <- paste0(as.character(xml), collapse = "\n")
    
    write.table(code,
                file = meta_path[j],
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE) 
  }
}

save_path <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"
# resave_files <- paste0("data/oyster_resource_suitability/aoc/", list.files("data/oyster_resource_suitability/aoc/"))
resave_files <- list.files("data/commercial_viability/", pattern = ".tif")

for (i in 1:length(resave_files)) {

  r   <- raster::stack(paste0("data/commercial_viability/", resave_files[i]))
  run <- substr(resave_files[i], 22, 30)
  r   <- r[[1:6]]
  tif_path <- paste0("C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/commercial_viability/", names(r), "_", run, ".tif")
  logger::log_info("saving {resave_files[i]} @ {tif_path}")
  
  writeRaster(
    r,
    filename = tif_path,
    overwrite = T,
    bylayer   = T,
    format    = "GTiff"
  )
}


save_path <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"
# resave_files <- paste0("data/oyster_resource_suitability/aoc/", list.files("data/oyster_resource_suitability/aoc/"))
resave_files <- list.files("data/aoc/")

for (i in 1:length(resave_files)) {
  r <- raster::stack(paste0("data/aoc/", resave_files[i]))
  run <- substr(resave_files[i], 5, 13)
  r <- r[[1:2]]
  tif_path <- paste0("C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/", names(r), "_", run, ".tif")
  logger::log_info("saving {resave_files[i]} @ {tif_path}")
  
  writeRaster(
    r,
    filename = tif_path,
    overwrite = T,
    bylayer   = T,
    format    = "GTiff"
  )
}











