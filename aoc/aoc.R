# Angus Watters
# Lynker
# generate AOC layers
rm(list = ls())

library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(terra)

source("utils/utils.R")

# ---- AOC HSI model ----

# read in all MP2023 files
# ov_files <- list.files("data/oyster_viability/", pattern = ".tif")
ov_files <- grep(
  ".tif",
  paste0("data/oyster_viability/", list.files("data/oyster_viability", pattern = "oyster_viability_S")),
  value = T
  )

cv_files <- grep(
  ".tif",
  paste0("data/commercial_viability/", list.files("data/commercial_viability", pattern = "commercial_viability_S")),
  value = T
  )

# dataframe of paths
path_df <- data.frame(
  ov_path = ov_files
) %>% 
  mutate(
    id = 1:n(),
    cv_path = case_when(
      id <= 10           ~ cv_files[1],
      id > 10 & id <= 21 ~ cv_files[2],
      id > 21 & id <= 31 ~ cv_files[3],
      id > 31 & id <= 42 ~ cv_files[4]
    ),
    run  = substr(ov_path, 40, 42),
    year = substr(ov_path, 44, 48)
  ) %>% 
  dplyr::select(-id)


# Loop through Oyster viability rasters and calculate/save AOC layers for all 10 years of MP data
for (i in 1:length(path_df$ov_path)) {
  
  # run <- substr(path_df$ov_path[i], 40, 48)
  run      <- path_df$run[i]
  year     <- path_df$year[i]
  run_year <- paste0(run, "_", year)
  
  logger::log_info("Calculating AOC - {run_year}")

  # read in MP2023 year
  ov <- raster::stack(path_df$ov_path[i])
  cv <- raster::stack(path_df$cv_path[i])
  # ov <- raster::stack(paste0("data/oyster_viability/", ov_files[i]))

  aoc <- get_aoc(
    shallow_cv   = cv$shallow_cv, 
    deep_cv      = cv$deep_cv, 
    combine_cv   = cv$cv,
    ov           = ov$si_ov
  ) 
  # %>% 
  #   raster::stack()
  
  tif_path <-  paste0("data/aoc/aoc_", run_year, ".tif")
  rds_path <-  paste0("data/aoc/aoc_", run, ".rds")
  
  
  logger::log_info("Saving:\n{tif_path}")
  
  # save to ../data/aoc/
  writeRaster(
    aoc,
    tif_path,
    overwrite = T
  )
  
  # aoc <- aoc[[1:2]]
  # 
  # # save individual rasters
  # final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"
  # save_txt <- paste0(final_save, "aoc/", names(aoc), "_",  run_year,".tif" )
  # 
  # logger::log_info("Saving:\n{save_txt}")
  # 
  # 
  # writeRaster(
  #   stack(aoc),  
  #   filename = paste0(final_save,"aoc/",names(aoc), "_", run_year, ".tif" ),
  #   bylayer  = TRUE,
  #   overwrite = T
  #   )

  
  # Write meta data files 
  meta_path      <-  paste0("data/metadata/", names(aoc), "_",  run_year, ".xml" )
  template_path  <- "data/metadata/template/metadata_template.xml"
  # meta_outline     <- data.frame(
  #   path_name = paste0("C:/Users/angus/OneDrive/Desktop/metadata/", list.files("C:/Users/angus/OneDrive/Desktop/metadata/")),
  #   file_name = list.files("C:/Users/angus/OneDrive/Desktop/metadata/")
  #   )
  #                                
  # cv_txt <- meta_outline %>%
  #   filter(file_name == "aoc_shallow_01.xml")
  # temp_x     <- readLines(cv_txt$path_name)
  # temp_x[16]
  
  # Abstracts
  aoc_shallow_abs  <- paste0(
    "<abstract>AOC suitability index for shallow water operations (assumes smaller boats without refrigeration capability) based on oyster viability and commercial viability; year 1. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft. Scenario ", substr(run, 2, 3),  " year ", year, "</abstract>"
  )
  
  aoc_deep_abs  <- paste0(
    "<abstract>AOC suitability index for deep water operations (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; year 1. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft. Scenario ", substr(run, 2, 3),  " year ", year, "</abstract>"
    )
  
  
  # Titles
  layer_titles <- paste0("<title>", names(aoc), "_", run_year, "</title>")
  
  # Abstract/titles to iterate through layers
  meta_df <- data.frame(
    layer1        = aoc_shallow_abs,
    layer2        = aoc_deep_abs
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

# ---- Calculate AOC Mean & Standard Deviation ----

aoc_files <- paste0("data/aoc/", list.files("data/aoc/"))

# dataframe of paths
path_df <- data.frame(
                  aoc_files = aoc_files
                  ) %>% 
  mutate(
    id = 1:n(),
    range = case_when(
      id <= 10           ~ "01_10",
      id > 10 & id <= 21 ~ "20_30",
      id > 21 & id <= 31 ~ "01_10",
      id > 31 & id <= 43 ~ "20_30",
    ),
    run  = substr(aoc_files, 14, 16),
    year = substr(aoc_files, 18, 22)
  )  %>%
  dplyr::select(-id) %>% 
  group_by(range, run) %>% 
  group_split()

# i = 2
for (i in 1:length(path_df)) {
  
  paths <- path_df[[i]]
  range <- paths$range[1]
  run   <- paths$run[1]
  
  run_range <- paste0(run, "_", range)
  
  logger::log_info("Calculating AOC mean & standard deviation - {run} years {range}")
  
  # st_lst <- list()
  shallow_lst  <- list()
  deep_lst     <- list()
  
  logger::log_info("Loading:")

  for (z in 1:length(paths$aoc_files)) {
    
    logger::log_info("{paths$aoc_files[z]}")
    
    r <- raster::stack(paths$aoc_files[z])
    
    shallow_lst[[z]] <- r$aoc_shallow
    deep_lst[[z]]    <- r$aoc_deep
    
  }
  
  # AOC Shallow mean
  aoc_shallow_mean <- shallow_lst %>% 
    stack() %>% 
    calc(mean) %>%
    setNames(c(paste0("aoc_shallow_mean_", run_range)))

  # AOC Deep mean
  aoc_deep_mean    <- deep_lst %>% 
    stack() %>% 
    calc(mean) %>%
    setNames(c(paste0("aoc_deep_mean_", run_range)))
  
  # AOC Shallow Standard deviation
  aoc_shallow_sd   <- shallow_lst %>% 
    stack() %>% 
    calc(sd) %>%
    setNames(c(paste0("aoc_shallow_sd_", run_range)))
  
  # AOC Deep Standard deviation
  aoc_deep_sd      <- deep_lst %>% 
    stack() %>% 
    calc(sd) %>% 
    setNames(c(paste0("aoc_deep_sd_", run_range)))
  
  # tif_path <- paste0("data/oyster_resource_suitability/aoc/")
  aoc_stk  <- stack(aoc_shallow_mean, aoc_shallow_sd, aoc_deep_mean, aoc_deep_sd)
  
  # tif_path <- paste0("data/oyster_resource_suitability/aoc/", names(aoc_stk), ".tif")
  tif_path <- paste0("C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/", names(aoc_stk),".tif")
  logger::log_info("Saving - {tif_path}")
  
  writeRaster(
    aoc_stk,
    filename = tif_path,
    overwrite = T,
    bylayer  = T,
    format   = "GTiff"
  )
  # Write meta data files 
  meta_path      <-  paste0("data/metadata/", names(aoc_stk), ".xml" )
  template_path  <- "data/metadata/template/metadata_template.xml"
 
  # Abstracts
  aoc_shallow_mean_abs  <- paste0(
    "<abstract>AOC suitability index for shallow water operations (assumes smaller boats without refrigeration capability) based on oyster viability and commercial viability; Scenario " , substr(run, 2, 3),  " mean years ", substr(run_range, 5, 6), "-" , substr(run_range, 8, 9), ". Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft.</abstract>"
  )
  aoc_shallow_sd_abs  <- paste0(
    "<abstract>AOC suitability index for shallow water operations (assumes smaller boats without refrigeration capability) based on oyster viability and commercial viability; Scenario " , substr(run, 2, 3),  " standard deviation years ", substr(run_range, 5, 6), "-" , substr(run_range, 8, 9), ". Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft.</abstract>"
  )
  aoc_deep_mean_abs  <- paste0(
    "<abstract>AOC suitability index for deep water operations (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; Scenario " , substr(run, 2, 3),  " mean years ", substr(run_range, 5, 6), "-" , substr(run_range, 8, 9), ". Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft.</abstract>"
  )
  aoc_deep_sd_abs  <- paste0(
    "<abstract>AOC suitability index for deep water operations (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; Scenario " , substr(run, 2, 3),  " standard deviation years ", substr(run_range, 5, 6), "-" , substr(run_range, 8, 9), ". Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft.</abstract>"
  )
  
  # Titles
  layer_titles <- paste0("<title>", names(aoc_stk), "</title>")
  
  # Abstract/titles to iterate through layers
  meta_df <- data.frame(
    layer1        = aoc_shallow_mean_abs,
    layer2        = aoc_shallow_sd_abs,
    layer3        = aoc_deep_mean_abs,
    layer4        = aoc_deep_sd_abs
  ) %>% 
    pivot_longer(
      cols      = everything(),
      names_to  = "id", 
      values_to = "abstract"
    ) %>% 
    mutate(title = layer_titles)
  
  for (j in 1:length(meta_path)) {
    
    logger::log_info("Generating ESRI metadata - {meta_path[j]}")
    abstract   <- meta_df$abstract[j]
    title      <- meta_df$title[j]
    
    temp_x     <- readLines(template_path)
    temp_x[12] <- abstract
    temp_x[7]  <- title
    
    xml        <- as.character(temp_x)
    
    code       <- paste0(as.character(xml), collapse = "\n")
    
    write.table(
      code,
      file = meta_path[j],
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
      ) 
  }
}
# save_path <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"
# # resave_files <- paste0("data/oyster_resource_suitability/aoc/", list.files("data/oyster_resource_suitability/aoc/"))
# resave_files <- list.files("data/oyster_resource_suitability/aoc/")
# 
# for (i in 1:length(resave_files)) {
#   r <- raster::raster(paste0("data/oyster_resource_suitability/aoc/", resave_files[i]))
#   logger::log_info("saving {resave_files[i]}")
#   writeRaster(r, paste0(save_path, resave_files[i]))
# }
# 
# 
# save_path <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"
# # resave_files <- paste0("data/oyster_resource_suitability/aoc/", list.files("data/oyster_resource_suitability/aoc/"))
# resave_files <- list.files("data/aoc/")
# 
# for (i in 1:length(resave_files)) {
#   r <- raster::stack(paste0("data/aoc/", resave_files[i]))
#   run <- substr(resave_files[i], 5, 13)
#   r <- r[[1:2]]
#   tif_path <- paste0("C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/", names(r), "_", run, ".tif")
#   logger::log_info("saving {resave_files[i]} @ {tif_path}")
# 
#   writeRaster(
#     r,
#     filename = tif_path,
#     overwrite = T,
#     bylayer   = T,
#     format    = "GTiff"
#   )
# }
# # empty lists to iteratively add to 
# shallow_lst  <- list()
# deep_lst     <- list()
# combine_lst  <- list()
# 
# for (i in 1:length(aoc_files)) {
#   
#   logger::log_info("reading {aoc_files[i]}")
#   
#   r <- raster::stack(aoc_files[1])
# 
#   shallow_lst[[i]]  <- r$aoc_shallow
#   deep_lst[[i]]     <- r$aoc_deep
#   combine_lst[[i]]  <- r$aoc_combine
# }
# 
# 
# # Calculate 10 year mean AOC 
# shallow_mean <- stack(shallow_lst)%>%
#   setNames(c(paste0("aoc_shallow_", 1:10))) %>% 
#   calc(mean) %>%
#   setNames(c("shallow_aoc_mean"))
# 
# deep_mean <- stack(deep_lst)%>%
#   setNames(c(paste0("aoc_deep_", 1:10))) %>% 
#   calc(mean) %>%
#   setNames(c("deep_aoc_mean"))
# 
# combine_mean <- stack(combine_lst)%>%
#   setNames(c(paste0("aoc_combine_", 1:10))) %>% 
#   calc(mean) %>%
#   setNames(c("combine_aoc_mean"))
# 
# # Standard deviation 
# shallow_sd <- stack(shallow_lst)%>%
#   setNames(c(paste0("aoc_shallow_", 1:10))) %>% 
#   calc(sd) %>%
#   setNames(c("shallow_aoc_sd"))
# 
# deep_sd <- stack(deep_lst)%>%
#   setNames(c(paste0("aoc_deep_", 1:10))) %>% 
#   calc(sd) %>%
#   setNames(c("deep_aoc_sd"))
# 
# combine_sd <- stack(combine_lst)%>%
#   setNames(c(paste0("aoc_combine_", 1:10))) %>% 
#   calc(sd) %>%
#   setNames(c("combine_aoc_sd"))
# 
# # Raster stack of AOC means
# mean_st <- raster::stack(shallow_mean, deep_mean, combine_mean) %>% 
#   # setNames(c("aoc_shallow_mean", "aoc_deep_mean")) %>%
#   terra::rast()
#   
# 
# # Raster stack of AOC standard deviations
# sd_st   <- raster::stack(shallow_sd, deep_sd, combine_sd) %>%
#   # setNames(c("aoc_shallow_standard_dev", "aoc_deep_standard_dev")) %>%
#   terra::rast()
# 
# 
# # save outputs
# writeRaster(mean_st, "data/oyster_resource_suitability/aoc_mean.tif", overwrite = T)
# writeRaster(sd_st, "data/oyster_resource_suitability/aoc_standard_dev.tif", overwrite = T)
# 
# rm(combine_st, aoc_lst, aoc, shallow_st, ov, r, cv, deep_st, combine_mean, deep_mean, shallow_mean, shallow_sd, deep_sd, combine_sd, mean_st, sd_st, shallow_lst, deep_lst, combine_lst, aoc_files, i, ov_files)










