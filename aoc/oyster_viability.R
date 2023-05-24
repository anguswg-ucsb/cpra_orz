# Angus Watters
# Lynker
# create oyster viability raster
rm(list = ls())

library(dplyr)
library(sf)
library(raster)
library(logger)

source("utils/utils.R")

# water file paths
water_files     <- grep(
  "/water_S0",
  paste0("data/masks/",list.files("data/masks/", pattern = "water_S0")),
  value = T
)

# land file paths
land_files      <- paste0("data/masks/",list.files("data/masks/", pattern = "land_polygon_S0"))

# water raster for masking
water        <-  readRDS("data/masks/water_raster.rds")

# land polygon for masking
land         <- readRDS("data/masks/land_polygon.rds")

# read in all MP2023 files
mp_files <- paste0("data/mp2023/raster/", list.files("data/mp2023/raster/", pattern = "mp2023_oyster_hsi_S"))

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


#  loop though MP2023 years and calculate salinity SI for AOC and save to disk
for (i in 1:length(path_df$mp_path)) {

  run <-  substr(path_df$mp_path[i], 38, 46)
  
  logger::log_info("Calculating Oyster Viability - {run}")

  # read in single MP year
  mp_data      <- raster::stack(path_df[i, 1])
  # mp_data <- raster::stack(paste0("data/mp2023/raster/", mp_files[i]))
  
  # land polygon for masking
  land         <- readRDS(path_df[i, 3])
  
  # # calculate Oyster viability, output is raster with OV components and OV final
  ov <- get_ov(
              mp_data = mp_data,
              land    = land,
              mask    = TRUE
            )

  # rds_path <-  paste0("data/oyster_viability/oyster_viability_", run, ".rds")
  tif_path <-  paste0("data/oyster_viability/oyster_viability_", run, ".tif")
  
  logger::log_info("Saving:\n{tif_path}")
  # logger::log_info("Saving:\n{rds_path}\n{tif_path}")

  # save RDS
  # saveRDS(
  #   ov,
  #   rds_path
  # )
 
  # save tif
  # writeRaster(
  #   terra::rast(ov), 
  #   tif_path,
  #   # paste0("data/oyster_viability/oyster_viability_",substr(mp_files[i], 19, 20), ".tif"),
  #   overwrite = TRUE
  #   )

  # year    <- substr(mp_files[[1]], 19, 20)
  final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_final_data_package/"

  writeRaster(ov,
              filename  = paste0(final_save,"oyster_viability/", names(ov), "_",  run,".tif" ),
              bylayer   = TRUE, 
              overwrite = T 
              )
  

  # Write meta data files 
  meta_path      <-  paste0("data/metadata/", names(ov), "_",  run, ".xml" )
  template_path  <- "data/metadata/template/metadata_template.xml"
  
  # ms_txt <- meta_outline %>% filter(file_name == "si_ms_01")
  # 

  # Abstracts
  si_sal_cool_abs  <- paste0("<abstract>Suitability index based on cool month (October through March) minimum salinity; Scenario ",
                             substr(run, 2, 3),  " model year ", substr(run, 5, 9), "</abstract>")
  si_sal_warm_abs  <- paste0("<abstract>Suitability index based on warm month (April through September) minimum salinity; Scenario ",
                             substr(run, 2, 3),  " model year ", substr(run, 5, 9), "</abstract>")
  si_sal_avg_abs   <- paste0("<abstract>Suitability index based on annual average salinity; Scenario ",
                             substr(run, 2, 3),  " model year ", substr(run, 5, 9), "</abstract>")
  si_ms_abs        <- paste0("<abstract>Suitability index based monthly minimum salinity; Scenario ",
                             substr(run, 2, 3), " model year ", substr(run, 5, 9), "</abstract>")
  si_ov_abs        <- paste0("<abstract>Oyster viability index based monthly minimum salinity and annual average salinity; Scenario ",
                             substr(run, 2, 3),  " model year ", substr(run, 5, 9), "</abstract>")

  # Titles
  layer_titles <- paste0("<title>", names(ov), "_", run, "</title>")
  
  # Abstract/titles to iterate through layers
  meta_df <- data.frame(
    si_sal_cool_abstract  = si_sal_cool_abs,
    si_sal_warm_abstract  = si_sal_warm_abs,
    si_sal_avg_abstract   = si_sal_avg_abs,
    si_ms_abstract        = si_ms_abs,
    si_ov_abstract        = si_ov_abs
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
  
  
  
  # # save RDS
  # saveRDS(ov, paste0("data/oyster_viability/oyster_viability_",substr(mp_files[i], 19, 20), ".rds"))
}

# ---- Edit metadata files ----
# meta_path  <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/metadata/metadata/"
meta_path     <- "C:/Users/angus/OneDrive/Desktop/metadata/"
template_meta <- "C:/Users/angus/OneDrive/Desktop/aoc_deep_01_xsltransform_usethisone.xml"
file_lst      <- list.files(meta_path)
meta_files    <- paste0(
  meta_path, 
  list.files(meta_path)
  )

# list.files(meta_path)[1]
meta_lst <- list()
for (i in 1:length(meta_files)) {
  logger::log_info("meta data fix for {meta_files[i]}")
  
  x <- readLines(meta_files[i])
  
  title <- gsub("C:/Users/angus/OneDrive/Desktop/metadata/", "", meta_files[i])
  title <- gsub(".xml", "", title)
  title_txt <- paste0("<title>", title, "</title>")
  
  abstract  <- x[16]
  abstract  <- gsub("<supplinf type=\"supplinf\">", "<abstract>", abstract)
  abstract  <- gsub("</supplinf>", "</abstract>", abstract)
  
  df <-  data.frame(
    file_name     = title,
    meta_title    = title_txt,
    meta_abstract = abstract
    )
  
  meta_lst[[i]] <- df
  
  # temp_x <- readLines(template_meta[1])
  # temp_x[12] <- abstract
  # temp_x[7] <- title_txt
  # 
  # temp_x <- gsub("poject", "project", temp_x)
  # temp_x <- gsub("cultch", "culture", temp_x)
  # final <- gsub("Lynker Technologies, Inc", "Lynker, Inc", temp_x)
  # # x <- gsub("The full model code is available at https://github.com/anguswg-ucsb/cpra_orz or provided upon request.", "", x)
  # # x <- gsub("cultch", "culture", x)
  # # x <- gsub("Lynker Technologies, Inc", "Lynker, Inc", x)
  # 
  # xml <- as.character(final)
  # 
  # code <- paste0(as.character(xml), collapse = "\n")
  # 
  # write.table(code,
  #             file = paste0("C:/Users/angus/OneDrive/Desktop/metadata2/", file_lst[i]),
  #             quote = FALSE,
  #             col.names = FALSE,
  #             row.names = FALSE)
}

# cv <- raster::stack("data/commercial_viability/commercial_viability.tif")

# final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"

writeRaster(cv, 
            filename = paste0(
              final_save,
              "commercial_viability/",
              names(cv),
              ".tif"
            ),
            bylayer   = TRUE, 
            overwrite = T
)