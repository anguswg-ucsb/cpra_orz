# Angus Watters
# Lynker
# Create meta data for all saved rasters
rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)
library(logger)
library(XML)
library(xml2)

source("utils/utils.R")

# path to wherever final output TIF files were saved 
final_save <- "C:/Users/angus/OneDrive/Desktop/cpra_ors_data/"

# list directories, remove /metadata/ directory before looping over other directories
tif_files <- paste0(final_save, list.files(final_save), "/") 
tif_files <- tif_files[!str_detect(tif_files,pattern="metadata")]

# data_layers <- raster::raster("C:/Users/angus/OneDrive/Desktop/cpra_ors_data/aoc/aoc_deep_01.tif")
# list.files(tif_files)


for (i in 1:length(tif_files)) {
  logger::log_info("Locating files in the directory --- {tif_files[i]} \nCreating XML metadata files for .tif files")
  # logger::log_info("Creating XML metadata files for .tif files")
  
  files <- paste0(tif_files[i], list.files(tif_files[i]))

  for (z in 1:length(files)) {
    
      file_str <- stringr::str_split(files[z], "/")[[1]][8]
      logger::log_info("Creating XML metadata files for {file_str}")
      
      r <- raster::raster(files[z])
      
      xml_str <- gsub(".tif", ".xml", file_str)
      
      create_raster_metadata(
        data_layers = r,
        output_file = paste0(
          final_save,
          "metadata/",
          xml_str
      )
      )
  }
  
}

# ---- Abstract text info ----
# meta_path  <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/metadata/metadata/"
meta_path     <- "C:/Users/angus/OneDrive/Desktop/metadata/"
template_meta <- "C:/Users/angus/OneDrive/Desktop/aoc_deep_01_xsltransform_usethisone.xml"
file_lst      <- list.files(meta_path)
meta_files    <- paste0(meta_path, list.files(meta_path))

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
  
}

meta_outline <- bind_rows(meta_lst)


# ---- Edit metadata files ----
# meta_path  <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/metadata/metadata/"
# meta_path <- "data/metadata"
# template_meta <- "C:/Users/angus/OneDrive/Desktop/aoc_deep_01_xsltransform_usethisone.xml"
file_lst      <- list.files("data/metadata/", pattern = ".xml")
meta_files    <- paste0("data/metadata/", list.files("data/metadata", pattern = ".xml"))


for (i in 1:length(meta_files)) {
  logger::log_info("meta data fix for {meta_files[i]}")

  x <- readLines(meta_files[i])
  x <-  gsub("Wagner", "Wager", x)
  
  # title_txt <- gsub("C:/Users/angus/OneDrive/Desktop/metadata/", "", x)
  # title_txt <- gsub(".xml", "", title_txt)
  # title_txt <- paste0("<title>", title_txt, "</title>")
  # 
  # abstract  <- x[16]
  # abstract  <- gsub("<supplinf type=\"supplinf\">", "<abstract>", abstract)
  # abstract  <- gsub("</supplinf>", "</abstract>", abstract)
  # 
  # 
  # 
  # temp_x <- readLines(template_meta[1])
  # temp_x[12] <- abstract
  # temp_x[7] <- title_txt
  # 
  # temp_x <- gsub("poject", "project", temp_x)
  # temp_x <- gsub("cultch", "culture", temp_x)
  # final <- gsub("Lynker Technologies, Inc", "Lynker, Inc", temp_x)
  # x <- gsub("The full model code is available at https://github.com/anguswg-ucsb/cpra_orz or provided upon request.", "", x)
  # x <- gsub("cultch", "culture", x)
  # x <- gsub("Lynker Technologies, Inc", "Lynker, Inc", x)
  
  xml <- as.character(x)
  
  code <- paste0(as.character(xml), collapse = "\n")
  
  write.table(code,
              file = paste0("data/metadata/", file_lst[i]),
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
}



























