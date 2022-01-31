# Calculate tradition cultch model HSI
# inputs:
    # 1. Masterplan raster data w/ naming conventions
    # 2. Sediment deposition 10 year mean
get_trad_cultch <- function(mp, sed_dep) {

  # SI 2 function
  si2_func <- function(x, ...) {
    ifelse(x < 5, 0,
           ifelse(x >= 5 & x < 10, (0.06*x) - 0.3,
                  ifelse(x >= 10 & x < 15, (0.07*x) - 0.4,
                         ifelse(x >= 15 & x < 18, ((0.1167)*x) -1.1,
                                ifelse(x >= 18 & x < 22, 1.0,
                                       ifelse(x >= 22 & x < 30, ((-0.0875)*x) + 2.925,
                                              ifelse(x >= 30 & x < 35, ((-0.04)*x) + 1.5,
                                                     ifelse(x >= 35 & x < 40, ((-0.02)*x) + 0.8, 0)
                                              ))))))
    )

  }

  # SI 3 cool function
  si3_cool_func <- function(x, ...) {
    ifelse(x <= 1, 0,
           ifelse(x > 1 & x < 8, (0.1429*x) - 0.1429,
                  ifelse(x >= 8 & x < 10, 1,
                         ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                                ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                       ifelse(x >= 20, 0.001, 0)
                                ))))
    )
  }

  # SI 3 warm function
  si3_warm_func <- function(x, ...) {
    ifelse(x <= 2, 0,
           ifelse(x > 2 & x < 8, (0.1667*x) - 0.3333,
                  ifelse(x >= 8 & x < 10, 1,
                         ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                                ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                       ifelse(x >= 20, 0.001, 0)
                                ))))
    )
  }

  # SI 4 average function
  si4_avg_func <- function(x, ...) {
    ifelse(x < 5, 0,
           ifelse(x >= 5 & x < 10, (0.2*x) - 1.0,
                  ifelse(x >= 10 & x < 15, 1,
                         ifelse(x >= 15 & x < 20, ((-0.16)*x) + 3.4,
                                ifelse(x >= 20 & x < 25, ((-0.04)*x) + 0.9996,
                                       ifelse(x >= 25, 0.001, 0)
                                ))))
    )
  }

  # SI 5 function % of cell covered by land
  si5_func <- function(x, ...) {
    -x + 1.0
  }

  # SI 6 Sediment Deposition function
  si6_sed_dep_func <- function(x, ...) {
    ifelse(x < 35, 1,
           ifelse(x >= 35 & x < 40, (-0.2*x) + 8.0,
                  ifelse(x >= 40, 0, 0)
           )
    )
  }

  # ---- SI 2 ----
  # Mean salinity during the spawning season, April through November
  si2      <- raster::calc(mp$s_spwn, si2_func)

  # ---- SI 3 ----
  #  Minimum monthly mean salinity cool/warm months
  si3_cool <- raster::calc(mp$smin_c, si3_cool_func)
  si3_warm <- raster::calc(mp$smin_w, si3_warm_func)

  # SI3 = (SI3 cool x SI3 warm)^1/2
  si3      <- (si3_cool * si3_warm)**(0.5)

  # ---- SI 4 ----
  # Mean annual salinity

  si4      <- raster::calc(mp$s_mean, si4_avg_func)

  # ---- SI 5 ----
  # % cell covered by land

  si5      <- raster::calc(mp$pct_land, si5_func)

  # ---- SI 6 ----
  # Cumulative Sediment Deposition
  # sed_dep <- mp$sedim*10

  si6      <- raster::calc(sed_dep, si6_sed_dep_func)

  # ---- HSI ----
  # HSI = (SI2 x SI3 x SI4 x SI5 x SI6)^1/5

  hsi    <- (si2 * si3 * si4 * si5 * si6)**(1/5)

  hsi_st <- raster::stack(si2, si3, si4, si5, si6, hsi) %>%
    setNames(c("si2", "si3", "si4", "si5", "si6", "hsi")) %>%
    terra::rast()

  return(hsi_st)
}

# Calculate Commercial viability
# inputs:
    # 1. Roads buffer raster
    # 2. Fetch raster
    # 1. Sediment deposition 10 year mean
    # 2. Depth layer reclassified to 3 levels
get_cv <- function(water, land, roads, fetch, sed_dep, depth) {
  
  # resample template raster
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs = CRS('+init=EPSG:26915'),
    ext = extent(405220, 909700, 3199570, 3416530)
  )
  
  # resample rasters to all fit same grid
  depth   <- resample(depth, resamp_r)
  water   <- resample(water, resamp_r)
  roads   <- resample(roads, resamp_r)
  fetch   <- resample(fetch, resamp_r)
  sed_dep <- resample(sed_dep, resamp_r)
  
  # mask for shallow waters
  shallow_mask <- setValues(
    depth, ifelse(getValues(depth) == 2, 1, NA)
  )
  
  # mask for deep waters
  deep_mask <- setValues(
    depth, ifelse(getValues(depth) == 3, 1, NA)
  )
  # CV relationships
  
  # Reclassify road buffers values
  road_mat <- matrix(
    c(0, 5, 1,
      5, 10, 0.5,
      10, 20, 0.2,
      20, 30, 0.1),
    ncol=3, byrow = T
  )
  
  # Reclassify fetch values
  fetch_mat_shallow <- matrix(
    c(0, 1000, 1,
      1000, 5000, .5,
      5000, 20001, .2),
    ncol=3, byrow = T
  )
  
  
  # Reclassify fetch values for deep water
  fetch_mat_deep <- matrix(
    c(0,     5000,   1,          # deep water fetch bins
      5000,  10000, .5,
      10000, 20001, .2),
    ncol=3, byrow = T
  )
  
  # sediment deposition CV (millimeters)
  sedim_func <- function(x, ...) { #
    ifelse(x == 0, 1,
           ifelse(x >= 0 & x < 40,  1 + (-0.0225*x),
                  ifelse(x >= 40, 0.1, 0
                  )))
  }
  
  # calculate CV SI
  # Fetch shallow/deep SI
  fetch_shallow_cv            <- reclassify(fetch, fetch_mat_shallow)
  fetch_deep_cv               <- reclassify(fetch, fetch_mat_deep)
  
  # Sed dep SI
  sed_dep_cv                  <- raster::calc(sed_dep, sedim_func)
  
  # Distance to Roads SI
  roads_cv                    <- reclassify(roads, road_mat, right = T)
  roads_cv[is.na(roads_cv[])] <- 0.1
  
  # calculate shallow/deep CV
  shallow_cv            <-  (fetch_shallow_cv*sed_dep_cv*roads_cv)**(1/3)
  deep_cv               <-  (fetch_deep_cv*sed_dep_cv)**(1/2)
  
  # mask to shallow/deep water levels
  shallow_cv            <-  mask(shallow_cv, shallow_mask) 
  deep_cv               <-  mask(deep_cv, deep_mask) 

  
  # combine shallow/deep CV + mask to land/water
  cv                    <-  cover(shallow_cv, deep_cv)
  cv                    <-  mask(cv, land, inverse = T)
  
  # mask shallow fetch to land/water
  fetch_shallow_cv      <-  mask(fetch_shallow_cv, shallow_mask) 
  fetch_shallow_cv      <-  mask(fetch_shallow_cv, land, inverse = T)

  # mask deep fetch to land/water
  fetch_deep_cv         <-  mask(fetch_deep_cv, deep_mask) 
  fetch_deep_cv         <-  mask(fetch_deep_cv, land, inverse = T)
  
  # mask roads to land/water
  roads_cv              <-  mask(roads_cv, water, inverse= F)
  roads_cv              <-  mask(roads_cv, land, inverse= T)
  
  # mask sed dep to land/water
  sed_dep_cv            <-  mask(sed_dep_cv, water, inverse= F)
  sed_dep_cv            <-  mask(sed_dep_cv, land, inverse= T)
  
  # stack all components of Comm. Viability
  cv_stk                <- raster::stack(
    fetch_shallow_cv,
    fetch_deep_cv, 
    roads_cv, 
    sed_dep_cv, 
    shallow_cv, 
    deep_cv,
    cv
  ) %>%
    setNames(c("si_fetch_shallow", "si_fetch_deep", "si_roads", "si_sed_dep", "shallow_cv", "deep_cv", "cv")) %>%
    terra::rast()
  
  return(cv_stk)
  
  rm(cv, deep_cv, shallow_cv, 
     roads_cv, sed_dep_cv,
     fetch_shallow_cv, fetch_deep_cv,
     road_mat, fetch_mat_deep, fetch_mat_shallow,
     sedim_func, resamp_r
  )
  
}
# make oyster viability layers from MP raster layers
get_ov <- function(mp_data, land = NULL, mask = FALSE) {
  
  # resample to raster
  resamp_r <- raster(
    nrows = 452,
    ncols = 1051,
    crs = CRS('+init=EPSG:26915'),
    ext = extent(405220, 909700, 3199570, 3416530)
  )
  
  # resample data to ensure they are on the same grid
  mp_data   <- resample(mp_data, resamp_r)
  # water     <- resample(water, resamp_r)
  
  # mp_data <- raster::stack(paste0("data/mp2023/raster/", mp_files[1]))
  
  # ---- SI cool calculation function ----
  si_cool_func <- function(x, ...) {
    ifelse(x <= 1, 0,
           ifelse(x > 1 & x < 8, (0.1429*x) - 0.1429,
                  ifelse(x >= 8 & x < 10, 1,
                         ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                                ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                       ifelse(x >= 20, 0.001, 0)
                                ))))
    )
  }
  
  # ---- SI warm calculation function ----
  si_warm_func <- function(x, ...) {
    ifelse(x <= 2, 0,
           ifelse(x > 2 & x < 8, (0.1667*x) - 0.3333,
                  ifelse(x >= 8 & x < 10, 1,
                         ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                                ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                       ifelse(x >= 20, 0.001, 0)
                                ))))
    )
  }
  
  # ---- SI average calculation function ----
  si_av_func <- function(x, ...) {
    ifelse(x < 5, 0,
           ifelse(x >= 5 & x < 10, (0.2*x) - 1.0,
                  ifelse(x >= 10 & x <= 25, 1,
                         ifelse(x > 25 & x <= 30, ((-0.16)*x) + 5,
                                ifelse(x > 30 & x <= 36, ((-0.0332)*x) + 1.195,
                                       ifelse(x > 36, 0.001, 0)
                                ))))
    )
  }
  
  # Apply SI equation
  si_cool <- raster::calc(mp_data$smin_c,  si_cool_func)
  si_warm <- raster::calc(mp_data$smin_w,  si_warm_func)
  si_avg  <- raster::calc(mp_data$s_mean,  si_av_func)
  
  # SI MS
  si_ms     <- (si_cool * si_warm)**(0.5)
  # si_ms     <- (si_sal_stk$si_sal_cool * si_sal_stk$si_sal_warm)**(0.5)
  
  # SI OV
  si_ov    <- (si_ms  * si_avg)**(0.5)
  # si_ov    <- (si_ms  * si_sal_stk$si_sal_avg)**(0.5)
  
  if(mask == TRUE) {
    # SI sal stack w/ layernames + mask
    si_sal_stk <- stack(si_cool, si_warm, si_avg, si_ms, si_ov) %>%
      setNames(c("si_sal_cool", "si_sal_warm", "si_sal_avg", "si_ms", "si_ov")) %>% 
      mask(land, inverse = T)
  } else{
    
    # SI sal stack w/ layernames
    si_sal_stk <- stack(si_cool, si_warm, si_avg, si_ms, si_ov) %>%
      setNames(c("si_sal_cool", "si_sal_warm", "si_sal_avg", "si_ms", "si_ov"))
    
  }
  
  return(si_sal_stk)
  rm(si_ms, si_cool, si_avg, si_warm, si_warm_func, si_cool_func, si_av_func, resamp_r)
}

# function takes in shallow and deep cv and OV rasters and outputs final AOC rasters
get_aoc <- function(shallow_cv, deep_cv, combine_cv, ov) {
  
  # AOC shallow
  aoc_shallow              <- (ov * shallow_cv)**(0.5)
  
  # Deep
  aoc_deep                 <- (ov * deep_cv)**(0.5)
  
  # Combined shallow/deep
  aoc_combine              <- (ov * combine_cv)**(0.5)
  
  # Stack and name AOC rasters
  aoc                      <- raster::stack(aoc_shallow, aoc_deep, aoc_combine) %>%
    setNames(c("aoc_shallow", "aoc_deep", "aoc_combine")) %>%
    terra::rast()
}

# make even clases from raster values
get_classes <- function(rast, lvls){
  int <- seq(
    cellStats(rast, min),
    cellStats(rast, max),
    length.out = lvls
  )
  from    <- int[1:(lvls - 1)]
  to      <- int[2:lvls]
  becomes <- 1:(lvls -1)
  reclass_m <- matrix(
                  c(from, to, becomes),
                  ncol = 3,
                  byrow = FALSE
                      )

  categories <- reclassify(rast, reclass_m, include.lowest = TRUE)
}

# checks geometry and adds character column of name
detect_geometry <- function(string) {
  is_polygon  <- stringr::str_detect(tolower(string), "polygon")
  is_polyline <- stringr::str_detect(tolower(string), "polyline")
  is_point    <- stringr::str_detect(tolower(string), "point")
  ifelse(
    is_polygon,
    "POLYGON",
    ifelse(
      is_polyline,
      "LINESTRING",
      ifelse(
        is_point,
        "POINT",
        NA
      )
    )
  )
}

# Tidy layer geometries from LDWF API
tidy_layer <- function(layer) {
  initial_clean <-
    layer %>%
    dplyr::select(geometryType, features) %>%
    tidyr::unpack(cols = c(tidyselect::everything(), -geometryType)) %>%
    tidyr::unpack(cols = c(tidyselect::everything(), -geometryType)) %>%
    dplyr::mutate(
      geometryType = detect_geometry(geometryType)
    ) %>%
    dplyr::rename(
      Name = tidyselect::contains("name")
    )
  geometry_column <-
    initial_clean %>%
    dplyr::select(tidyselect::last_col()) %>%
    colnames()

  final_clean <-
    initial_clean %>%
    dplyr::rename(
      geometry = tidyselect::contains(geometry_column)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      geometry = sf::st_sfc(sf::st_multipoint(geometry[,,]))
    ) %>%
    # dplyr::mutate(
    #   geometry = dplyr::case_when(
    #     geometryType == "POLYGON"    ~ sf::st_cast(geometry, "POLYGON"),
    #     geometryType == "LINESTRING" ~ sf::st_cast(geometry, "LINESTRING"),
    #     geometryType == "POINT" ~ sf::st_cast(geometry, "POINT")
    #     # geometryType == "MULTIPOINT" ~ sf::st_cast(geometry, "MULTIPOINT"),
    #   )
    # ) %>%
    dplyr::ungroup() %>%
    sf::st_sf(
      crs = "+proj=lcc +lat_1=29.3 +lat_2=30.7 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"
    ) %>%
    sf::st_transform(4326) %>%
    sf::st_as_sf() %>%
    dplyr::select(-geometryType)
}
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "hawaii_d90f_20ee_c4cb_LonPM180",
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                       time = c(time_df$start, time_df$end),
                       latitude = c(26.5, 30.5),
                       longitude = c(-94, -86),
                       fields = c("salt", "temp"))$data %>%
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time) %>%
    select(lon, lat, t, salt, temp) %>%
    na.omit()
}
# Date download range by start and end dates per year
# dl_years <- data.frame(date_index = 1:3,
#                        start = as.Date(c( "1990-01-01", "1998-01-01", "2006-01-01")),
#                        end = as.Date(c("1997-12-31", "2005-12-31", "2010-12-01")))
#   OISST_data <- dl_years %>%
#     group_by(date_index) %>%
#     group_modify(~OISST_sub_dl(.x)) %>%
#     ungroup() %>%
#     select(lon, lat, t, salt, temp)

tidy_raster <- function(df, rm_z, param) {
  s <- df %>%
    select(-rm_z) %>%
    group_by(t) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = "t", values_from = param)
  rs <-  rasterFromXYZ(s) %>%
    stack()
}

agg_monthly <- function(stack, metric) {
  #get the date from the names of the layers and extract the month
  indices <- format(as.Date(names(stack), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)

  #stack layers
  agg <- stackApply(stack, indices, fun = mean)
  agg <- stack(agg)
  # agg <- agg %>%
  #   setNames(c("sal_1", "sal_2", "sal_3", "sal_4","sal_5", "sal_6",
  #              "sal_7", "sal_8", "sal_9", "sal_10","sal_11", "sal_12"))
  #
}
# loads climateR gridmet data for desired paramter rasters, masks to the specified basin, seperates data by district, and aggregates to a tidy tibble
get_gridmet <- function(basin, param, start_date, end_date = NULL,
                        shp_path) {
  # read shapefile, filter to a basin, cast to a multipolygon for masking later on
  shp <- sf::st_read(shp_path, quiet = TRUE) %>%
    dplyr::filter(BASIN == !!basin) %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON")

  # pull gridmet data from climateR gridMET
  gridmet <-
    climateR::getGridMET(
      shp,
      param     = param,
      startDate = start_date,
      endDate   = end_date
    ) %>%
    raster::stack()

  # mask stacks to districts
  gridmet <- lapply(
    X   = seq_len(nrow(shp)),
    FUN = function(x) {
      raster::mask(gridmet, shp[x, ])
    }
  )
  stack_list     <- lapply(X = gridmet, FUN = raster::stack)       # stack list of masked rasterstacks
  district_names <- paste0(shp$DISTRICT)                        # district number
  stack_list     <- setNames(stack_list, nm = district_names)   # add district names to stacks

  # create tidy tibbles from each raster stack in list, then wrangle date columns
  tidy_gridmet <- lapply(X = stack_list, FUN = tidy_raster)
  tidy_gridmet <- lapply(
    X = names(tidy_gridmet),
    FUN = function(x) {
      dplyr::mutate(
        tidy_gridmet[[x]],
        district = x
      )
    }
  ) %>%
    bind_rows()
}
get_terra <- function(basin, param, start_date, end_date = NULL,
                      shp_path) {

  shp <- sf::st_read(shp_path, quiet = TRUE) %>%
    dplyr::filter(BASIN == !!basin) %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON")

  terra <- climateR::getTerraClim(
    AOI       = sf::st_transform(shp, 4326),
    param     = param,
    startDate = start_date,
    endDate   = end_date
  ) %>%
    stack()


  terra <- lapply(
    X   = seq_len(nrow(shp)),
    FUN = function(x) {
      raster::mask(terra, shp[x, ])
    }
  )


  stack_list     <- lapply(X = terra, FUN = raster::stack)       # stack list of masked rasterstacks
  district_names <- paste0(shp$DISTRICT)                        # district number
  stack_list     <- setNames(stack_list, nm = district_names)   # add district names to stacks

  # create tidy tibbles from each raster stack in list, then wrangle date columns
  tidy_terra <- lapply(X = stack_list, FUN = tidy_terra_raster)

  tidy_terra <- lapply(
    X = names(tidy_terra),
    FUN = function(x) {
      dplyr::mutate(
        tidy_terra[[x]],
        district = x
      )
    }
  ) %>%
    bind_rows()
}

tidy_drought_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    # dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# tidy each raster stack in gridMET (exluding PDSI) list of Raster stacks into a tidy tibble
tidy_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# tidy each raster stack in terraclim list of Raster stacks into a tidy tibble
tidy_terra_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        paste0(stringr::str_sub(colnames(.)[-(1:2)], start = 2L), ".1")
      )) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# # tidy each raster stack in ridMET's PDSI and getEDDI() list of Raster stacks into a tidy tibble
tidy_drought_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_drought_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             # p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
}

tidy_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             # p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
}

tidy_to_raster <- function(data, x, y, z) {
  xyz <- data %>%
    dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
    dplyr::rename(
      x = {{ x }},
      y = {{ y }},
      z = {{ z }}
    )
  raster::rasterFromXYZ(
    xyz = xyz,
    crs = sf::st_crs(4326)$proj4string
  )
}
# assigns value "1" if matrix position <= radius and value "NA" if matrix position> radius
make_circ_filter <- function(radius, res){
  circ_filter <- matrix(NA, nrow=1+(2*radius/res), ncol=1+(2*radius/res))
  dimnames(circ_filter)[[1]] <- seq(-radius, radius, by=res)
  dimnames(circ_filter)[[2]] <- seq(-radius, radius, by=res)
  sweeper <- function(mat){
    for(row in 1:nrow(mat)){
      for(col in 1:ncol(mat)){
        dist <- sqrt((as.numeric(dimnames(mat)[[1]])[row])^2 +
                       (as.numeric(dimnames(mat)[[1]])[col])^2)
        if(dist<=radius) {mat[row, col]<-1}
      }
    }
    return(mat)
  }
  out <- sweeper(circ_filter)
  return(out)
}
