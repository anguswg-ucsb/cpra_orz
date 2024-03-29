
# **Oyster Resource Suitability Model**

The purpose of this repository is to process and model Coastal Master
Plan data to aid the \*\**Coastal Protection and Restoration Authority’s
(CPRA) efforts to recover, manage, and maximize eastern oyster
(Crassostrea virginica) resources*

<br>

# Packages used in analysis data

``` r
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(mapview)
library(waver)
library(logger)
```

# Installation

``` r
install.packages(c("tidyverse", "sf", "sp", "raster",     # if missing packages
                   "rgdal", "mapview", "waver", "logger")) 
```

# Steps to get data outputs:

The final data outputs from the AOC and Traditional cultch models are
located in `/data` folder. Additionally, all of the raw data used to
generate the AOC and traditional cultch model data are packaged in this
repository, allowing anyone to run the same analysis from there machine.

## Preprocessing

1.  If necessary, **install the above packages to successfully run** the
    scripts in this projects
2.  Open `cpra_orz/mp/process_mp.R` and run the entire script, this
    script will grid the MP datasets to the MP grid, and then rasterize
    the grid to a raster stack.
3.  Open `cpra_orz/shapes/landwater.R` and run the entire script, this
    script create a 3 rasters (binary landwater, only land, only water)
    and 1 land polygon. These files are saved to `cpra_orz/data/masks`,
    and are used going forward to mask out land areas.

<br>

## AOC model

1.  Open `cpra_orz/aoc/depth.R` and run the entire script
2.  Open `cpra_orz/aoc/roads.R` and run the entire script
3.  Open `cpra_orz/aoc/sediment_dep.R` and run the entire script
4.  Open `cpra_orz/aoc/fetch.R` and run the entire script

-   ***(This script will take \>5 hours to fully run, I don’t recommend
    running this script unless you wish to get an entirely new fetch
    dataset, otherwise, the necessary fetch data can be found in
    `data/fetch/`)***

5.  Open `cpra_orz/aoc/commercial_viability.R` and run the entire script
6.  Open `cpra_orz/aoc/oyster_viability.R` and run the entire script
7.  Open `cpra_orz/aoc/aoc.R` and run the entire script, AOC model
    outputs have now been saved to `cpra_orz/data/aoc`

<br>

## Traditional cultch model

1.  Open `cpra_orz/traditional_cultch/traditional_cultch.R` and run the
    entire script, Traditional cultch model outputs have now been saved
    to `cpra_orz/data/traditional_cultch`

<br>

## Masks + Reference layers

1.  Open `cpra_orz/shapes/oyster_harvest_areas.R` and run the entire
    script, this script create a raster of LDH Oyster harvest
    classification areas for masking
2.  Open `cpra_orz/shapes/coastal_use_permits.R` and run the entire
    script, this script create a raster of Coastal use permits for
    masking
3.  Open `cpra_orz/shapes/navigation_channels` and run the entire
    script, this script create a raster of navigation channels for
    masking
4.  Open `cpra_orz/shapes/state_owned_water_bottoms.R` and run the
    entire script, this script creates a raster state owned water
    bottoms
5.  Open `cpra_orz/shapes/cpra_projects.R` and run the entire script,
    this script creates a raster of CPRA Restoration projects for
    masking
