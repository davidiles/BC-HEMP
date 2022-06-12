# ***************************************************************
# ***************************************************************
# Script to select a random sample of locations for target PSUs
# ***************************************************************
# ***************************************************************

rm(list=ls())

# ----------------------------------------------------------
# Directory containing survey design files
# ----------------------------------------------------------

setwd("~/iles_ECCC/Landbirds/BC-landbirds/BC-HEMP/")

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------

# Data manipulation
library(tidyverse)
library(sf)
library(sp)
library(ggspatial)

# Plotting
library(viridis)
library(RColorBrewer)
library(units)

# Survey site selection
## devtools::install_github("paul-vdb/DFO-master-sample")
library(BASMasterSample)

# Custom function to select a sample using BAS
source("survey-design/scripts/0_functions.R")

# ----------------------------------------------------------
# Prepare plotting options
# ----------------------------------------------------------

theme_set(theme_bw())
options(ggplot2.continuous.colour = "viridis")
colscale = scale_fill_manual(values = brewer.pal(3,"Set2"))

# ----------------------------------------------------------
# Prepare spatial data
# ----------------------------------------------------------

# Location of shapefiles within survey design directory
setwd("data-and-shapefiles/shapefiles/")

# Load spatial objects
studyRegion = read_sf("StudyArea_Habitat.shp")     # Entire BC study area
all_psu = read_sf("PSUs_Habitat_update_updated.shp")              # Habitat strata within PSUs (alpine, subalpine, upper montane)
all_psu_contour = read_sf("PSUs_500_contours.shp") # 50 m contours within PSUs
wetlands = read_sf("Wetland_Buffers_100_PSUs.shp") # Wetlands
disturbed = read_sf("Disturbed_Habitat_Final.shp") # Disturbed habitat

setwd("../../")

# * CROP WETLANDS TO WITHIN PSUS