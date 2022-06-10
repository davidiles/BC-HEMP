# ***************************************************************
# ***************************************************************
# Script to select a random sample of locations for target PSUs
# ***************************************************************
# ***************************************************************

rm(list=ls())

# ----------------------------------------------------------
# Directory containing survey design files
# ----------------------------------------------------------

setwd("~/1_Work/BC-HEMP/")

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

# --------------------------------------------
# Prepare master sample for entire study region
# --------------------------------------------

set.seed(999)
bb_MS = buildMS(shp = studyRegion) 
save(bb_MS, file = "survey-design/output/bb_MS.RData")

# --------------------------------------------
# Combine habitat geometries within each PSU
# --------------------------------------------

all_psu = all_psu %>%
  group_by(habitat, PSU_ID) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
            geometry = st_union(geometry)) %>%
  ungroup() %>% 
  arrange(PSU_ID,habitat) %>%
  mutate(PSU_habitat = paste0(PSU_ID," - ",habitat))

# Elevation contours within all PSUs
all_psu_contour = all_psu_contour %>%
  mutate(elevation = as.numeric(low_cont)) %>%
  st_intersection(all_psu)

# --------------------------------------------
# Sample design settings
# --------------------------------------------

# Assume each ARU surveys a circular area with radius 300 m
aru_area = pi*300^2 

# Set maximum sample size within each habitat stratum, within each PSU
all_psu$max_n = ( all_psu$Shape_Area_m / aru_area ) %>% ceiling()

# Limit n to 20, when max_n is greater than 20
all_psu$n = all_psu$max_n
all_psu$n[all_psu$n>20] = 20

# Examine anticipated total sample size across study region
sample_size_total = all_psu %>%
  as.data.frame() %>%
  group_by(habitat) %>%
  summarize(n = sum(n))

# Expected total sample size across all PSUs in study area
print(sample_size_total) 

# *******************************************************************
# *******************************************************************
# Example: plot a PSU and select a sample within it using BAS
# *******************************************************************
# *******************************************************************

psu_id = 122

# Shapefile for PSU of interest
psu = all_psu[all_psu$PSU_ID == psu_id,]

# Elevation contours within PSU
psu_contour = subset(all_psu_contour,PSU_ID == psu_id)

# Plot the PSU 
psu_plot = ggplot(psu)+
  geom_sf(aes(fill = habitat))+
  annotation_scale() + colscale + ggtitle(paste0("PSU_ID = ",psu_id))+
  geom_sf(data = psu_contour, aes(col = elevation),fill = "transparent")
print(psu_plot)

# Select a stratified spatially balanced sample using BAS
n = psu$n
names(n) = psu$PSU_habitat
psu_samp = masterSample(psu, N = n, bb = bb_MS, stratum = "PSU_habitat")

# Add habitat information
psu_samp <- st_intersection(psu_samp,all_psu) %>% dplyr::select(-PSU_habitat.1)

# Plot the sample locations
psu_plot + geom_sf(data = psu_samp, aes(shape = habitat))

# *******************************************************************
# *******************************************************************
# Example: plot all PSUs and select a sample within them using BAS
# *******************************************************************
# *******************************************************************

# Plot the PSUs 
all_psu_plot = ggplot(all_psu)+
  geom_sf(aes(fill = habitat))+
  annotation_scale() + colscale + ggtitle("All PSUs")+
  geom_sf(data = all_psu_contour, aes(col = elevation),fill = "transparent")
print(all_psu_plot)

# Select a stratified spatially balanced sample using BAS
n = all_psu$n
names(n) = all_psu$PSU_habitat
all_psu_samp = masterSample(all_psu, N = n, bb = bb_MS, stratum = "PSU_habitat")

# Add habitat information
all_psu_samp <- st_intersection(all_psu_samp,all_psu) %>% dplyr::select(-PSU_habitat.1)


# Plot the sample locations
all_psu_plot + geom_sf(data = all_psu_samp, aes(shape = habitat))