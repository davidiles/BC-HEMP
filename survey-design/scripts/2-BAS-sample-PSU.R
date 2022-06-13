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

# ----------------------------------------------------------
# Prepare plotting options
# ----------------------------------------------------------
custom_theme = theme_bw()
theme_set(custom_theme)

options(ggplot2.continuous.colour = "viridis")
colscale = scale_fill_manual(values = brewer.pal(3,"Set2"))

# ----------------------------------------------------------
# Prepare spatial data
# ----------------------------------------------------------

# Location of shapefiles within survey design directory
setwd("data-and-shapefiles/shapefiles/")

# Load spatial objects
studyRegion = read_sf("StudyArea_Habitat.shp")         # Entire BC study area
all_psu = read_sf("PSUs_Habitat_update_updated.shp")   # Habitat strata within PSUs (alpine, subalpine, upper montane)
all_psu_contour = read_sf("PSUs_500_contours.shp")     # 50 m contours within PSUs
stillwater_in_PSUs = read_sf("stillwater_in_PSUs.shp") # Wetlands trimmed to PSU perimeter (created by script 1-edit-shapefiles.R)

setwd("../../")

# ****************************************************
# ****************************************************
# TERRESTRIAL HABITAT SAMPLE DESIGN
#  - use BAS to select spatially balanced samples
#  - sample size based on area of habitats
# ****************************************************
# ****************************************************

# --------------------------------------------
# Prepare master sample for terrestrial portion of study region
# --------------------------------------------

# set.seed(111)
bb_MS = buildMS(shp = studyRegion) 
save(bb_MS, file = "survey-design/output/bb_MS.RData")

# --------------------------------------------
# Define sample size targets in each PSU
# --------------------------------------------

# Assume each ARU surveys a circular area with radius 300 m. 
# This is analogous to choosing a maximum ARU density 
aru_area = pi*300^2
aru_density_ha = aru_area  / 10000 # approximately 1 ARU per 28 ha
aru_density_ha

# Calculate area of terrestrial habitat strata in each PSU
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

# Set maximum sample size within each terrestrial habitat stratum, within each PSU
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

# ****************************************************
# ****************************************************
# WETLAND SAMPLE DESIGN
#  - use simple random sampling to position a maximum of 1 ARU per distinct waterbody
#  - sample size based on area and number of wetlands
# ****************************************************
# ****************************************************

# --------------------------------------------
# Define sample size targets in each PSU
# --------------------------------------------

# Assume each ARU surveys a circular area with radius 300 m. 
# This is analogous to choosing a maximum ARU density 
aru_area = pi*150^2
aru_density_ha = aru_area  / 10000 # approximately 1 ARU per 7 ha
aru_density_ha

# Calculate area occupied by stillwater in each PSU
stillwater_PSU_summary = stillwater_in_PSUs %>%
  group_by(PSU_ID) %>%
  summarize(Shape_Area_m = sum(as.numeric(st_area(geometry))))

# Calculate number of distinct waterbodies in each PSU
n_waterbodies_per_PSU = stillwater_in_PSUs %>%
  as.data.frame() %>%
  group_by(PSU_ID) %>%
  summarize(n_waterbodies = n())

# Set target wetland sample size in each PSU
stillwater_PSU_summary$n_area = ( stillwater_PSU_summary$Shape_Area_m / aru_area ) %>% round() # Maximum sample size based on overall area of wetlands
stillwater_PSU_summary <- full_join(stillwater_PSU_summary, n_waterbodies_per_PSU) # Maximum sample size based on number of discrete waterbodies

# Target sample size is smaller of n_area and n_waterbody
stillwater_PSU_summary$n <- apply(stillwater_PSU_summary,1,function(x) min(c(x$n_area,x$n_waterbodies)))

# Examine anticipated total sample size in each PSU
stillwater_PSU_summary

# *******************************************************************
# *******************************************************************
# USE-CASE: SELECT A SAMPLE FOR A SINGLE PSU
# *******************************************************************
# *******************************************************************

psu_id = 123

# ----------
# Subset shapefiles to the PSU of interest
# ----------

psu = all_psu[all_psu$PSU_ID == psu_id,] # Terrestrial habitats within PSU
psu_stillwater = subset(stillwater_in_PSUs,PSU_ID == psu_id) # Stillwater within PSU
psu_contour = subset(all_psu_contour,PSU_ID == psu_id) # Elevation contours within PSU

# Plot the PSU 
psu_plot = ggplot(psu)+
  # Terrestrial habitats
  geom_sf(aes(fill = habitat))+
  # Stillwater (wetlands)
  geom_sf(data = psu_stillwater, fill = "navyblue", col = "white")+
  # Elevation contours
  geom_sf(data = psu_contour, aes(col = elevation),fill = "transparent")+
  # Annotation and title
  annotation_scale() + colscale + 
  ggtitle(paste0("PSU_ID = ",psu_id))
print(psu_plot)

# ----------
# Select a stratified spatially balanced sample for terrestrial habitats
# ----------

n_habitat = psu$n
names(n_habitat) = psu$PSU_habitat
habitat_samp = masterSample(psu, N = n_habitat, bb = bb_MS, stratum = "PSU_habitat")

# Add habitat information
habitat_samp <- st_intersection(habitat_samp,all_psu) %>% dplyr::select(-PSU_habitat.1)

# ----------
# Select a random sample for stillwater areas
# ----------

n_target = stillwater_PSU_summary$n[which(stillwater_PSU_summary$PSU_ID == psu_id)]

# Calculate area of each lake
psu_stillwater = psu_stillwater %>%
  mutate(Shape_Area_m = as.numeric(st_area(geometry)))

# Define inclusion probabilities for each lake
psu_stillwater$inclusion_prob = psu_stillwater$Shape_Area_m / sum(psu_stillwater$Shape_Area_m) * n_target

# Select sample
stillwater_sample <- psu_stillwater[sample(1:nrow(psu_stillwater),size=n_target,prob = psu_stillwater$inclusion_prob),]

stillwater_sample_centroids <- stillwater_sample %>% st_centroid()

# ----------
# Plot the sample locations
# ----------

psu_plot + 
  geom_sf(data = habitat_samp, aes(shape = habitat), col = "black") +
  geom_sf(data = stillwater_sample_centroids, col = "yellow")
  

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

# *******************************************************************
# *******************************************************************
# Compare draw with a single PSU, or all PSUs (should be identical)
# *******************************************************************
# *******************************************************************

tmp <- st_intersection(all_psu_samp, psu)

psu_plot + geom_sf(data = habitat_samp, aes(shape = habitat))
psu_plot + geom_sf(data = tmp, aes(shape = habitat))

# *******************************************************************
# *******************************************************************
# Example of carving off a portion of the PSU
# *******************************************************************
# *******************************************************************
aru_area = pi*300^2

# Only include the lower half of the study area 
point_coords <- st_coordinates(habitat_samp)
habitat_samp <- cbind(habitat_samp,point_coords)

# Create a bounding box to crop the PSU
poly_crop  <- subset(habitat_samp,Y < mean(habitat_samp$Y)) %>%
  st_bbox() %>%
  st_as_sfc()

# Subset the psu polygon
psu_crop <- st_intersection(psu,poly_crop) %>%
  group_by(habitat,PSU_ID) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
            geometry = st_union(geometry)) %>%
  mutate(PSU_habitat = paste0(PSU_ID," - ",habitat),
         n = ceiling(Shape_Area_m / aru_area ))
psu_crop$n[psu_crop$n>20] = 20

# Define revised sample size target for this portion of the PSU
n = psu_crop$n
names(n) = psu_crop$PSU_habitat
psu_crop_samp = masterSample(psu_crop, N = n, bb = bb_MS, stratum = "PSU_habitat")

# Add habitat information
psu_crop_samp <- st_intersection(psu_crop_samp,all_psu) %>% dplyr::select(-PSU_habitat.1)

# Plot the PSU (filled black circles are "revised" sample locations, dark gray empty circles are sample locations in full PSU) 
psu_crop_plot = ggplot(psu_crop)+
  geom_sf(data = psu, fill = "gray95", col = "transparent")+
  geom_sf(aes(fill = habitat), col = "transparent")+
  annotation_scale() + colscale + ggtitle("Cropped section of PSU")+
  
  # Carved off portion
  geom_sf(data = psu_crop_samp, shape = 19)+
  
  # Original sample
  geom_sf(data = habitat_samp, shape = 1, col = "gray35", size = 3)

psu_crop_plot
# This illustrates the master sample concept at work