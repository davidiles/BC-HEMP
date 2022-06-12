# ***************************************************************
# ***************************************************************
# Script to generate a master sample for the study area, and select
#   spatially balanced samples within pre-defined habitat polygons
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
library(nngeo)

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
stillwater = read_sf("Stillwater_SF.shp") # Wetlands

setwd("../../")

# ----------------------------------------------------------
# ***********************
# Need to fill in holes/gaps within PSU boundaries before merging wetlands in
# ***********************
# ----------------------------------------------------------

tmp <- subset(all_psu, PSU_ID == 122)

tmp2 <- st_remove_holes(tmp)

ggplot(tmp) + geom_sf()
ggplot(tmp2) + geom_sf()


# Check that geometries are valid
st_is_valid(all_psu)
ggplot(all_psu[13,])+geom_sf()

# Fix invalid geometries
all_psu <- st_make_valid(all_psu)
ggplot(all_psu[13,])+geom_sf()

# Merge water with all_psu object
tmp1 <- st_intersection(water,all_psu) %>%
  group_by(PSU_ID,habitat) %>%
  summarise(Shape_Area_m = as.numeric(st_area(geometry))) %>%
  subset(Shape_Area_m > 0)


%>%
  group_by(PSU_ID) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
            geometry = st_union(geometry)) %>%
  mutate(habitat = "water")

tmp <- st_union(all_psu,water)
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

# Assume each ARU surveys a circular area with radius 300 m. This is used to define a maximum density of ARUs in each stratum
aru_area = pi*300^2
max_density_per_km2 = aru_area / 1000000

# Set maximum sample size within each habitat stratum, within each PSU
all_psu$max_n = ( all_psu$Shape_Area_m / aru_area ) %>% ceiling()

# Limit n to 20, when max_n is greater than 20
all_psu$n = all_psu$max_n
all_psu$n[all_psu$n > 20] = 20

# Examine anticipated total sample size across study region
sample_size_total = all_psu %>%
  as.data.frame() %>%
  group_by(habitat) %>%
  summarize(n = sum(n))

# Expected total sample size across all PSUs in study area
print(sample_size_total) 

# --------------------------------------------
# Select a sample within each PSU
# --------------------------------------------

# Select a stratified spatially balanced sample using BAS
n = all_psu$n
names(n) = all_psu$PSU_habitat
all_psu_samp = masterSample(all_psu, N = n, bb = bb_MS, stratum = "PSU_habitat")

# Add habitat information
all_psu_samp <- st_intersection(all_psu_samp,all_psu) %>% dplyr::select(-PSU_habitat.1)

# --------------------------------------------
# Save the sample as a shapefile and a csv file
# --------------------------------------------

st_write(all_psu_samp, dsn = "survey-design/output/selected-survey-locations/2022_survey_locations.shp", delete_layer = TRUE)

# Add lat/lon information as columns (WGS84)
all_psu_samp_ll <- st_transform(all_psu_samp, crs = 4326) %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])

write.csv(all_psu_samp_ll, file = "survey-design/output/selected-survey-locations/2022_survey_locations.csv", row.names = FALSE)

# --------------------------------------------
# Plot the sample in each PSU
# --------------------------------------------

all_psu_plot = ggplot(all_psu)+
  geom_sf(aes(fill = habitat), col = "transparent")+
  annotation_scale() + colscale + ggtitle("All PSUs")+
  geom_sf(data = all_psu_samp, aes(shape = habitat), size = 0.1)

print(all_psu_plot)

pdf(file = "survey-design/output/figures/all_PSU.pdf", height = 5, width = 15)
print(all_psu_plot)
dev.off()

# --------------------------------------------
# Plot the sample in each PSU sequentially
# --------------------------------------------

for (psu_id in unique(all_psu$PSU_ID)){
  
  # Subset to this PSU
  psu = subset(all_psu, PSU_ID == psu_id)
  psu_contour = subset(all_psu_contour, PSU_ID == psu_id)
  psu_samp = subset(all_psu_samp, PSU_ID == psu_id)
  
  # Plot
  psu_plot = ggplot(psu)+
    geom_sf(aes(fill = habitat), col = "transparent")+
    geom_sf(data = psu_contour, aes(col = elevation),fill = "transparent")+
    annotation_scale() + colscale + ggtitle(paste0("PSU ",psu_id))+
    geom_sf(data = psu_samp, aes(shape = habitat), size = 1)
  
  pdf(file = paste0("survey-design/output/figures/PSU_",psu_id,".pdf"), height = 10, width = 10)
  print(psu_plot)
  dev.off()
  
}

