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
library(nngeo)      # for closing holes in polygons

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

theme_set(theme_bw())
options(ggplot2.continuous.colour = "viridis")
colscale = scale_fill_manual(values = brewer.pal(3,"Set2"))

# ----------------------------------------------------------
# Load spatial data
# ----------------------------------------------------------

# Location of shapefiles within survey design directory
setwd("data-and-shapefiles/shapefiles/")

# Load spatial objects
studyRegion = read_sf("StudyArea_Habitat.shp")         # Entire BC study area
all_psu = read_sf("PSUs_Habitat_update_updated.shp")   # Habitat strata within PSUs (alpine, subalpine, upper montane)
all_psu_contour = read_sf("PSUs_500_contours.shp")     # 50 m contours within PSUs
stillwater = read_sf("Stillwater_SF.shp")                # Wetlands
disturbed = read_sf("Disturbed_Habitat_Final.shp")     # Disturbed habitat

setwd("../../")

# ----------------------------------------------------------
# Crop wetlands to only those within PSUs
# * NOTE: it would be best to have a shapefile layer that was the outer extent of each PSU
#         currently, there are holes in the psu shapefile (where water was cropped out)
# ----------------------------------------------------------

# Close holes in each PSU (define outer perimeter of PSU)
psu_perimeter = all_psu %>%
  group_by(PSU_ID) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_buffer(dist = 100) %>%
  nngeo::st_remove_holes(.) %>%
  st_buffer(dist = -100)

# Break apart disjunct waterbody polygons
stillwater <- st_cast(stillwater,"POLYGON")

# Unique identifier for each lake
stillwater$Lake_ID = 1:nrow(stillwater)

# Plot psu perimeters
ggplot() + 
  geom_sf(data = psu_perimeter, fill = "gray80") +
  geom_sf(data = all_psu, fill = "transparent", col = "dodgerblue")

# Crop wetland shapefile to only those within PSUs
stillwater_in_PSUs = st_intersection(stillwater,psu_perimeter) %>%
  dplyr::select(PSU_ID, Lake_ID)

# Plot an example PSU to confirm it works
psu_to_plot = 130
ggplot() + 
  geom_sf(data = subset(psu_perimeter, PSU_ID == psu_to_plot), fill = "gray80") +
  geom_sf(data = subset(all_psu,PSU_ID == psu_to_plot),aes(fill = habitat))+
  geom_sf(data = subset(stillwater_in_PSUs, PSU_ID == psu_to_plot), fill = "black", col = "yellow")+
  annotation_scale() + colscale

# ----------------------------------------------------------
# Save edited wetland file
# ----------------------------------------------------------
st_write(stillwater_in_PSUs, dsn = "data-and-shapefiles/shapefiles/stillwater_in_PSUs.shp", delete_layer = TRUE)
