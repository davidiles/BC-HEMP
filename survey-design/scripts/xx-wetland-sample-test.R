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
library(nngeo)        # for closing holes in polygons

# Plotting
library(viridis)
library(RColorBrewer)
library(units)

# Survey site selection
## devtools::install_github("paul-vdb/DFO-master-sample")
library(BASMasterSample)
library(survey)

# ----------------------------------------------------------
# Prepare plotting options
# ----------------------------------------------------------
custom_theme = theme_bw()
theme_set(custom_theme)

options(ggplot2.continuous.colour = "viridis",
        scipen = 10000)
colscale = scale_fill_manual(values = brewer.pal(3,"Set2"))

# ----------------------------------------------------------
# Prepare spatial data
# ----------------------------------------------------------

# Location of shapefiles within survey design directory
setwd("data-and-shapefiles/shapefiles/")

# Load spatial objects
studyRegion = read_sf("StudyArea_Habitat.shp")         # Entire BC study area
all_psu = read_sf("old_archived/PSUs_Habitat.shp")
stillwater = read_sf("Stillwater_SF.shp")  
setwd("../../")

# Close holes in each PSU (define outer perimeter of PSU)
psu_perimeter = all_psu %>%
  group_by(PSU_ID) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_buffer(dist = 100) %>%
  nngeo::st_remove_holes(.) %>%
  st_buffer(dist = -100)

# Break apart disjunct waterbody polygons
stillwater <- st_cast(stillwater,"POLYGON")

# Trim out waterbodies that are not within PSUs
stillwater <- st_intersection(stillwater,psu_perimeter)

# Calculate area of each waterbody
stillwater <- stillwater %>%
  mutate(Shape_Area_m = as.numeric(st_area(geometry))) %>%
  mutate(Shape_Area_ha = Shape_Area_m / 10000)

stillwater$Lake_ID = 1:nrow(stillwater)

# Distribution of water body sizes
hist(stillwater$Shape_Area_ha)

# Merge wetlands within each PSU
stillwater_combined <- stillwater %>%
  group_by(PSU_ID) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
            geometry = st_union(geometry)) %>%
  mutate(Shape_Area_ha = Shape_Area_m / 10000)

# --------------------------------------------
# Sample design settings
# --------------------------------------------

# ----------
# Wetland habitats
# ----------

# Assume each ARU surveys a circular area with radius 300 m. 
# This is analogous to choosing a maximum ARU density 
aru_area = pi*150^2
aru_density_ha = aru_area  / 10000 # approximately 1 ARU per 7 ha
aru_density_ha

# What proportion of wetlands are smaller than the assumed ARU area?
mean(stillwater$Shape_Area_m < aru_area) # Most (96%) wetlands would not accomodate more than 1 ARU, even if EDR is only 100m

# Total sample size (expected)
sum(stillwater$Shape_Area_m) / aru_area

# *******************************************************************
# *******************************************************************
# APPROACH 1: Draw a BAS sample and evaluate spatial properties
# *******************************************************************
# *******************************************************************

# Select a new master sample
bb_MS = buildMS(shp = studyRegion) 

# Select a sample with specified n in each PSU
for (psu in unique(stillwater_combined$PSU_ID)){
  
  psu_sf = subset(stillwater_combined, PSU_ID == psu)
  n = (psu_sf$Shape_Area_m / aru_area ) %>% ceiling() 
  samp = masterSample(psu_sf, N = n, bb = bb_MS) %>%
    mutate(PSU_ID = psu)
  
  if (psu == unique(stillwater_combined$PSU_ID)[1]){
    stillwater_samp = samp} else{
      stillwater_samp = rbind(stillwater_samp,samp)
    }
  print(psu)
}

# Determine the waterbody that each sample point falls into
samp_info = st_intersection(stillwater_samp,stillwater)

# Determine distances between nearest points
dists = stillwater_samp %>% st_distance()
min_dist = apply(dists, 1, function(x){min(x[x!=0])})
samp_info$min_dist = min_dist
mean(min_dist < 150) # Probability two points are closer than 150 m

total_sample_size = nrow(samp_info)

# Samples per PSU
PSU_n = samp_info %>% 
  as.data.frame() %>%
  group_by(PSU_ID) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

PSU_n

# ---------------------------------------------------------
# Wetland areas in sample, relative to availability on landscape
# --------------------------------------------------------- 

BAS_sample_density <- ggplot()+
  geom_density(data = samp_info,aes(x = Shape_Area_ha, col = "Sample"), size = 2)+
  geom_density(data = stillwater,aes(x = Shape_Area_ha, col = "Available"), size = 2)


# ---------------------------------------------------------
# 1. How does lake area affect relative sample size, and probability a lake is sampled?
# ---------------------------------------------------------

lake_info <- stillwater %>%
  as.data.frame() %>%
  group_by(Lake_ID) %>%
  summarize(Shape_Area_ha = mean(Shape_Area_ha))

n_per_lake<- samp_info %>%
  as.data.frame() %>%
  group_by(Lake_ID) %>%
  summarize(n = n(),
            nearest_point_m = min(min_dist))

lake_info <- full_join(lake_info,n_per_lake)
lake_info$n[is.na(lake_info$n)] <- 0
lake_info$sampled <- lake_info$n > 0

# ---------------------------
# Relationship between lake size and sample size is linear
# ---------------------------

ggplot(lake_info,aes(x = Shape_Area_ha, y = n)) +
  geom_jitter(alpha = 0.2, width = 0, height = 0.1)+
  xlab("Lake Area (ha)")+
  ylab("Number of samples in lake")

# ---------------------------
# Probability lake is sampled
# ---------------------------

# logistic regression
m1 = glm(sampled ~ log10(Shape_Area_ha), data = lake_info, family = binomial())
pred_df <- data.frame(Shape_Area_ha = seq(min(lake_info$Shape_Area_ha),max(lake_info$Shape_Area_ha),length.out = 1000))
pred_df$pred <- predict(m1,newdata = pred_df, type = "response")

ggplot(lake_info,aes(x = Shape_Area_ha, y = as.numeric(n>0))) +
  geom_jitter(alpha = 0.2, width = 0, height = 0.1)+
  geom_line(data = pred_df, aes(x = Shape_Area_ha, y = pred))+
  scale_x_continuous(trans = "log10")+
  xlab("Lake_Area_ha")+
  ylab("Probability lake will be sampled")
 
# ---------------------------------------------------------
# How close together are points?
# --------------------------------------------------------- 

summary(samp_info$min_dist)
mean(samp_info$min_dist < 150) # Probability a point has a neighbour closer than 150 m

# 18% of points are closer than 150 m from nearest...
# 45-50% of points are closer than 300 m from nearest

# ---------------------------------------------------------
# Example of PSU with lots of lakes 
# --------------------------------------------------------- 

table(samp_info$PSU_ID) %>% sort()

psu_id = 88

BAS_example_plot_1 <- ggplot(subset(stillwater, PSU_ID == psu_id))+
  geom_sf(col = "transparent")+
  geom_sf(data = subset(samp_info,PSU_ID == psu_id)) +
  annotation_scale()

BAS_example_plot_1

# ---------------------------------------------------------
# Example of a PSU with large lakes
# --------------------------------------------------------- 

psu_id = 90

BAS_example_plot_2 <- ggplot(data = subset(stillwater, PSU_ID == psu_id))+
  geom_sf(col = "transparent")+
  geom_sf(data = subset(samp_info,PSU_ID == psu_id)) +
  annotation_scale()
BAS_example_plot_2

# *********************************************************
# *********************************************************
# APPROACH 2: only place 1 ARU per wetland. Sample wetlands in proportion to area using SRS
# *********************************************************
# *********************************************************

# Define sample size and lake-level inclusion probabilities
n_target <- total_sample_size

p = lake_info$Shape_Area_ha / sum(lake_info$Shape_Area_ha) * n_target

lake_sample <- sample(1:nrow(lake_info),size=n_target,prob = p)
lake_sample <- lake_info[lake_sample,]

# Selected lake centroids
lake_sample_centroids <- subset(stillwater, Lake_ID %in% lake_sample$Lake_ID) %>%
  st_centroid()

# Determine distances between nearest points
dists = lake_sample_centroids %>% st_distance()
min_dist = apply(dists, 1, function(x){min(x[x!=0])})

mean(min_dist < 150) # 3-5% are closer than 150m to nearest neighbour
mean(min_dist < 300) # 15-20% are closer than 300m to nearest neighbour

# Samples per PSU
PSU_n = lake_sample_centroids %>% 
  as.data.frame() %>%
  group_by(PSU_ID) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

PSU_n

# ---------------------------------------------------------
# Wetland areas in sample, relative to availability on landscape
# --------------------------------------------------------- 

SRS_sample_density <- ggplot()+
  geom_density(data = lake_sample_centroids,aes(x = Shape_Area_ha, col = "Sample"), size = 2)+
  geom_density(data = stillwater,aes(x = Shape_Area_ha, col = "Available"), size = 2)

# ---------------------------------------------------------
# Example of PSU with lots of lakes - evaluate sample
# --------------------------------------------------------- 

psu_id = 88

SRS_example_plot_1 <- ggplot(subset(stillwater, PSU_ID == psu_id))+
  geom_sf(col = "transparent")+
  geom_sf(data = subset(lake_sample_centroids,PSU_ID == psu_id)) +
  annotation_scale()

SRS_example_plot_1

# ---------------------------------------------------------
# Example of a PSU with large lake
# --------------------------------------------------------- 

psu_id = 90

SRS_example_plot_2 <- ggplot(subset(stillwater, PSU_ID == psu_id))+
  geom_sf(col = "transparent")+
  geom_sf(data = subset(lake_sample_centroids,PSU_ID == psu_id)) +
  annotation_scale()

SRS_example_plot_2

# Main takeaways:
#    - most waterbodies are relatively small and could not accomodate more than 1 ARU
#    - Given the spatial configuration of wetlands, BAS does not achieve desirable samples
#          - it does not acheive adequate spatial balance and often selects sites that are very close together
#
#    - better to use alternative approach in which individual wetlands are selected in proportion to their area
#    - position 1 ARU per wetland (more efficient)
#        - increases spatial coverage across PSUs
#        - increases spacing between neighbouring survey locations
#    - SRS also more closely matches the distribution of lake sizes on landscape (larger sample of small wetlands than BAS)

