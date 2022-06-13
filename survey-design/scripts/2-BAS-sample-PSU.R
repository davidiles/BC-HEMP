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

# # ********************************************************************
# # ********************************************************************
# # EVALUATE SAMPLE DESIGN ACROSS REPEATED SAMPLE DRAWS
# # ********************************************************************
# # ********************************************************************
# 
# # * NOTE: CODE BELOW IS DEPRECATED (BASED ON OLD PSU SHAPEFILE ACROSS THE ENTIRE STUDY REGION) 
# #         AND NEEDS TO BE EDITED IN ACCORDANCE WITH REVISED PSU BOUNDARIES
# #         - CURRENTLY THERE ARE ONLY 4 PSUs WITH EDITED BOUNDARIES
# 
# # --------------------------------------------
# # Loop through PSUs and select a large oversample (1 ARU per ha)
# # Then thin this to desired ARU density
# # --------------------------------------------
# 
# sample_df = NULL
# 
# for (psu_id in unique(all_psu$PSU_ID)){
# 
#   # Set a random seed in R (Note that the seed used by BAS is different)
#   R_seed = 999
#   set.seed(R_seed)
# 
#   psu = all_psu[all_psu$PSU_ID == psu_id,]
#   psu_cont = all_psu_contour[all_psu_contour$PSU_ID == psu_id,]
#   psu_cont$elevation = as.numeric(psu_cont$low_cont)
# 
#   # sample size target (with large oversample) for this psu
#   n_stratum = psu$n_oversample
#   names(n_stratum) = psu$habitat
# 
#   # Select (over)sample using BAS
#   bas_samp = NULL
#   while(is.null(bas_samp)){
#     bb_psu = buildMS(shp = psu)
#     bas_samp = tryCatch(
#       {masterSample(psu, N = n_stratum, bb = bb_psu, stratum = "habitat")},
#       error = function(cond){NULL}
#     )
#   }
#   bb_attributes = attributes(bb_psu)
# 
#   # Extract elevation contour information
#   bas_samp = bas_samp %>% st_join(psu_cont)
# 
#   # ------------------------------------------------
#   # Distinguish primary sample and oversample
#   # ------------------------------------------------
#   bas_samp$Sample_Type = "Oversample"
# 
#   for (hab in unique(bas_samp$habitat)){
# 
#     # Primary samples
#     n = subset(psu, habitat == hab)$n
#     if (n > 20) n = 20
#     bas_samp[which(bas_samp$habitat == hab)[1:n],"Sample_Type"] = "Primary"
#   }
# 
# 
#   # Distances between points in Primary Sample
#   bas_samp$nearest_point_m = NA
# 
#   dists = bas_samp %>% subset(Sample_Type == "Primary") %>% st_distance()
#   min_dist = apply(dists, 1, function(x){min(x[x!=0])})
# 
#   bas_samp$nearest_point_m[which(bas_samp$Sample_Type == "Primary")] = min_dist
#   bas_samp$R_seed = R_seed
# 
#   sample_df = rbind(sample_df,bas_samp)
# }
# 
# # --------------------------------------------
# # Anticipated distances between nearest points
# # --------------------------------------------
# distance_df = all_results %>%
#   group_by(habitat,seed) %>%
#   summarize(mean_DistToNearest = mean(nearest_point_m),
#             Proportion_less_than_150m = mean(nearest_point_m < 150),
#             Proportion_less_than_300m = mean(nearest_point_m < 300))
# 
# ggplot(distance_df)+
#   geom_boxplot(aes(x = habitat,y=mean_DistToNearest, fill = habitat))+
#   scale_y_continuous(limits = c(0,1000))+
#   xlab("Habitat")+
#   ylab("Expected distance to nearest point")
# 
# plot_150m_spacing = ggplot(distance_df)+
#   geom_boxplot(aes(x = habitat,y=Proportion_less_than_150m, fill = habitat))+
#   xlab("Habitat")+
#   ylab("Proportion")+
#   ylim(c(0,1))+
#   colscale +
#   ggtitle("Proportion of points closer than 150 m")+
#   theme(legend.position = "none")
# 
# plot_300m_spacing = ggplot(distance_df)+
#   geom_boxplot(aes(x = habitat,y=Proportion_less_than_300m, fill = habitat))+
#   xlab("Habitat")+
#   ylab("Proportion")+
#   ylim(c(0,1))+
#   colscale+
#   ggtitle("Proportion of points closer than 300 m")+
#   theme(legend.position = "none")
# 
# spacing_plot = plot_grid(plot_150m_spacing,plot_300m_spacing,nrow=1,align = "hv")
# spacing_plot
# 
# pdf("figures/point_spacing_plot.pdf", width = 8,height=4)
# print(spacing_plot)
# dev.off()
# 
# # --------------------------------------------
# # Availability of habitats within overall study area
# # --------------------------------------------
# 
# # Total area occupied by each habitat stratum
# study_habitats = studyRegion %>%
#   group_by(habitat) %>%
#   summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
#             geometry = st_union(geometry)) %>%
#   ungroup()
# 
# # Total area occupied by each contour band
# study_contours = all_psu_contour %>%
#   rename(Shape_Area_m = Shape_Area) %>%
#   group_by(low_cont) %>%
#   summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))),
#             geometry = st_union(geometry)) %>%
#   ungroup()
# 
# # ----------------
# # Illustrating that AL and SA are over-sampled relative to availability
# # ----------------
# 
# # Available habitat in study area
# available_habitat = study_habitats %>%
#   as.data.frame() %>%
#   select(habitat,Shape_Area_m) %>%
#   mutate(Proportion = Shape_Area_m/sum(Shape_Area_m)) %>%
#   add_column(Type = "Available")
# 
# sample_habitat = all_results %>%
#   as.data.frame() %>%
#   select(habitat) %>%
#   group_by(habitat) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   mutate(Proportion = count/sum(count))%>%
#   add_column(Type = "Sample")
# 
# compare_habitat = bind_rows(available_habitat,sample_habitat)
# 
# habitat_sample_plot = ggplot(compare_habitat, aes(x = habitat,y = Proportion, fill = Type)) + 
#   geom_bar(stat = "identity", position = position_dodge(), alpha = 0.5)+
#   scale_fill_manual(name = "",values = c("dodgerblue","orangered"))+
#   ggtitle("Sampled habitat relative to available habitat")
# habitat_sample_plot
# 
# pdf("figures/sample_habitats.pdf", width = 6,height=4)
# print(habitat_sample_plot)
# dev.off()
# 
# # ----------------
# # Illustrating that high elevations are slightly over-sampled relative to availability
# # ----------------
# 
# # Available elevation bands in study area
# available_elevation = study_contours %>%
#   as.data.frame() %>%
#   select(low_cont,Shape_Area_m) %>%
#   mutate(Proportion = Shape_Area_m/sum(Shape_Area_m)) %>%
#   add_column(Type = "Available")
# 
# sample_elevation = all_results %>%
#   as.data.frame() %>%
#   select(low_cont) %>%
#   group_by(low_cont) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   mutate(Proportion = count/sum(count))%>%
#   add_column(Type = "Sample")
# 
# compare_elevation = bind_rows(available_elevation,sample_elevation)
# compare_elevation$low_cont = as.numeric(compare_elevation$low_cont)
# 
# elevation_sample_plot = ggplot(compare_elevation, aes(x = low_cont,y = Proportion, col = Type)) + 
#   geom_line(size=1.5)+
#   scale_color_manual(name = "",values = c("dodgerblue","orangered"))+
#   ggtitle("Sampled elevation relative to available elevation")
# elevation_sample_plot
# 
# pdf("figures/sample_elevations.pdf", width = 6,height=4)
# print(elevation_sample_plot)
# dev.off()
# 
# # --------------------------------------------
# # Pie charts showing proportion of habitat and size of each PSU
# # --------------------------------------------
# library(scatterpie)
# 
# # Wide format
# psu_summary = all_psu %>% 
#   as.data.frame() %>%
#   mutate(Area_ha = Shape_Area_m / 10000) %>%
#   dplyr::select(PSU_ID,habitat,Area_ha) %>%
#   pivot_wider(names_from = habitat, values_from = Area_ha)
# 
# # Centroids of each PSU
# psu_summary = all_psu %>% group_by(PSU_ID) %>% 
#   summarise(geometry = sf::st_union(geometry)) %>% 
#   st_centroid() %>%
#   full_join(psu_summary)
# psu_summary$lon = st_coordinates(psu_summary)[,1]
# psu_summary$lat = st_coordinates(psu_summary)[,2]
# 
# psu_summary[is.na(psu_summary)] = 0
# psu_summary_df = psu_summary %>%
#   as.data.frame() %>%
#   dplyr::select(PSU_ID,alpine,subalpine,uppermontane,lon,lat) %>%
#   mutate(TotalArea_ha = alpine+subalpine+uppermontane)
# 
# psu_summary_df$radius = psu_summary_df$TotalArea_ha * 3
# 
# psu_habitat_map = ggplot() +
#   geom_sf(data = studyRegion, fill = "gray90", col = "transparent")+
#   geom_scatterpie(data = psu_summary_df,aes(x = lon, y = lat, group = PSU_ID, r = radius), cols = c("alpine","subalpine","uppermontane"), size = 0.1,alpha=0.8)+
#   geom_text(data = psu_summary_df,aes(x = lon, y = lat, label = PSU_ID),size = 1)+
#   colscale+
#   ggtitle("Size and habitat composition of each PSU")+
#   xlab("Longitude")+ylab("Latitude")
# 
# pdf("figures/psu_habitat_map.pdf", width = 8,height=8)
# print(psu_habitat_map)
# dev.off()