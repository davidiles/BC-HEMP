## devtools::install_github("paul-vdb/DFO-master-sample")
library(BASMasterSample)
library(sf)
library(sp)
library(data.table)
library(spsurvey)
library(tidyverse)
library(sampling)
library(units)
library(ggspatial)
library(RColorBrewer)
library(viridis)

rm(list=ls())

setwd("~/iles_ECCC/Landbirds/BC-landbirds/HEMP-survey-design/iles")

theme_set(theme_bw())
options(ggplot2.continuous.colour = "viridis")
colscale = scale_fill_manual(values = brewer.pal(3,"Set2"), breaks = c("alpine","subalpine","uppermontane"))

# ********************************************************************
# ********************************************************************
# PART 1: PREPARE DATA
# ********************************************************************
# ********************************************************************

# --------------------------------------------
# Read in shapefiles
# --------------------------------------------

## Study Area
studyRegion <- read_sf("StudyArea_Habitat.shp")

# Habitat strata within PSUs (alpine, subalpine, upper montane)
all_psu <- read_sf("PSUs_Habitat.shp")

# 50 m contours within PSUs
all_psu_contour <- read_sf("PSUs_500_contours.shp")

wetlands <- read_sf("Wetland_Buffers_100_PSUs.shp")
disturbed <- read_sf("Disturbed_Habitat_Final.shp")

# pdf("figures/study_region.pdf", width = 8,height=6)
# plot(studyRegion["habitat"], reset=FALSE, key.width = lcm(4), main  = "Study Area",
#      border = "transparent")
# dev.off()

# --------------------------------------------
# Combine geometries within each PSU
# --------------------------------------------

all_psu <- all_psu %>%
  group_by(habitat, PSU_ID) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
            geometry = st_union(geometry)) %>%
  ungroup()

all_psu = all_psu %>%
  arrange(PSU_ID,habitat)

# ********************************************************************
# ********************************************************************
# PART 2: EXAMPLE OF SELECTING A SAMPLE USING BAS WITHIN A PSU
# ********************************************************************
# ********************************************************************

# --------------------------------------------
# Set a maximum sampling intensity within a stratum, use this to define max sample size
# --------------------------------------------
ARU_area = pi*300^2
all_psu$max_n = ( all_psu$Shape_Area_m / ARU_area ) %>% ceiling()

sample_size_histogram = ggplot(all_psu)+
  geom_histogram(aes(x = max_n, fill = habitat))+
  colscale+
  ylab("Number of PSUs")+
  xlab("Maximum attainable sample size in PSU")+
  facet_grid(.~habitat)

pdf("figures/sample_size_histogram.pdf", width = 8,height=4)
print(sample_size_histogram)
dev.off()

# --------------------------------------------
# Select a single PSU
# --------------------------------------------

example_psu = 801
psu = all_psu[all_psu$PSU_ID == example_psu,]
psu_cont = all_psu_contour[all_psu_contour$PSU_ID == example_psu,]
psu_cont$elevation = as.numeric(psu_cont$low_cont)

psu$habitat = factor(psu$habitat, levels = unique(all_psu$habitat))

# Plot the PSU
example_PSU_plot = ggplot(psu)+
  geom_sf(aes(fill = habitat))+
  annotation_scale() + colscale + ggtitle(paste0("PSU_ID = ",example_psu))+
  geom_sf(data = psu_cont, aes(col = elevation),fill = "transparent")

pdf("figures/example_PSU_plot.pdf", width = 8,height = 6)
print(example_PSU_plot)
dev.off()

# --------------------------------------------
# Select a sample using BAS
# --------------------------------------------

# define sample size target
n_stratum = psu$max_n
names(n_stratum) = psu$habitat
n_stratum[n_stratum>20] = 20   # Place a maximum of 20 ARUs in each stratum/PSU

# BAS
bas_samp = NULL
while(is.null(bas_samp)){
  bb_psu <- buildMS(shp = psu)
  bas_samp <- tryCatch(
    {masterSample(psu, N = n_stratum, bb = bb_psu, stratum = "habitat")},
    error = function(cond){NULL}
  )
}
attributes(bb_psu) # Random seed for this PSU

# Extract elevation contour information
bas_samp = bas_samp %>% st_join(psu_cont)

# Distances between points
dists = bas_samp %>% st_distance()
min_dist = apply(dists, 1, function(x){min(x[x!=0])})
bas_samp$nearest_point_m = min_dist

# Plot the selected sample locations
example_BAS_sample_plot = ggplot(psu)+
  geom_sf(aes(fill = habitat))+
  annotation_scale() + colscale + ggtitle(paste0("PSU_ID = ",example_psu))+
  geom_sf(data = psu_cont, aes(col = elevation),fill = "transparent")+
  geom_sf(data = bas_samp)

pdf("figures/example_BAS_sample_plot.pdf", width = 8,height=4)
print(example_BAS_sample_plot)
dev.off()
