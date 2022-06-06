## devtools::install_github("paul-vdb/DFO-master-sample")
library(BASMasterSample)
library(sf)
library(sp)
library(data.table)
library(spsurvey)
library(dplyr)
library(ggplot2)
library(sampling)
library(units)
library(ggspatial)
library(RColorBrewer)
library(viridis)
library(tidyverse)
library(cowplot)
rm(list=ls())

setwd("~/iles_ECCC/Landbirds/BC-landbirds/HEMP-survey-design/iles")

theme_set(theme_bw())
options(ggplot2.continuous.colour = "viridis")
colscale = scale_fill_manual(values = brewer.pal(3,"Set2"))


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
# PART 2: EVALUATE SAMPLE DESIGN ACROSS REPEATED SAMPLE DRAWS
# ********************************************************************
# ********************************************************************

# --------------------------------------------
# Select a large oversample (1 ARU per ha)
# --------------------------------------------
all_psu$n_oversample = (all_psu$Shape_Area_m / 10000) %>% ceiling()

# --------------------------------------------
# Set a maximum sampling intensity within a stratum, use this to define max sample size
# --------------------------------------------
ARU_radius = 150 # units = m
ARU_area = pi*ARU_radius^2 # units are m^2 
all_psu$n = ( all_psu$Shape_Area_m / ARU_area ) %>% ceiling() # Sample size required to stay under maximum ARU density

# --------------------------------------------
# Loop through PSUs and select a large oversample (1 ARU per ha)
# Then thin this to desired ARU density
# --------------------------------------------

sample_df = NULL

for (psu_id in 92){ # unique(all_psu$PSU_ID)){

  # Set a random seed in R (Note that the seed used by BAS is different)
  R_seed = 999
  set.seed(R_seed)

  psu = all_psu[all_psu$PSU_ID == psu_id,]
  psu_cont = all_psu_contour[all_psu_contour$PSU_ID == psu_id,]
  psu_cont$elevation = as.numeric(psu_cont$low_cont)

  # sample size target (with large oversample) for this psu
  n_stratum = psu$n_oversample
  names(n_stratum) = psu$habitat

  # Select (over)sample using BAS
  bas_samp = NULL
  while(is.null(bas_samp)){
    bb_psu <- buildMS(shp = psu)
    bas_samp <- tryCatch(
      {masterSample(psu, N = n_stratum, bb = bb_psu, stratum = "habitat")},
      error = function(cond){NULL}
    )
  }
  bb_attributes = attributes(bb_psu)

  # Extract elevation contour information
  bas_samp = bas_samp %>% st_join(psu_cont)

  # ------------------------------------------------
  # Distinguish primary sample and oversample
  # ------------------------------------------------
  bas_samp$Sample_Type = "Oversample"

  for (hab in unique(bas_samp$habitat)){

    # Primary samples
    n = subset(psu, habitat == hab)$n
    if (n > 20) n = 20
    bas_samp[which(bas_samp$habitat == hab)[1:n],"Sample_Type"] = "Primary"
  }


  # Distances between points in Primary Sample
  bas_samp$nearest_point_m = NA

  dists = bas_samp %>% subset(Sample_Type == "Primary") %>% st_distance()
  min_dist = apply(dists, 1, function(x){min(x[x!=0])})

  bas_samp$nearest_point_m[which(bas_samp$Sample_Type == "Primary")] = min_dist
  bas_samp$R_seed = R_seed

  sample_df = rbind(sample_df,bas_samp)
}


# ----------------------
# Plot an example PSU
# ----------------------
psu_id = 92
psu = all_psu[all_psu$PSU_ID == psu_id,]
psu_cont = all_psu_contour[all_psu_contour$PSU_ID == psu_id,]
psu_cont$elevation = as.numeric(psu_cont$low_cont)

psu_sample = subset(sample_df, PSU_ID == psu_id)

# Plot the PSU
BAS_sample_plot = ggplot(psu)+
  geom_sf(aes(fill = habitat))+
  annotation_scale() + colscale + ggtitle(paste0("PSU_ID = ",psu_id))+
  geom_sf(data = psu_cont, aes(col = elevation),fill = "transparent")+
  
  # Full oversample
  geom_sf(data = psu_sample, alpha = 0.1) +
  geom_sf(data = subset(psu_sample, Sample_Type == "Primary"), col = "black")

BAS_sample_plot


# --------------------------------------------
# Output summary tables
# --------------------------------------------

# write.csv(all_results, file = "BAS_all_results.csv",row.names = FALSE)
# all_results = read.csv(file = "BAS_all_results.csv")

# ********************************************************************
# ********************************************************************
# PART 3: SUMMARIZE RESULTS AND GENERATE SOME MORE PLOTS
# ********************************************************************
# ********************************************************************

# --------------------------------------------
# Anticipated distances between nearest points
# --------------------------------------------
distance_df = all_results %>%
  group_by(habitat,seed) %>%
  summarize(mean_DistToNearest = mean(nearest_point_m),
            Proportion_less_than_150m = mean(nearest_point_m < 150),
            Proportion_less_than_300m = mean(nearest_point_m < 300))

ggplot(distance_df)+
  geom_boxplot(aes(x = habitat,y=mean_DistToNearest, fill = habitat))+
  scale_y_continuous(limits = c(0,1000))+
  xlab("Habitat")+
  ylab("Expected distance to nearest point")

plot_150m_spacing = ggplot(distance_df)+
  geom_boxplot(aes(x = habitat,y=Proportion_less_than_150m, fill = habitat))+
  xlab("Habitat")+
  ylab("Proportion")+
  ylim(c(0,1))+
  colscale +
  ggtitle("Proportion of points closer than 150 m")+
  theme(legend.position = "none")

plot_300m_spacing = ggplot(distance_df)+
  geom_boxplot(aes(x = habitat,y=Proportion_less_than_300m, fill = habitat))+
  xlab("Habitat")+
  ylab("Proportion")+
  ylim(c(0,1))+
  colscale+
  ggtitle("Proportion of points closer than 300 m")+
  theme(legend.position = "none")

spacing_plot = plot_grid(plot_150m_spacing,plot_300m_spacing,nrow=1,align = "hv")
spacing_plot

pdf("figures/point_spacing_plot.pdf", width = 8,height=4)
print(spacing_plot)
dev.off()

# --------------------------------------------
# Availability of habitats within overall study area
# --------------------------------------------

# Total area occupied by each habitat stratum
study_habitats = studyRegion %>%
  group_by(habitat) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))), 
            geometry = st_union(geometry)) %>%
  ungroup()

# Total area occupied by each contour band
study_contours = all_psu_contour %>%
  rename(Shape_Area_m = Shape_Area) %>%
  group_by(low_cont) %>%
  summarise(Shape_Area_m = sum(as.numeric(st_area(geometry))),
            geometry = st_union(geometry)) %>%
  ungroup()

# ----------------
# Illustrating that AL and SA are over-sampled relative to availability
# ----------------

# Available habitat in study area
available_habitat = study_habitats %>%
  as.data.frame() %>%
  select(habitat,Shape_Area_m) %>%
  mutate(Proportion = Shape_Area_m/sum(Shape_Area_m)) %>%
  add_column(Type = "Available")

sample_habitat = all_results %>%
  as.data.frame() %>%
  select(habitat) %>%
  group_by(habitat) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(Proportion = count/sum(count))%>%
  add_column(Type = "Sample")

compare_habitat = bind_rows(available_habitat,sample_habitat)

habitat_sample_plot = ggplot(compare_habitat, aes(x = habitat,y = Proportion, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.5)+
  scale_fill_manual(name = "",values = c("dodgerblue","orangered"))+
  ggtitle("Sampled habitat relative to available habitat")
habitat_sample_plot

pdf("figures/sample_habitats.pdf", width = 6,height=4)
print(habitat_sample_plot)
dev.off()

# ----------------
# Illustrating that high elevations are slightly over-sampled relative to availability
# ----------------

# Available elevation bands in study area
available_elevation = study_contours %>%
  as.data.frame() %>%
  select(low_cont,Shape_Area_m) %>%
  mutate(Proportion = Shape_Area_m/sum(Shape_Area_m)) %>%
  add_column(Type = "Available")

sample_elevation = all_results %>%
  as.data.frame() %>%
  select(low_cont) %>%
  group_by(low_cont) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(Proportion = count/sum(count))%>%
  add_column(Type = "Sample")

compare_elevation = bind_rows(available_elevation,sample_elevation)
compare_elevation$low_cont = as.numeric(compare_elevation$low_cont)

elevation_sample_plot = ggplot(compare_elevation, aes(x = low_cont,y = Proportion, col = Type)) + 
  geom_line(size=1.5)+
  scale_color_manual(name = "",values = c("dodgerblue","orangered"))+
  ggtitle("Sampled elevation relative to available elevation")
elevation_sample_plot

pdf("figures/sample_elevations.pdf", width = 6,height=4)
print(elevation_sample_plot)
dev.off()

# --------------------------------------------
# Pie charts showing proportion of habitat and size of each PSU
# --------------------------------------------
library(scatterpie)

# Wide format
psu_summary = all_psu %>% 
  as.data.frame() %>%
  mutate(Area_ha = Shape_Area_m / 10000) %>%
  dplyr::select(PSU_ID,habitat,Area_ha) %>%
  pivot_wider(names_from = habitat, values_from = Area_ha)

# Centroids of each PSU
psu_summary = all_psu %>% group_by(PSU_ID) %>% 
  summarise(geometry = sf::st_union(geometry)) %>% 
  st_centroid() %>%
  full_join(psu_summary)
psu_summary$lon = st_coordinates(psu_summary)[,1]
psu_summary$lat = st_coordinates(psu_summary)[,2]

psu_summary[is.na(psu_summary)] = 0
psu_summary_df = psu_summary %>%
  as.data.frame() %>%
  dplyr::select(PSU_ID,alpine,subalpine,uppermontane,lon,lat) %>%
  mutate(TotalArea_ha = alpine+subalpine+uppermontane)

psu_summary_df$radius = psu_summary_df$TotalArea_ha * 3

psu_habitat_map = ggplot() +
  geom_sf(data = studyRegion, fill = "gray90", col = "transparent")+
  geom_scatterpie(data = psu_summary_df,aes(x = lon, y = lat, group = PSU_ID, r = radius), cols = c("alpine","subalpine","uppermontane"), size = 0.1,alpha=0.8)+
  geom_text(data = psu_summary_df,aes(x = lon, y = lat, label = PSU_ID),size = 1)+
  colscale+
  ggtitle("Size and habitat composition of each PSU")+
  xlab("Longitude")+ylab("Latitude")

pdf("figures/psu_habitat_map.pdf", width = 8,height=8)
print(psu_habitat_map)
dev.off()