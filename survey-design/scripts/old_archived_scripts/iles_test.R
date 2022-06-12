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
# Set a maximum sampling intensity within a stratum, use this to define max sample size
# --------------------------------------------
ARU_area = pi*300^2
all_psu$max_n = ( all_psu$Shape_Area_m / ARU_area ) %>% ceiling()

# Place a maximum of 20 locations in each habitat/PSU
all_psu$n = all_psu$max_n
all_psu$n[all_psu$n>20] = 20

# Examine totals
sample_size_total = all_psu %>%
  as.data.frame() %>%
  group_by(habitat) %>%
  summarize(n = sum(n))

sample_size_total

# --------------------------------------------
# Loop through PSUs and select samples
# --------------------------------------------

all_results = data.frame()

for (seed in 1:100){

  for (psu_id in unique(all_psu$PSU_ID)){

    set.seed(seed)

    psu = all_psu[all_psu$PSU_ID == psu_id,]
    psu_cont = all_psu_contour[all_psu_contour$PSU_ID == psu_id,]
    psu_cont$elevation = as.numeric(psu_cont$low_cont)

    # sample size target for this psu
    n_stratum = psu$n
    names(n_stratum) = psu$habitat

    # BAS
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

    # Distances between points
    dists = bas_samp %>% st_distance()
    min_dist = apply(dists, 1, function(x){min(x[x!=0])})
    bas_samp$nearest_point_m = min_dist

    bas_samp_df = as.data.frame(bas_samp) %>%
      dplyr::select(-geometry) %>%
      add_column(seed = seed)

    all_results = rbind(all_results, bas_samp_df)
  }
  print(seed)
}

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
  select(PSU_ID,habitat,Area_ha) %>%
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
  select(PSU_ID,alpine,subalpine,uppermontane,lon,lat) %>%
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