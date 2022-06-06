library(tidyverse)
library(jagsUI)
library(ggthemes)
library(ggrepel)
library(pwr)

rm(list = ls())

setwd("~/Projects/BC_landbird_program/BC_HEMP/scripts")

load(file = "../output/sp_counts.RData")

all_results = data.frame()

for (sp in unique(sp_counts$species)){
  if (file.exists(file = paste0("../output/",sp,"_sim_results.RData"))){
    load(file = paste0("../output/",sp,"_sim_results.RData"))
    all_results = rbind(all_results, sp_sim_results)
  }
}

all_results = subset(all_results, Rhat_1 <= 1.3 & Rhat_2 <= 1.3)

# Calculate simulation statistics for each species/habitat/nARU combination
all_results$power = all_results$prob_decline_est >= 0.95 # average across repeated simulations to obtain power

sim_stats = all_results %>%
  group_by(species,nARU,HABITAT) %>%
  summarize(mc_runs = n()) %>%
  full_join(all_results) %>%
  group_by(species,nARU,HABITAT) %>%
  summarize_all(.funs = mean)

# ggplot(sim_stats,aes(x = nARU, y = power, col = HABITAT))+
#   geom_point()+
#   geom_line() +
#   facet_grid(species~HABITAT)+
#   theme_bw()+
#   ggtitle("Power to 'detect' a 30% decline\n(Probability the 90% CRI does not contain 0)")

ggplot(sim_stats,aes(x = nARU, y = power, col = species))+
  geom_hline(yintercept = 0.8, linetype = 2)+
  geom_point()+
  geom_line() +
  geom_label_repel(data = subset(sim_stats, nARU == 300), 
                   aes(x = nARU, y = power, label = species, col = species), 
                   size = 2,segment.alpha = 0.2, nudge_x = 10)+
  coord_cartesian(xlim=c(min(sim_stats$nARU),max(sim_stats$nARU)+20),
                  ylim = c(0,1))+
  facet_grid(HABITAT~.)+
  theme_bw()+
  ggtitle("Power to 'detect' a 30% decline\n(Probability the 90% CRI does not contain 0)")+
  guides(color = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = unique(sim_stats$nARU))

# To check how many iterations were completed
sim_stats %>% select(species,nARU,HABITAT,mc_runs,hab_count) %>% as.data.frame() 

# ---------------------------
# Compare results to "back of napkin" method using t tests
# ---------------------------
all_results_napkin = data.frame()

for (sp in unique(sp_counts$species)){
  if (file.exists(file = paste0("../output/",sp,"_sim_results_napkin.RData"))){
    load(file = paste0("../output/",sp,"_sim_results_napkin.RData"))
    all_results_napkin = rbind(all_results_napkin, sp_sim_results)
  }
}

napkin_stats = all_results_napkin %>%
  group_by(species,nARU,HABITAT) %>%
  summarize(pchange_mean = mean(pchange),
            pchange_se = sd(pchange),
            pchange_sd = sd(pchange)*sqrt(mean(nARU)),
            cohen_d = 30 / (sd(pchange)*sqrt(mean(nARU))))
napkin_stats      

napkin_stats$power_napkin = NA
for (i in 1:nrow(napkin_stats)){
  
  pwr = pwr.t.test(d = napkin_stats$cohen_d[i],
                   n = napkin_stats$nARU[i],
                   sig.level = 0.05,
                   type = "one.sample",
                   alternative = "two.sided")
  napkin_stats$power_napkin[i] = pwr$power
  
}

# Comparison
napkin_compare = napkin_stats %>% full_join(sim_stats)
napkin_compare = napkin_compare %>% mutate(label = paste0(species,"-",HABITAT,"-",nARU))
ggplot(napkin_compare,aes(x = power, y = power_napkin, label = label))+
  geom_abline(slope = 1)+
  geom_hline(yintercept = 0.8, linetype = 2)+
  geom_vline(xintercept = 0.8, linetype = 2)+
  geom_label_repel(size=2,alpha = 0.5)+
  coord_cartesian(xlim = c(0,1),ylim=c(0,1))+
  geom_point()+
  theme_bw()+
  ggtitle("Power to 'detect' a 30% decline\n(Probability the 90% CRI does not contain 0)")+
  guides(color = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())