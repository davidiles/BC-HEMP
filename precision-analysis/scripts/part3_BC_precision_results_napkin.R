library(tidyverse)
library(jagsUI)
library(ggthemes)
library(ggrepel)
library(pwr)

rm(list = ls())

setwd("~/iles_ECCC/Landbirds/BC-landbirds/HEMP-precision-analysis/scripts")

load(file = "../output/sp_counts.RData")

# ---------------------------
# "Back of the napkin" version of a power analysis (using t-tests)
# ---------------------------
all_results_napkin = data.frame()

for (sp in unique(sp_counts$species)){
  if (file.exists(file = paste0("../output/",sp,"_sim_results_napkin.RData"))){
    load(file = paste0("../output/",sp,"_sim_results_napkin.RData"))
    all_results_napkin = rbind(all_results_napkin, sp_sim_results)
  }
}

all_results_napkin = subset(all_results_napkin,expected_1 >0)


napkin_stats = all_results_napkin %>%
  group_by(species,nARU,HABITAT) %>%
  summarize(n = n(),
            expected = mean(expected_1),
            se_expected = sd(expected_1),
            pchange_mean = mean(pchange),
            pchange_se = sd(pchange),
            pchange_sd = sd(pchange)*sqrt(mean(nARU)),
            cohen_d = 30 / (sd(pchange)*sqrt(mean(nARU))))
napkin_stats      

napkin_stats$power_napkin = NA
for (i in 1:nrow(napkin_stats)){
  
  pwr = pwr.t.test(d = napkin_stats$cohen_d[i],
                   n = napkin_stats$nARU[i],
                   sig.level = 0.1,
                   type = "one.sample",
                   alternative = "two.sided")
  napkin_stats$power_napkin[i] = pwr$power
  
}

# Power to detect change
change_plot = ggplot(napkin_stats,aes(x = nARU, y = power_napkin, col = power_napkin>0.8, 
                                      linetype = species))+
  geom_hline(yintercept = 0.8, linetype = 2)+
  geom_line(data = napkin_stats,aes(x = nARU, y = power_napkin, linetype = species), col = "gray80") +
  scale_linetype_manual(values = rep(1, length(unique(napkin_stats$species))), guide = "none")+
  geom_point()+
  scale_color_manual(values=c("orangered","dodgerblue"), name = "Power > 80%")+
  geom_label_repel(data = subset(napkin_stats, nARU == 500), 
                   aes(x = nARU, y = power_napkin, label = species, col = power_napkin>0.8), 
                   size = 1.5,segment.alpha = 0.2, nudge_x = 100, direction="y")+
  coord_cartesian(ylim = c(0,1.4),xlim=c(min(napkin_stats$nARU),max(napkin_stats$nARU)+150))+
  facet_grid(.~HABITAT)+
  theme_bw()+
  ylab('Power')+
  xlab("nARU")+
  ggtitle("Power to 'detect' a 30% decline\n(Probability the 90% CRI does not contain 0)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(0,max(napkin_stats$nARU),100))+
  scale_y_continuous(breaks = seq(0,1,0.2))

pdf(file = "../figures/power_for_change.pdf", width = 10, height = 6)
change_plot
dev.off()

# CV of expected counts during a single round of sampling
cv_plot = ggplot(napkin_stats,aes(x = nARU, y = se_expected/expected, col = power_napkin>0.8, linetype = species))+
  geom_line(data = napkin_stats,aes(x = nARU, y = se_expected/expected, linetype = species), col = "gray80") +
  scale_linetype_manual(values = rep(1, length(unique(napkin_stats$species))), guide = FALSE)+
  geom_point()+
  scale_color_manual(values=c("orangered","dodgerblue"), name = "Power >80% for 30% decline")+
  geom_label_repel(data = subset(napkin_stats, nARU == 500), 
                   aes(x = nARU, y = se_expected/expected, label = species, col = power_napkin>0.8), 
                   size = 1.5,segment.alpha = 0.2, nudge_x = 100, direction="y")+
  coord_cartesian(xlim=c(min(napkin_stats$nARU),max(napkin_stats$nARU)+150))+
  facet_grid(.~HABITAT)+
  theme_bw()+
  ylab('CV of relative density estimate (SE/expected)')+
  xlab("nARU")+
  ggtitle("CV of Density Estimate")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(0,max(napkin_stats$nARU),100))+
  scale_y_continuous(breaks = seq(0,1,0.2))
pdf(file = "../figures/CV_rel_dens.pdf", width = 10, height = 6)
cv_plot
dev.off()

# Number of species in each habitat type
dat = read.csv('../data/martin_dat.csv')

# Convert to long format
dat_long = gather(dat, species, count, GCRF:WWCR, factor_key=TRUE)
dat_long$HABITAT = factor(dat_long$HABITAT, levels = c("UM","SA","AL"))

hab_species = dat_long %>%
  group_by(species,HABITAT) %>%
  summarize(hab_count = sum(count),
            hab_mean = mean(count/OFFSET)) %>%
  subset(hab_count > 0) %>%
  group_by(HABITAT) %>%
  summarize(n_species = length(unique(species)),
            n_common_species = sum(hab_mean > 0.1))

prop_species_monitored = napkin_stats %>%
  full_join(hab_species) %>%
  group_by(HABITAT,nARU) %>%
  summarize(prop_monitored_total = sum(power_napkin > 0.8)/mean(n_species),
            prop_monitored_analyzed = sum(power_napkin > 0.8)/length(unique(species)),
            n_species_total = mean(n_species),
            n_species_analyzed = length(unique(species)))

ggplot(prop_species_monitored)+
  geom_point(aes(x = nARU, y = prop_monitored_analyzed))+
  geom_line(aes(x = nARU, y = prop_monitored_analyzed)) +
  facet_grid(.~HABITAT)+
  theme_bw()+
  ylab("Proportion Species Adequately Monitored")+
  xlab("nARU")+
  ggtitle("Proportion species adequately monitored")+
  guides(color = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(min(napkin_stats$nARU),max(napkin_stats$nARU),100))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
