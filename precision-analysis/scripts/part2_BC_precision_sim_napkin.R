library(tidyverse)
library(jagsUI)
library(pwr)

rm(list = ls())

setwd("~/Projects/BC_landbird_program/BC_HEMP/scripts")

# ------------------------------------------------------------------------------
# Conduct "back of napkin" power analysis to evaluate sampling variance in estimate of mean count,
# given the glm parameters

# Note that this script must be run *after* part1
# ------------------------------------------------------------------------------

load(file = "../output/sp_counts.RData")

start = Sys.time()
# Monte Carlo simulations used to calculate expected standard error of mean for this survey scenario
for (sp in unique(sp_counts$species)){
  
  sp_dat = subset(sp_counts, species == sp)
  sp_sim_results = data.frame()
  for (i in 1:nrow(sp_dat)){
    
    sp_dat_i = sp_dat[i,]
    for (nARU in seq(50,500,50)){
      
      if (file.exists(file = paste0("../output/",sp,"_sim_results.RData"))) load(file = paste0("../output/",sp,"_sim_results_napkin.RData"))
      
      for (mc_rep in 1001:2500){
        
        
        
        # Prepare dataframes for simulation
        simdat_1 = simdat_2 = slice(sp_dat_i, rep(1:n(), each = nARU)) %>% mutate(visits = 6)
        
        # ---------------------------------------------
        # Simulate/analyze counts in first time period
        # ---------------------------------------------
        simdat_1$mu = exp(rnorm(nrow(simdat_1), simdat_1$alpha,simdat_1$sigma)) * simdat_1$visits
        simdat_1$z = rbinom(nrow(simdat_1),1,simdat_1$p)
        simdat_1$y = rpois(nrow(simdat_1),simdat_1$mu) * simdat_1$z
        expected_1 = mean(simdat_1$y/simdat_1$visits)
        
        # ---------------------------------------------
        # Simulate/analyze counts in second time period
        # ---------------------------------------------
        simdat_2$alpha = simdat_2$alpha + log(0.7)  # Reduce density in simdat_2 by 30%
        simdat_2$mu = exp(rnorm(nrow(simdat_2), simdat_2$alpha,simdat_2$sigma)) * simdat_2$visits
        simdat_2$z = rbinom(nrow(simdat_2),1,simdat_2$p)
        simdat_2$y = rpois(nrow(simdat_2),simdat_2$mu) * simdat_2$z
        expected_2 = mean(simdat_2$y/simdat_2$visits)
        
        # ---------------------------------------------
        # Change estimate between time periods
        # ---------------------------------------------
        pchange = 100 * (expected_2 - expected_1) / expected_1
        
        mc_rep_results = sp_dat_i %>% mutate(mc_rep = mc_rep,
                                           nARU = nARU,
                                           nvisit = 6,
                                           expected_1 = expected_1,
                                           expected_2 = expected_2,
                                           pchange = pchange)
        
        sp_sim_results = rbind(sp_sim_results, mc_rep_results) %>% as.data.frame()
      }
      
      save(sp_sim_results, file = paste0("../output/",sp,"_sim_results_napkin.RData"))
      
    } # mc_rep
    
  } # nARU
  
  print(sp)
} # sp
end = Sys.time()
end-start
