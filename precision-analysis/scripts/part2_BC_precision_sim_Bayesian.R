library(tidyverse)
library(jagsUI)

rm(list = ls())

setwd("~/Projects/BC_landbird_program/BC_HEMP/scripts")

# ------------------------------------------------------------------------------
# Conduct precision analysis to evaluate sampling variance in estimate of mean count,
# given the glm parameters
# Assume repeated observations (within season at a site) are poisson distributed
# ------------------------------------------------------------------------------
load(file = "../output/sp_counts.RData")

sim_results_complete <- data.frame()

start = Sys.time()
# Monte Carlo simulations used to calculate expected standard error of mean for this survey scenario
for (sp in rev(unique(sp_counts$species))){
  
  sp_dat = subset(sp_counts, species == sp)
  
  # Number of habitats to evaluate for this species
  nhab = data.frame(hab_number = 1:length(unique(sp_dat$HABITAT)),
                    HABITAT = unique(sp_dat$HABITAT))
  
  # Ensure correct habitat numbers/order
  sp_dat = full_join(sp_dat,nhab) %>% arrange(hab_number)
  
  sp_sim_results = data.frame()
  
  for (nARU in c(100,200,300)){
    
    for (mc_rep in 1:250){
      if (file.exists(file = paste0("../output/",sp,"_sim_results.RData"))) load(file = paste0("../output/",sp,"_sim_results.RData"))
      
      run_complete = sp_sim_results$nARU == nARU & sp_sim_results$mc_rep == mc_rep
      if (TRUE %in% run_complete) next
      
      # Prepare dataframes for simulation
      simdat_1 = simdat_2 = slice(sp_dat, rep(1:n(), each = nARU)) %>% mutate(visits = 6)
      
      # ---------------------------------------------
      # Simulate/analyze counts in first time period
      # ---------------------------------------------
      simdat_1$mu = exp(rnorm(nrow(simdat_1), simdat_1$alpha,simdat_1$sigma)) * simdat_1$visits
      simdat_1$z = rbinom(nrow(simdat_1),1,simdat_1$p)
      simdat_1$y = rpois(nrow(simdat_1),simdat_1$mu) * simdat_1$z
      
      # Analyze data in first time period
      jags_dat_1 = list(y = simdat_1$y,HABITAT = simdat_1$hab_number,OFFSET = simdat_1$visits,nhab = max(simdat_1$hab_number),nobs = nrow(simdat_1))
      
      inits = function()list(z = rep(1,jags_dat_1$nobs),exp_alpha = exp(sp_dat$alpha))
      
      out_1 <- jags(data = jags_dat_1,
                    model.file = "zip.jags",
                    parameters.to.save = c("expected","alpha","p","sigma"),
                    inits = inits,
                    n.chains = 3,
                    n.thin = 1,
                    n.iter = 8000,
                    n.burnin = 4000,
                    parallel = TRUE,
                    n.cores = 3)
      
      # ---------------------------------------------
      # Simulate/analyze counts in second time period
      # ---------------------------------------------
      simdat_2$alpha = simdat_2$alpha + log(0.7)  # Reduce density in simdat_2 by 30%
      simdat_2$mu = exp(rnorm(nrow(simdat_2), simdat_2$alpha,simdat_2$sigma)) * simdat_2$visits
      simdat_2$z = rbinom(nrow(simdat_2),1,simdat_2$p)
      simdat_2$y = rpois(nrow(simdat_2),simdat_2$mu) * simdat_2$z
      
      # Analyze data in first time period
      jags_dat_2 = list(y = simdat_2$y,HABITAT = simdat_2$hab_number,OFFSET = simdat_2$visits,nhab = max(simdat_2$hab_number),nobs = nrow(simdat_2))
      
      inits = function()list(z = rep(1,jags_dat_2$nobs),exp_alpha = exp(sp_dat$alpha))
      
      out_2 <- jags(data = jags_dat_2,
                    model.file = "zip.jags",
                    parameters.to.save = c("expected","alpha","p","sigma"),
                    inits = inits,
                    n.chains = 3,
                    n.thin = 1,
                    n.iter = 8000,
                    n.burnin = 4000,
                    parallel = TRUE,
                    n.cores = 3)
      
      # ---------------------------------------------
      # Estimated change in expected counts between time periods
      # ---------------------------------------------
      pchange = 100 * (out_2$sims.list$expected - out_1$sims.list$expected) / out_1$sims.list$expected
      mc_rep_results = sp_dat %>% mutate(mc_rep = mc_rep,
                                         nARU = nARU,
                                         nvisit = 6)
      
      # Convergence statistics
      mc_rep_results$Rhat_1 = out_1$Rhat$expected
      mc_rep_results$Rhat_2 = out_2$Rhat$expected
      
      if (nrow(mc_rep_results) == 1){
        
        # If only a single habitat type
        mc_rep_results$expected_CV_1 = out_1$sd$expected/out_1$mean$expected
        mc_rep_results$expected_se_1 = out_1$sd$expected
        mc_rep_results$expected_CV_2 = out_2$sd$expected/out_2$mean$expected
        mc_rep_results$expected_se_2 = out_2$sd$expected
        
        mc_rep_results$pchange_est_mean = mean(pchange)
        mc_rep_results$pchange_est_se = sd(pchange)
        mc_rep_results$pchange_est_q025 = quantile(pchange,0.025)
        mc_rep_results$pchange_est_q975 = quantile(pchange,0.975)
        mc_rep_results$prob_decline_est = mean(pchange < 0)} else{
          
          # If more than 1 habitat type
          mc_rep_results$expected_CV_1 = out_1$sd$expected/out_1$mean$expected
          mc_rep_results$expected_se_1 = out_1$sd$expected
          mc_rep_results$expected_CV_2 = out_2$sd$expected/out_2$mean$expected
          mc_rep_results$expected_se_2 = out_2$sd$expected
          mc_rep_results$pchange_est_mean = apply(pchange,2,mean)
          mc_rep_results$pchange_est_se = apply(pchange,2,sd)
          mc_rep_results$pchange_est_q025 = apply(pchange,2,function(x) quantile(x,0.025))
          mc_rep_results$pchange_est_q975 = apply(pchange,2,function(x) quantile(x,0.975))
          mc_rep_results$prob_decline_est = apply(pchange,2,function(x) mean(x < 0))}
      
      sp_sim_results = rbind(sp_sim_results, mc_rep_results) %>% as.data.frame()
      save(sp_sim_results, file = paste0("../output/",sp,"_sim_results.RData"))
      
    } # mc_rep
    
  } # nARU
  
  
  sim_results_complete = rbind(sim_results_complete, sp_sim_results)
} # sp
end = Sys.time()

end-start

