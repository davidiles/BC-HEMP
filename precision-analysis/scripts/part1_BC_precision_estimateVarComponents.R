library(tidyverse)
library(jagsUI)

rm(list = ls())

setwd("~/Projects/BC_landbird_program/BC_HEMP/scripts")

# Model script
sink("zip.jags")
cat("

    model {

      p ~ dunif(0,1)      # (1-p) is zero inflation probability
      sigma ~ dunif(0,2)  # spatial SD
      tau = pow(sigma,-2)
      
      for (hab in 1:nhab){
        exp_alpha[hab] ~ dunif(0,2)   
        alpha[hab] = log(exp_alpha[hab])
        expected[hab] = exp(alpha[hab] + 0.5*sigma*sigma)*p    # Expected count | exp_alpha, sigma, p
      }
      
      for (i in 1:nobs){
      
        # 'Occupancy' status at each ARU
        z[i] ~ dbern(p)
        
        # density | occupancy
        log_mu[i] ~ dnorm(alpha[HABITAT[i]] + log(OFFSET[i]),tau)
        
        # Data
        y[i] ~ dpois(exp(log_mu[i])*z[i])
        
        # Posterior predictive check
        sim_z[i] ~ dbern(p)
        sim_log_mu[i] ~ dnorm(alpha[HABITAT[i]] + log(OFFSET[i]),tau)
        sim_y[i] ~ dpois(exp(sim_log_mu[i])*sim_z[i])
        
        # Squared error
        sq_err_obs[i] = pow(y[i] - exp(log_mu[i])*p,2)
        sq_err_pred[i] = pow(sim_y[i] - exp(sim_log_mu[i])*p,2)
        
      }
      
      # Derived quantities (for calculating 'Bayesian p-value')
      RMSE_obs = sqrt(sum(sq_err_obs[])/nobs)
      RMSE_pred = sqrt(sum(sq_err_pred[])/nobs)
      
    }
",fill = TRUE)
sink()

# # Read in Martin's data
# dat = read.csv('../data/martin_dat.csv')
# 
# # Convert to long format
# dat_long = gather(dat, species, count, GCRF:WWCR, factor_key=TRUE)
# dat_long
# 
# dat_long$HABITAT = factor(dat_long$HABITAT, levels = c("UM","SA","AL"))
# 
# # Calculate total counts in each habitat for each species; restrict analyses to only those combinations
# # where at least 5 individuals were seen in the habitat, AND at least 10 individuals were seen in total across
# # all habitats
# 
# hab_counts = dat_long %>%
#   group_by(species,HABITAT) %>%
#   summarize(hab_count = sum(count),
#             hab_mean = mean(count/OFFSET))
# 
# sp_counts = dat_long %>%
#   group_by(species) %>%
#   summarize(total_count = sum(count)) %>%
#   full_join(hab_counts)
# 
# sp_counts = subset(sp_counts, hab_count > 5 & total_count >= 10)
# length(unique(to_analyze$species)) # 35 species, 63 species/habitat combinations
# 
# 
# all_results = data.frame()
# for (sp in unique(sp_counts$species)){
#   
#   sp_analyze = subset(sp_counts, species == sp)
#   
#   sp_dat = subset(dat_long, species == sp & HABITAT %in% sp_analyze$HABITAT)
#   
#   # Number of habitats to evaluate
#   nhab = data.frame(hab_number = 1:length(unique(sp_dat$HABITAT)),
#                     HABITAT = unique(sp_dat$HABITAT))
#   
#   # Mean counts
#   means = sp_dat %>% group_by(HABITAT) %>% summarize(mean = mean(count/OFFSET)) %>% full_join(nhab) %>%
#     arrange(hab_number)
#   
#   # Ensure correct habitat numbers
#   sp_dat = full_join(sp_dat,means)
#   
#   jags.data = list(y = sp_dat$count,
#                    HABITAT = sp_dat$hab_number,
#                    OFFSET = sp_dat$OFFSET,
#                    nhab = max(sp_dat$hab_number),
#                    nobs = nrow(sp_dat))
#   
#   inits = function()list(z = rep(1,jags.data$nobs),
#                          exp_alpha = means$mean)
#   
#   out <- jags(data = jags.data,
#               model.file = "zip.jags",
#               parameters.to.save = c("expected","alpha","p","sigma","RMSE_obs","RMSE_pred"),
#               inits = inits,
#               n.chains = 3,
#               n.thin = 1,
#               n.iter = 2000,
#               n.burnin = 1000)
#   
#   # Habitat-specific intercepts
#   sp_results = data.frame(species = sp,
#                           HABITAT = means$HABITAT,
#                           alpha = out$mean$alpha,
#                           p = out$mean$p,
#                           sigma = out$mean$sigma,
#                           expected = out$mean$expected,
#                           se_expected = out$sd$expected,
#                           Bayesian_pval = mean(out$sims.list$RMSE_obs > out$sims.list$RMSE_pred))
#   
#   all_results = rbind(all_results, sp_results)
#   
#   print(sp)
# }
# 
# # Attach results
# sp_counts = sp_counts %>%
#   full_join(all_results) %>%
#   na.omit()
# 
# # Expected counts are reasonable
# ggplot(sp_counts)+
#   geom_point(aes(x = hab_mean, y = expected))+
#   facet_grid(HABITAT~.)+
#   xlab("Observed Mean Count")+
#   ylab("Expected from GLM")+
#   geom_abline()
# 
# save(sp_counts, file = "../output/sp_counts.RData")

