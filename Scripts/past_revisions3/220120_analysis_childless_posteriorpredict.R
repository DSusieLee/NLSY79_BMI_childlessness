library(bayesplot)
library(rstanarm)
library(loo)
library(hrbrthemes)
library(bayestestR)

racegroup <- c("Blacks","Hispanics","Whites")
bmigroup <- c("U","H","OV","O")

# simulate contrasts: only bmi --------------------------------------------

curdt <- readRDS('./Results/220119_stanglm_men_onlybmi.rds')
curdt <- readRDS('./Results/220119_stanglm_women_onlybmi.rds')

numsim <- 1e4

simdt <- data.frame(underweight = rep(NA, numsim*3),
                    healthy = rep(NA, numsim*3),
                    overweight = rep(NA, numsim*3),
                    obese = rep(NA, numsim*3)
)

k <- 1

for(i in 1:numsim){
  
  childless_sim <- posterior_predict(curdt, 
                                     newdata = data.frame(BMI_pred_cat = bmigroup),
                                     draws = 4000,
                                     seed = i) %>% 
    colMeans()
  
  simdt[k, ] <- as.numeric(childless_sim)
  
  k <- k + 1
  
}

simdt <- simdt %>%
  mutate(UD_H = underweight-healthy,
         OV_H = overweight-healthy,
         OB_H = obese-healthy) 

# simulate contrasts: race and bmi ------------------------------------------------------

curdt <- readRDS('./Results/220119_stanglm_men.rds')
curdt <- readRDS('./Results/220119_stanglm_women.rds')

numsim <- 1e4

simdt <- data.frame(race = rep(racegroup, numsim),
                    underweight = rep(NA, numsim*3),
                    healthy = rep(NA, numsim*3),
                    overweight = rep(NA, numsim*3),
                    obese = rep(NA, numsim*3)
)

k <- 1

for(i in 1:numsim){
  
  childless_sim <- posterior_predict(curdt, 
                                     newdata = data.frame(BMI_pred_cat = bmigroup,
                                                          racenew = rep(racegroup, each = 4)),
                                     draws = 4000,
                                     seed = i) %>% 
    colMeans()
  
  simdt[k, 2:5] <- as.numeric(childless_sim)[1:4]
  simdt[k+1, 2:5] <- as.numeric(childless_sim)[5:8]
  simdt[k+2, 2:5] <- as.numeric(childless_sim)[9:12]
  
  k <- k + 3
  
}

simdt <- simdt %>%
  mutate(UD_H = underweight-healthy,
         OV_H = overweight-healthy,
         OB_H = obese-healthy) 
  
saveRDS(simdt, './Data/220119_simdt_men.rds')
