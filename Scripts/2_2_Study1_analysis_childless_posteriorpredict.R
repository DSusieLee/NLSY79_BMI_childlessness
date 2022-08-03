library(bayesplot)
library(rstanarm)
library(loo)
library(hrbrthemes)
library(bayestestR)

# data prep ---------------------------------------------------------------

data_list1 <- 
  list(readRDS('./Results/2022-03-01_stanglm_women_onlybmi.rds'),
       readRDS('./Results/2022-03-01_stanglm_men_onlybmi.rds'))

data_list2 <- 
  list(readRDS('./Results/2022-03-01_stanglm_women_withbcohort.rds'),
       readRDS('./Results/2022-03-01_stanglm_men_withbcohort.rds'))

racegroup <- c("Blacks","Hispanics","Whites")
bmigroup <- c("U","H","OV","O")

numsim <- 1e4

# simulate contrasts: only bmi --------------------------------------------

for(i in 1:2){
  
  curdt <- data_list1[[i]]
  cur_sex <- ifelse(i == 1, 'women', 'men')
  
  simdt <- data.frame(underweight = rep(NA, numsim),
                      healthy = rep(NA, numsim),
                      overweight = rep(NA, numsim),
                      obese = rep(NA, numsim)
  )
  
  k <- 1
  
  for(j in 1:numsim){
    
    childless_sim <- posterior_predict(curdt, 
                                       newdata = data.frame(BMI_pred_cat = bmigroup,
                                                            bcohort = "19"),
                                       draws = 4000,
                                       seed = j) %>% 
      colMeans()
    
    simdt[k, ] <- as.numeric(childless_sim)
    
    #update the row number so that
    #results from the next iteration can be stored
    k <- k + 1
    
  }
  
  simdt <- simdt %>%
    mutate(UD_H = underweight-healthy,
           OV_H = overweight-healthy,
           OB_H = obese-healthy) 
  
  filename <- paste("./Data/", Sys.Date(), "_simdt_", cur_sex, "_bmionly.rds", sep = "")
  saveRDS(simdt, filename)
  
}

# simulate contrasts: race and bmi ------------------------------------------------------

for(i in 1:2){
  
  curdt <- data_list2[[i]]
  cur_sex <- ifelse(i == 1, 'women', 'men')
  
  simdt <- data.frame(race = rep(racegroup, numsim),
                      underweight = rep(NA, numsim*3),
                      healthy = rep(NA, numsim*3),
                      overweight = rep(NA, numsim*3),
                      obese = rep(NA, numsim*3)
  )
  
  k <- 1
  
  for(j in 1:numsim){
    
    childless_sim <- posterior_predict(curdt, 
                                       newdata = data.frame(BMI_pred_cat = bmigroup,
                                                            racenew = rep(racegroup, each = 4),
                                                            bcohort = rep("19", 12)),
                                       draws = 4000,
                                       seed = j) %>% 
      colMeans()
    
    simdt[k, 2:5] <- as.numeric(childless_sim)[1:4]
    simdt[k+1, 2:5] <- as.numeric(childless_sim)[5:8]
    simdt[k+2, 2:5] <- as.numeric(childless_sim)[9:12]
    
    #update the row number so that
    #results from the next iteration can be stored
    k <- k + 3
    
  }
  
  simdt <- simdt %>%
    mutate(UD_H = underweight-healthy,
           OV_H = overweight-healthy,
           OB_H = obese-healthy) 
  
  filename <- paste("./Data/", Sys.Date(), "_simdt_", cur_sex, "_bmirace.rds", sep = "")
  saveRDS(simdt, filename)
  
}
