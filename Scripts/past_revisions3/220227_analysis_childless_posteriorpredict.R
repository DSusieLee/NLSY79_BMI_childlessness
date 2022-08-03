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
  
  curdt <- data_list1[[1]]
  cur_sex <- ifelse(i == 1, 'women', 'men')
  
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
  
  filename <- past("./Data/", Sys.Date(), "_simdt_", cur_sex, "_bmionly.rds", sep = "")
  saveRDS(simdt, filename)
  
}

# simulate contrasts: race and bmi ------------------------------------------------------

for(i in 1:2){
  
  curdt <- data_list1[[1]]
  cur_sex <- ifelse(i == 1, 'women', 'men')
  
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
  
  filename <- past("./Data/", Sys.Date(), "_simdt_", cur_sex, "_bmirace.rds", sep = "")
  saveRDS(simdt, filename)
  
}
