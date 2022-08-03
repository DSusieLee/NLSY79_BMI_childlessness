library(tidyverse)
library(rstanarm)
library(loo)
library(bayesplot)

# data prep ---------------------------------------------------------------

data_list <- 
  list(df = readRDS('./Data/220118_df.rds') %>% 
         filter(!(racenew %in% "Other") &
                  last_surveyage>=40 &
                  !is.na(childless)) %>%
         mutate(childless = as.factor(childless),
                bcohort = as.factor(AGEATINT_1979),
                racenew = factor(racenew, 
                                 levels = c("Whites","Blacks","Hispanics"))),
       
       dm = readRDS('./Data/220118_dm.rds') %>% 
         filter(!(racenew %in% "Other") &
                  last_surveyage>=40 &
                  !is.na(childless)) %>%
         mutate(childless = as.factor(childless),
                bcohort = as.factor(AGEATINT_1979),
                racenew = factor(racenew, 
                                 levels = c("Whites","Blacks","Hispanics"))))

# estimating models -------------------------------------------------------

SEED = 52421
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

for(i in 1:2){
  
  curdt <- data_list[[i]]
  cur_sex <- ifelse(i == 1, 'women', 'men')
  
  post1 <- stan_glm(childless ~ BMI_pred_cat + bcohort, 
                    data = curdt,
                    family = binomial(link = "logit"), 
                    prior = t_prior,
                    iter = 5000,
                    seed = SEED)
  
  post2 <- update(post1, .~. + racenew)
  post3 <- update(post1, .~. + BMI_pred_cat*racenew)
  
  #model comparison
  
  loo_compare(loo(post1, cores = 2),
              loo(post2, cores = 2),
              loo(post3, cores = 2))
  
  #trace plots
  fit <- as.array(post3)
  dimnames(fit)[[2]] <- c("chain1", "chain2", "chain3", "chain4")
  dimnames(fit)[[3]] <- c("Intercept \n(Healthy BMI & Whtes)",
                          "Underweight", 
                          "Overweight", 
                          "Obese",
                          "Birth cohort 1964",
                          "Birth cohort 1963",
                          "Birth cohort 1962",
                          "Birth cohort 1961",
                          "Birth cohort 1960",
                          "Birth cohort 1959",
                          "Birth cohort 1958",
                          "Birth cohort 1957",
                          "Blacks",
                          "Hispanics",
                          "Underweight & Blacks",
                          "Overweight & Blacks",
                          "Obese & Blacks",
                          "Underweight & Hispanics",
                          "Overweight & Hispanics",
                          "Obese & Hispanics")
  mcmc_trace(fit)
  filename <- paste("./Figures/", Sys.Date(), "_traceplot_", cur_sex, "_withbcohort.png", sep = "")
  ggsave(filename, type = "cairo")

  #model outputs
  mcmc_areas(fit,
             prob = 2/3) +
    labs(
      title = paste(cur_sex, ":", "Posterior distribution with medians and 2/3 of probability mass (shaded) intervals", sep = "")
    )
  filename <- paste("./Figures/", Sys.Date(), "_posterior_", cur_sex, "_withbcohort.png", sep = "")
  ggsave(filename, type = "cairo")
  
  #save 
  filename <- paste("./Results/", Sys.Date(), "_stanglm_", cur_sex, "_withbcohort.rds", sep = "")
  saveRDS(post3, filename)
  
  filename <- paste("./Results/", Sys.Date(), "_stanglm_", cur_sex, "_onlybmi.rds", sep = "")
  saveRDS(post1, filename)
}

#women
# elpd_diff se_diff
# post3  0.0       0.0   
# post2 -0.9       3.3   
# post1 -8.6       5.5  

#men
# elpd_diff se_diff
# post2  0.0       0.0   
# post1 -0.9       2.4   
# post3 -4.4       2.2  