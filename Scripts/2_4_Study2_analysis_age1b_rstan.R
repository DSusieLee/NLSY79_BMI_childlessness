library(tidyverse)
library(rstanarm)
library(hrbrthemes) 
library(bayestestR)
library(cowplot)
library(bayesplot)

dm <- readRDS('./Data/220118_dm.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         age1b = age1b-15,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('H', 'U', 'OV', 'O')),
         bcohort = as.factor(AGEATINT_1979))

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         age1b = age1b-15,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('H', 'U', 'OV', 'O')),
         bcohort = as.factor(AGEATINT_1979))

data_list <- list(df, dm)

racegroup <- c("Blacks","Hispanics","Whites")

bmigroup <- unique(df$BMI_pred_cat)[2:4]
bminame <- c("Underweight","Overweight","Obese")

colors = c("grey82","grey49","grey5")

# stan_surv models --------------------------------------------------------

SEED = 52421

for(i in 1:2){
  
  curdt1 <- data_list[[i]]
  cur_sex <- ifelse(i == 1, 'women', 'men')
  
  post1 <- stan_surv(formula = Surv(age1b, status) ~ BMI_pred_cat + bcohort,
                   data = curdt1,
                   basehaz = "exp",
                   seed = SEED,
                   iter = 5000)
  
  post2 <- update(post1, .~. + racenew)
  post3 <- update(post1, .~. + BMI_pred_cat*racenew)


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
                          "Hispanics",
                          "Whites",
                          "Underweight & Hispanics",
                          "Overweight & Hispanics",
                          "Obese & Hispanics",
                          "Underweight & Whites",
                          "Overweight & Whites",
                          "Obese & Whites"
)
  mcmc_trace(fit)
  filename <- paste("./Figures/220304_traceplot_", cur_sex, "_surv.png", sep = "")
  ggsave(filename, type = "cairo")
  
  mcmc_areas(fit,
             prob = 2/3) +
    labs(
      title = paste(cur_sex, ":", "Posterior distribution with medians and 2/3 of probability mass (shaded) intervals", sep = "")
    )
  filename <- paste("./Figures/220304_posterior_", cur_sex, "_surv.png", sep = "")
  ggsave(filename, type = "cairo")
  
  filename <- paste("./Results/220304_", cur_sex, "_post1_surv.rds", sep = "")
  saveRDS(post1, filename)
  
  filename <- paste("./Results/220304_", cur_sex, "_post2_surv.rds", sep = "")
  saveRDS(post2, filename)
  
  filename <- paste("./Results/220304_", cur_sex, "_post3_surv.rds", sep = "")
  saveRDS(post3, filename)
  
  for(j in 1:3){
    
    cur_race <- racegroup[j]
    
    curdt1 <- curdt2 %>% filter(racenew %in% cur_race)
    
    #model
    mod2 <- stan_surv(formula = Surv(age1b, status) ~ BMI_pred_cat + bcohort,
                      data = curdt2,
                      basehaz = "exp",
                      seed = SEED,
                      iter = 5000)
    
    filename <- paste("./Results/220304_", cur_race, '_', cur_sex, "_onlybmi_surv.rds", sep = "")
    saveRDS(mod2, filename)
    
  }
  
}


# posterior draws ---------------------------------------------------------
# 
# #newdata
# nd <- data.frame(BMI_pred_cat = c("H",curbmi))
# 
# numsim = 1e4
# 
# for(j in 1:numsim){
#   
#   m <- posterior_survfit(mod1, type = "surv", newdata = nd) %>%
#     mutate(id = ifelse(id == 1, "Healthy BMI", curbmi))
#   
#   if(j == 1){
#     p <- ggplot(m, aes(x = time, y = median, group = id, colour = id)) +
#       geom_line()
#   }
#   
#   else{
#     p <- p + 
#       geom_line(aes(x = time, y = median, group = id, colour = id), data = m)
#     
#   }
#     
#   
# }
# 
