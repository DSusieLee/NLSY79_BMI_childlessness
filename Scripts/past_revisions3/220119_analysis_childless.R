library(tidyverse)
library(tidybayes)
library(caret)
library(GGally)
library(ggplot2)
library(bayesplot)
library(rstanarm)
library(loo)
library(hrbrthemes)
library(bayestestR)
library(cowplot)

SEED = 52421

# data prep ---------------------------------------------------------------

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(!(racenew %in% "Other") &
           last_surveyage>=40 &
         !is.na(childless)) %>%
  mutate(childless = as.factor(childless),
         BMI_pred_cat = factor(BMI_pred_cat, 
                               levels = c('H','U', 'OV', 'O')),
         racenew = factor(racenew, 
                          levels = c("Whites","Blacks","Hispanics")))
dm <- readRDS('./Data/220118_dm.rds') %>% 
  filter(!(racenew %in% "Other") &
           last_surveyage>=40 &
         !is.na(childless)) %>%
  mutate(childless = as.factor(childless),
         BMI_pred_cat = factor(BMI_pred_cat, 
                               levels = c('H','U', 'OV', 'O')),
         racenew = factor(racenew, 
                          levels = c("Whites","Blacks","Hispanics")))

# prior -------------------------------------------------------------------

t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# models: women ------------------------------------------------------------------

post1 <- stan_glm(childless ~ BMI_pred_cat, 
                  data = df,
                  family = binomial(link = "logit"), 
                  prior = t_prior, 
                  prior_intercept = t_prior, 
                  seed = SEED)

post2 <- update(post1, .~. + racenew)
post3 <- update(post1, .~. + BMI_pred_cat*racenew)

loo_compare(loo(post1, cores = 2),
            loo(post2, cores = 2),
            loo(post3, cores = 2))
# elpd_diff se_diff
# post3  0.0       0.0   
# post2 -0.4       2.1   
# post1 -8.3       4.9   

saveRDS(post1, './Results/220119_stanglm_women_onlybmi.rds')
saveRDS(post3, './Results/220119_stanglm_women.rds')

# models: men ---------------------------------------------------------------------

post1 <- stan_glm(childless ~ BMI_pred_cat, 
                  data = dm,
                  family = binomial(link = "logit"), 
                  prior = t_prior, 
                  prior_intercept = t_prior, 
                  seed = SEED)

post2 <- update(post1, .~. + racenew)
post3 <- update(post1, .~. + BMI_pred_cat*racenew)

loo_compare(loo(post1, cores = 2),
            loo(post2, cores = 2),
            loo(post3, cores = 2))

saveRDS(post1, './Results/220119_stanglm_men_onlybmi.rds')
saveRDS(post3, './Results/220119_stanglm_men.rds')

# elpd_diff se_diff
# post2  0.0       0.0   
# post1 -1.1       2.4   
# post3 -4.3       1.7   