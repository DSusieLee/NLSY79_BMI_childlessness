library(tidyverse)
library(rstanarm)
library(loo)

dm <- readRDS('./Data/211214_dm.rds') %>% 
  filter(AGE1B_XRND != -999 & !(racenew %in% "Other")) %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND))

df <- readRDS('./Data/211214_df.rds') %>% 
  filter(AGE1B_XRND != -999 & !(racenew %in% "Other")) %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND))

racegroup <- c("Blacks","Hispanics","Whites")

dm$BMI_pred_cat <- factor(dm$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))
df$BMI_pred_cat <- factor(df$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))


# model: women ------------------------------------------------------------

mod1 <- stan_surv(formula = Surv(age1b, status) ~ BMI_pred_cat,
                  data = df,
                  basehaz = "exp",
                  iter = 1000)

mod1_weibull <- update(mod1, basehaz = "weibull")
mod1_gompertz <- update(mod1, basehaz = "gompertz")
mod1_bspline <- update(mod1, basehaz = "bs")
mod1_mspline1 <- update(mod1, basehaz = "ms")
mod1_mspline2 <- update(mod1, basehaz = "ms", basehaz_ops = list(df = 9))

loo_compare(loo(mod1),
            loo(mod1_weibull),
            loo(mod1_gompertz),
            loo(mod1_bspline),
            loo(mod1_mspline1),
            loo(mod1_mspline2))

saveRDS(mod1, './Data/220119_survival_women.rds')

ps <- posterior_survfit(mod1,
                        newdata = data.frame(BMI_pred_cat = c('U', 'H', 'OV', 'O')))

panel_labels <- c('1' = "Underweight",
                  '2' = "Healthy",
                  '3' = "Overweight",
                  '4' = "Obese")
plot(ps) +
  facet_grid(.~id, labeller = labeller(id = panel_labels))



