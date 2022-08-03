library(tidyverse)
library(Greg)
library(survival)
library(survminer)
library(sjPlot)

dm <- readRDS('./Data/220118_dm.rds') %>% 
  filter(AGE1B_XRND != -999 & !(racenew %in% "Other")) %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND))

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(AGE1B_XRND != -999 & !(racenew %in% "Other")) %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND))

racegroup <- c("Blacks","Hispanics","Whites")

dm$BMI_pred_cat <- factor(dm$BMI_pred_cat, 
                          levels = c('H', 'U', 'OV', 'O'))
df$BMI_pred_cat <- factor(df$BMI_pred_cat, 
                          levels = c('H', 'U', 'OV', 'O'))

# models ------------------------------------------------------------------

#women: almost linear decline in the rate as BMI increases
fit <- coxph(formula = Surv(age1b, status) ~ BMI_pred, 
             data = df)
nonlinear_fit <- update(fit, .~.-BMI_pred + pspline(BMI_pred))
anova(fit, nonlinear_fit)

termplot(nonlinear_fit)

#restricted to early childbearers?
fit_w <- coxph(formula = Surv(age1b, status) ~ BMI_pred_cat, 
             data = df)
fit_wr <- coxph(formula = Surv(age1b, status) ~ BMI_pred_cat, 
             data = df %>% filter(age1b<23))

tab_model(fit_w, fit_wr)

fit_m <- coxph(formula = Surv(age1b, status) ~ BMI_pred_cat, 
             data = dm)
fit_mr <- coxph(formula = Surv(age1b, status) ~ BMI_pred_cat, 
                data = dm %>% filter(age1b<25))

tab_model(fit_m, fit_mr)

#piecewise
fit_phw2 <- pchreg(formula = Surv(age1b, status) ~ BMI_pred_cat, 
                   breaks = 2,
                   data = df)

fit_phm2 <- pchreg(formula = Surv(age1b, status) ~ BMI_pred_cat, 
                   breaks = 2,
                   data = dm)

