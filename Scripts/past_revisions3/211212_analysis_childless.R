library(tidyverse)
library(sjPlot)

dm <- readRDS('./Data/211214_dm.rds') %>%
  filter(last_surveyage >=40)
df <- readRDS('./Data/211214_df.rds') %>%
  filter(last_surveyage >=40)

dm$racenew <- relevel(as.factor(dm$racenew), ref = "Hispanics")
df$racenew <- relevel(as.factor(df$racenew), ref = "Hispanics")

dm$childless <- as.numeric(dm$childless)
df$childless <- as.numeric(df$childless)

dm$evermarried <- as.factor(dm$evermarried)
df$evermarried <- as.factor(df$evermarried)

#for visualization
dm$BMI_pred_cat <- factor(dm$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))
df$BMI_pred_cat <- factor(df$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))

m1 <- glm(childless ~ BMI_pred_cat, 
         data = dm,
         family = binomial)

m2 <- update(m1, .~. 
             + racenew*BMI_pred_cat)

m3 <- update(m1, .~. 
             + evermarried)

m4 <- update(m3, .~. 
             + evermarried*BMI_pred_cat)

AIC(m1,m2,m3,m4)

m11 <- glm(childless ~ BMI_pred_cat, 
          data = dm %>% filter(BMI_known_beforeparenthood))

m21 <- update(m11, .~. 
             + racenew*BMI_pred_cat)

m31 <- update(m11, .~. 
             + evermarried)

m41 <- update(m31, .~. 
             + evermarried*BMI_pred_cat)

f1 <- glm(childless ~ BMI_pred_cat, 
          data = df,
          family = binomial)

f2 <- update(f1, .~. 
             + racenew*BMI_pred_cat)

f3 <- glm(evermarried ~ BMI_pred_cat, 
          data = df,
          family = binomial)

f4 <- update(f3, .~. 
             + racenew*BMI_pred_cat)

AIC(f1,f2)
AIC(f3,f4)

f11 <- glm(childless ~ BMI_pred_cat, 
           data = df %>% filter(BMI_known_beforeparenthood),
           family = binomial)

f21 <- update(f11, .~. 
              + racenew*BMI_pred_cat)

f31 <- update(f11, .~. 
             + evermarried)

f41 <- update(f31, .~. 
             + evermarried*BMI_pred_cat)

# plot --------------------------------------------------------------------

p1 <- plot_model(f2, type = "int") 
p2 <- plot_model(f21, type = "int")