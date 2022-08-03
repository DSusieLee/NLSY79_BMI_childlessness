library(tidyverse)
library(sjPlot)

dt <- readRDS('./Data/211201_8670.rds') %>%
  mutate(BMIcat = relevel(BMIcat, ref = "H"),
         racenew = relevel(as.factor(racenew), ref = "Whites"),
         BMIscaled = scale(BMI) %>% as.numeric()) %>%
  filter(EarlyParents == FALSE &
           !(racenew %in% "Other")) %>%
  mutate(racenew = droplevels(racenew))

dtm <- dt %>% filter(sex == 1)
dtf <- dt %>% filter(sex == 2)

#men
curdt <- dtm

m1 <- glm(childless ~ racenew, data = curdt)
m2 <- update(m1, .~. + BMIcat)
m3 <- update(m1, .~. + BMIcat*racenew)

AIC(m1,m2,m3)

curdt <- dtf

m1 <- glm(childless ~ racenew, data = curdt)
m2 <- update(m1, .~. + BMIcat)
m3 <- update(m1, .~. + BMIcat*racenew)

AIC(m1,m2,m3)

