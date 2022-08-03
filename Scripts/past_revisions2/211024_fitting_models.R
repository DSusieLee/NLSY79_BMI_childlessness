library(tidyverse)

dt <- readRDS('./Data/211014_7810.rds') %>%
  mutate(BMIcat = relevel(BMIcat, ref = "H"),
         racenew = relevel(as.factor(racenew), ref = "Whites"),
         BMIscaled = scale(BMI)) %>%
  filter(!is.na(EarlyParents) &
           !(racenew %in% "Other"))

# by sex ------------------------------------------------------------------

#men
m0 <- glm(childless ~ 
            1,
          data = dt %>% 
            filter(sex == 1 & !EarlyParents) %>%
            mutate(childlss = as.factor(childless))
)
m1 <- update(m0, .~. + racenew)
m2 <- update(m0, .~. + BMIscaled)
m3 <- update(m0, .~. + poly(BMIscaled, 2)) #favored
m4 <- update(m0, .~. + racenew + BMIscaled)
m5 <- update(m0, .~. + racenew + poly(BMIscaled, 2))
m6 <- update(m0, .~. + racenew*BMIscaled)
m7 <- update(m0, .~. + racenew*(poly(BMIscaled, 2)))

AIC(m0,m1,m2,m3,m4,m5,m6,m7)

#women
m0 <- glm(childless ~ 
            1,
          data = dt %>% 
            filter(sex == 2 & !EarlyParents) %>%
            mutate(childlss = as.factor(childless))
)
m1 <- update(m0, .~. + racenew)
m2 <- update(m0, .~. + BMIscaled)
m3 <- update(m0, .~. + poly(BMIscaled, 2)) 
m4 <- update(m0, .~. + racenew + BMIscaled) #favored
m5 <- update(m0, .~. + racenew + poly(BMIscaled, 2))
m6 <- update(m0, .~. + racenew*BMIscaled)
m7 <- update(m0, .~. + racenew*(poly(BMIscaled, 2)))

AIC(m0,m1,m2,m3,m4,m5,m6,m7)

# BMI ---------------------------------------------------------------------

#men
m0 <- lm(BMI ~ 1,
         data = dt %>% filter(sex == 1) %>% mutate(EarlyParents = as.factor(EarlyParents)))
m1 <- update(m0, .~. + racenew)
m2 <- update(m0, .~. + EarlyParents) 
m3 <- update(m1, .~. + EarlyParents) #Favored
m4 <- update(m3, .~. + racenew*EarlyParents)

AIC(m1,m2,m3,m4)

#women
m0 <- lm(BMI ~ 1,
         data = dt %>% filter(sex == 2) %>% mutate(EarlyParents = as.factor(EarlyParents)))
m1 <- update(m0, .~. + racenew)
m2 <- update(m0, .~. + EarlyParents)
m3 <- update(m1, .~. + EarlyParents)
m4 <- update(m3, .~. + racenew*EarlyParents) #Favored

AIC(m1,m2,m3,m4)
plot_model(m4, type = "int")
