dt <- readRDS('./Data/211201_8670.rds') %>%
  mutate(BMIcat = relevel(BMIcat, ref = "H"),
         racenew = relevel(as.factor(racenew), ref = "Whites"),
         BMIscaled = scale(BMI)) %>%
  filter(!is.na(EarlyParents) &
           !(racenew %in% "Other")) %>%
  filter(EarlyParents == FALSE)

levels(droplevels(dt$racenew))


# models ------------------------------------------------------------------

#men

m1 <- lm(childless ~ BMIscaled + I(BMIscaled^2), data = dt %>% filter(sex==1))
m2 <- update(m1, .~. + racenew)
m3 <- update(m2, .~. + BMIscaled*racenew + I(BMIscaled^2)*racenew)
m4 <- update(m2, .~. + evermarried)
m5 <- update(m4, .~. + BMIscaled*evermarried + I(BMIscaled^2)*evermarried)

BIC(m1,m2,m3,m4,m5)

m1 <- lm(childless ~ BMIcat, data = dt %>% filter(sex==1))
m2 <- update(m1, .~. + racenew)
m3 <- update(m2, .~. + BMIcat*racenew)
m4 <- update(m2, .~. + evermarried)
m5 <- update(m4, .~. + BMIcat*evermarried)

BIC(m1,m2,m3,m4,m5)

#women

f1 <- lm(childless ~ BMIscaled + I(BMIscaled^2), data = dt %>% filter(sex==2))
f2 <- update(f1, .~. + racenew)
f3 <- update(f2, .~. + BMIscaled*racenew + I(BMIscaled^2)*racenew)
f4 <- update(f2, .~. + evermarried)
f5 <- update(f4, .~. + BMIscaled*evermarried + I(BMIscaled^2)*evermarried)

BIC(f1,f2,f3,f4,f5)

f1 <- lm(childless ~ BMIcat, data = dt %>% filter(sex==2))
f2 <- update(f1, .~. + racenew)
f3 <- update(f2, .~. + BMIcat*racenew)
f4 <- update(f2, .~. + evermarried)
f5 <- update(f4, .~. + BMIcat*evermarried)

BIC(f1,f2,f3,f4,f5)
