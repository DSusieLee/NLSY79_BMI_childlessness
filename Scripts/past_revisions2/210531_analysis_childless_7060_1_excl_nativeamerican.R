dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")

library(dplyr)
library(lmtest)
library(sjPlot)


dt$college <- as.factor(dt$college)
dt$race <- relevel(as.factor(dt$race), ref = '3') #1=hispanic, 2=black
dt$ageatmeasurement <- as.factor(dt$ageatmeasurement)

#prepare desire variable
desire <- dt$`FER-1B_1982`

desire_cat <- desire
desire_cat[between(desire, 2, 3)] <- 2
desire_cat[desire > 3] <- 3
dt$desire_cat <- as.factor(desire_cat)

#make desire category 2 as reference
dt$desire_cat <- relevel(dt$desire_cat, ref = '2')

dt$desire_cat <- as.factor(dt$desire_cat)

# pooled ------------------------------------------------------------------

m0 <- lm(childless ~
           sex + 
           race +
           ageatmeasurement +
           college,
         data = dt) 

m1 <- lm(childless ~
           sex*race +
           ageatmeasurement +
           college,
         data = dt) 

waldtest(m0, m1)
anova(m0, m1)

#CONCLUDE: no difference by race in the sex difference in childlessness.

# men ----------------------------------------------------------

m0 <- lm(childless ~ 
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m1 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m2 <- lm(childless ~ 
           BMIcat +
           race +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m3 <- lm(childless ~ 
           BMIcat +
           race +
           evermarried +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m4 <- lm(childless ~ 
           BMIcat*race +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m5 <- lm(childless ~ 
           BMIcat*evermarried +
           race +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m6 <- lm(childless ~ 
           BMIcat*evermarried +
           race +
           desire_cat +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 


m7 <- lm(childless ~ 
           BMIcat +
           race +
           evermarried +
           desire_cat +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 

m8 <- lm(childless ~ 
           BMIcat*desire_cat +
           race +
           evermarried +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 1 )) 


# waldtest(m0, m1, m2, m3)
# waldtest(m2, m4)
# waldtest(m3, m5)
# 
# anova(m0, m1, m2, m3)
# anova(m2, m4)
# anova(m3, m5)
# anova(m7, m8)


m9 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college +
           race,
         data = subset(dt, sex %in% 1  & evermarried & (AGE1M_XRND>0)))

m10 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college +
           race +
           AGE1M_XRND,
         data = subset(dt, sex %in% 1  & evermarried & (AGE1M_XRND>0)))

m11 <- lm(childless ~ 
            BMIcat +
            ageatmeasurement +
            college +
            race +
            AGE1M_XRND +
            desire_cat,
          data = subset(dt, sex %in% 1  & evermarried & (AGE1M_XRND>0)))

#tab models wo desire
tab_model(m1, m2, m4, m3, m5, m9, m10,
          rm.terms = c('ageatmeasurement [18]',
                       'ageatmeasurement [19]',
                       'ageatmeasurement [20]',
                       'ageatmeasurement [21]',
                       'ageatmeasurement [22]',
                       'ageatmeasurement [23]',
                       'ageatmeasurement [24]',
                       'ageatmeasurement [25]'),
          pred.labels = c('Intercept', 
                          'Obese [O]',
                          'Overweight [Ov]',
                          'Underweight [U]',
                          'Not entered college',
                          'Hispanic', 
                          'Black',
                          'O & Hispanic',
                          'Ov & Hispanic',
                          'U & Hispanic',
                          'O & Black',
                          'Ov & Black',
                          'U & Black',
                          'Ever-married',
                          'O & Ever-married', 
                          'Ov & Ever-married', 
                          'U & Ever-married', 
                          # 'Fertility desire: 0',
                          # 'Fertility desire: 1',
                          # 'Fertility desire: 3+',
                          'Age at 1st marriage'),
          #order.terms = c(1, 2, 3, 4, 21, 5:20),
          title = 'Men',
          show.p = FALSE,
          show.ci50 = TRUE,
          p.style = 'stars', 
          linebreak = T, 
          collapse.ci = T,
          file = '210301_lpm_wodesire_men.doc')


# women -------------------------------------------------------------------

f0 <- lm(childless ~ 
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f1 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f2 <- lm(childless ~ 
           BMIcat +
           race +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f3 <- lm(childless ~ 
           BMIcat +
           race +
           evermarried +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f4 <- lm(childless ~ 
           BMIcat*race +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 


f5 <- lm(childless ~ 
           BMIcat*evermarried +
           race +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f6 <- lm(childless ~ 
           BMIcat*evermarried +
           race +
           desire_cat +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f7 <- lm(childless ~ 
           BMIcat +
           race +
           evermarried +
           desire_cat +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

f8 <- lm(childless ~ 
           BMIcat*desire_cat +
           race +
           evermarried +
           ageatmeasurement +
           college,
         data = subset(dt, sex %in% 2 )) 

# waldtest(f0, f1, f2, f3)
# waldtest(f2, f4)
# waldtest(f3, f5)
# 
# 
# anova(f0, f1, f2, f3)
# anova(f2, f4)
# anova(f3, f5)
# anova(f5, f6)
# anova(f5, f6)
# anova(f7, f8)


f9 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college +
           race,
         data = subset(dt, sex %in% 2  & evermarried & (AGE1M_XRND>0)))

f10 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college +
           race +
           AGE1M_XRND,
         data = subset(dt, sex %in% 2  & evermarried & (AGE1M_XRND>0)))

f11 <- lm(childless ~ 
           BMIcat +
           ageatmeasurement +
           college +
           race +
           AGE1M_XRND +
           desire_cat,
         data = subset(dt, sex %in% 2  & evermarried & (AGE1M_XRND>0)))


tab_model(f1, f2, f4, f3, f5, f9, f10,
          rm.terms = c('ageatmeasurement [18]',
                       'ageatmeasurement [19]',
                       'ageatmeasurement [20]',
                       'ageatmeasurement [21]',
                       'ageatmeasurement [22]',
                       'ageatmeasurement [23]',
                       'ageatmeasurement [24]',
                       'ageatmeasurement [25]'),
          pred.labels = c('Intercept', 
                          'Obese [O]',
                          'Overweight [Ov]',
                          'Underweight [U]',
                          'Not entered college',
                          'Hispanic', 
                          'Black',
                          'O & Hispanic',
                          'Ov & Hispanic',
                          'U & Hispanic',
                          'O & Black',
                          'Ov & Black',
                          'U & Black',
                          'Ever-married',
                          'O & Ever-married', 
                          'Ov & Ever-married', 
                          'U & Ever-married', 
                          # 'Fertility desire: 0',
                          # 'Fertility desire: 1',
                          # 'Fertility desire: 3+',
                          'Age at 1st marriage'),
          #order.terms = c(1, 2, 3, 4, 21, 5:20),
          title = 'Women',
          show.p = FALSE,
          show.ci50 = TRUE,
          p.style = 'stars', 
          linebreak = T, 
          collapse.ci = T,
          file = '210301_lpm_wodesire_women.doc')

tab_model(f1, f2, f3, f5, f6, f8, f9, f10, f11,
          rm.terms = c('ageatmeasurement [18]',
                       'ageatmeasurement [19]',
                       'ageatmeasurement [20]',
                       'ageatmeasurement [21]',
                       'ageatmeasurement [22]',
                       'ageatmeasurement [23]',
                       'ageatmeasurement [24]',
                       'ageatmeasurement [25]'),
          pred.labels = c('Intercept', 
                          'Obese [O]',
                          'Overweight [Ov]',
                          'Underweight [U]',
                          'Not entered college',
                          'Hispanic', 
                          'Black',
                          'O & Hispanic',
                          'Ov & Hispanic',
                          'U & Hispanic',
                          'O & Black',
                          'Ov & Black',
                          'U & Black',
                          'Ever-married',
                          'O & Ever-married', 
                          'Ov & Ever-married', 
                          'U & Ever-married', 
                          'Fertility desire: 0',
                          'Fertility desire: 1',
                          'Fertility desire: 3+',
                          'Age at 1st marriage'),
          #order.terms = c(1, 2, 3, 4, 21, 5:20),
          title = 'Women',
          show.p = FALSE,
          show.ci50 = TRUE,
          p.style = 'stars', 
          linebreak = T, 
          collapse.ci = T,
          file = '210301_lpm_desire_women.doc')
