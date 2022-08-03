library(tidyverse)
library(lme4)

bmidf1_long <- readRDS('./Data/211209_bmidf_long.rds') %>%
  mutate(SAMPLE_ID_1979 = ifelse(SAMPLE_ID_1979 %in% c(1:8), "cross-sectional",
                                 ifelse(SAMPLE_ID_1979 %in% c(9:14), "supplemental", "military")))

bmidf1 <- readRDS('./Data/211213_bmidf.rds') %>%
  mutate(SAMPLE_ID_1979 = ifelse(SAMPLE_ID_1979 %in% c(1:8), "cross-sectional",
                                 ifelse(SAMPLE_ID_1979 %in% c(9:14), "supplemental", "military")))

bmidf1_long$num <- c(1:nrow(bmidf1_long))
rsq <- function(x, y) summary(lm(y~x))$r.squared

# men ---------------------------------------------------------------------

#prepare data to predict
r <- bmidf1_long %>%
  filter(sex == 1) %>%
  group_by(CASEID_1979) %>%
  summarize(find = length(unique(transition)) == 3)

test_m <- bmidf1_long %>%
  filter(CASEID_1979 %in% r$CASEID_1979[r$find] 
         & transition %in% "before")

m_men <- lmer(BMI ~ 
                age + 
                I(age^2) + 
                #transition +
                after_parenthood + 
                AGEATINT_1979 + 
                SAMPLE_ID_1979 +
                racenew + 
                residence + 
                religion + 
                mother_hgc + 
                father_hgc + 
                residence_withwhom + 
                (1|CASEID_1979), 
              data = bmidf1_long %>% filter(sex == 1 &
                                              !(num %in% test_m$num)))

test_m <- test_m %>%
  filter(CASEID_1979 %in% unique(m_men@frame[["CASEID_1979"]]))

#predict
bmi_pred_men <- predict(m_men, test_m)

#compare with observed
cor.test(as.numeric(bmi_pred_men), test_m$BMI)
rsq(as.numeric(bmi_pred_men), test_m$BMI)

plot(as.numeric(bmi_pred_men), test_m$BMI, 
     xlab = "Predicted BMI", 
     ylab = "Observed BMI", 
     main = 'Males (r-squared = 0.73)')
abline(lm(test_m$BMI ~ as.numeric(bmi_pred_men)), col = "blue")

# women -------------------------------------------------------------------

#prepare data to predict
r <- bmidf1_long %>%
  filter(sex == 2) %>%
  group_by(CASEID_1979) %>%
  summarize(find = length(unique(transition)) == 3)

test_f<- bmidf1_long %>%
  filter(CASEID_1979 %in% r$CASEID_1979[r$find] 
         & transition %in% "before")

m_women <- lmer(BMI ~ 
                age + 
                I(age^2) + 
                transition+ 
                AGEATINT_1979 + 
                SAMPLE_ID_1979 +
                racenew + 
                residence + 
                religion + 
                mother_hgc + 
                father_hgc + 
                residence_withwhom + 
                (1|CASEID_1979), 
              data = bmidf1_long %>% filter(sex == 2 &
                                              !(num %in% test_f$num)))

test_f <- test_f %>%
  filter(CASEID_1979 %in% unique(m_women@frame[["CASEID_1979"]]))

#predict
bmi_pred_women <- predict(m_women, test_f)

#compare with observed
cor.test(as.numeric(bmi_pred_women), test_f$BMI)
rsq(as.numeric(bmi_pred_women), test_f$BMI)


plot(as.numeric(bmi_pred_women), test_f$BMI, 
     xlab = "Predicted BMI", 
     ylab = "Observed BMI", 
     main = 'Females (r-squared = 0.74)')
abline(lm(test_f$BMI ~ as.numeric(bmi_pred_women)), col = "blue")
