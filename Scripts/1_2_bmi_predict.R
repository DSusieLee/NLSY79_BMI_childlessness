library(tidyverse)

bmidf1_long <- readRDS('./Data/220118_bmidf_long.rds') %>%
  mutate(SAMPLE_ID_1979 = ifelse(SAMPLE_ID_1979 %in% c(1:8), "cross-sectional",
                                 ifelse(SAMPLE_ID_1979 %in% c(9:14), "supplemental", "military")))
bmidf1 <- readRDS('./Data/220118_bmidf.rds')

# predict BMI -------------------------------------------------------------

#1. models for prediction
library(lme4)
m_men <- lmer(BMI ~ 
                age + 
                I(age^2) + 
                transition +
                AGEATINT_1979 + 
                SAMPLE_ID_1979 +
                racenew + 
                residence + 
                religion + 
                mother_hgc + 
                father_hgc + 
                residence_withwhom + 
                (1|CASEID_1979), 
              data = bmidf1_long %>% filter(sex == 1))

m_women <- lmer(BMI ~ 
                  age + 
                  transition + 
                  AGEATINT_1979 + 
                  SAMPLE_ID_1979 +
                  racenew + 
                  residence + 
                  religion + 
                  mother_hgc + 
                  father_hgc + 
                  residence_withwhom +
                  (1|CASEID_1979), 
                data = bmidf1_long %>% filter(sex == 2))

#2. data to predict
bmidf1 <- readRDS('./Data/220118_bmidf.rds') %>%
  mutate(BMI = NA, #set conditions for prediction
         BMI_age = 16,
         transition = 'before',
         after_parenthood = FALSE)

data_men <- bmidf1 %>% 
  filter(sex == 1 &
           CASEID_1979 %in% unique(m_men@frame[["CASEID_1979"]])) %>%
  rename(age = BMI_age) %>%
  select(BMI, BMI_known_beforeparenthood, jokela_omitted, birthbefore_1981, 
         childless, evermarried, last_surveyage, CASEID_1979, AGE1B_XRND,
         age, transition, AGEATINT_1979, SAMPLE_ID_1979, MO1M1B_XRND, AGE1M_XRND, racenew, residence, religion, mother_hgc, father_hgc, residence_withwhom)

data_women <- bmidf1 %>% 
  filter(sex == 2 &
           CASEID_1979 %in% unique(m_women@frame[["CASEID_1979"]])) %>%
  rename(age = BMI_age) %>%
  select(BMI, BMI_known_beforeparenthood, jokela_omitted, birthbefore_1981, 
         childless, evermarried, last_surveyage, CASEID_1979, AGE1B_XRND,
         age, transition, AGEATINT_1979, SAMPLE_ID_1979, MO1M1B_XRND, AGE1M_XRND, racenew, residence, religion, mother_hgc, father_hgc, residence_withwhom)

#predict
bmi_pred_men <- predict(m_men, data_men)
bmi_pred_women <- predict(m_women, data_women)

dm <- data_men %>%
  mutate(BMI_pred = as.numeric(bmi_pred_men),
         BMI_pred_cat = cut(BMI_pred,
                            c(0, 18.5, 25, 29.9, 50), 
                            labels = c("U","H","OV","O")),
         BMI_pred_cat = relevel(BMI_pred_cat, ref = "H")) %>%
  filter(!(AGE1B_XRND %in% c(11:15)))

df <- data_women %>%
  mutate(BMI_pred = as.numeric(bmi_pred_women),
         BMI_pred_cat = cut(BMI_pred,
                            c(0, 18.5, 25, 29.9, 50), 
                            labels = c("U","H","OV","O")),
         BMI_pred_cat = relevel(BMI_pred_cat, ref = "H")) %>%
  filter(!(AGE1B_XRND %in% c(11:15)))

#why the sample size??
#race categories for 'other' are removed.
#note that AGE1B_XRND of negative values include lifetime childless inds, thus shouldn't be removed.

saveRDS(dm, './Data/220118_dm.rds')
saveRDS(df, './Data/220118_df.rds')
