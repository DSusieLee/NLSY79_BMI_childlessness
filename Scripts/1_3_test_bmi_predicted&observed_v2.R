library(tidyverse)
library(lme4)
library(hrbrthemes)

dm <- readRDS('./Data/220118_dm.rds')
df <- readRDS('./Data/220118_df.rds')

bmidf1_long <- readRDS('./Data/211209_bmidf_long.rds') %>%
  mutate(SAMPLE_ID_1979 = ifelse(SAMPLE_ID_1979 %in% c(1:8), "cross-sectional",
                                 ifelse(SAMPLE_ID_1979 %in% c(9:14), "supplemental", "military")))

bmidf1 <- readRDS('./Data/211213_bmidf.rds') %>%
  mutate(SAMPLE_ID_1979 = ifelse(SAMPLE_ID_1979 %in% c(1:8), "cross-sectional",
                                 ifelse(SAMPLE_ID_1979 %in% c(9:14), "supplemental", "military")))

bmidf1_long$num <- c(1:nrow(bmidf1_long))
rsq <- function(x, y) summary(lm(y~x))$r.squared


# predicted vs.observed ---------------------------------------------------

dt <- bmidf1_long %>% 
  filter(age==16 & !is.na(BMI) & transition %in% "before") %>%
  select(CASEID_1979, BMI, sex)

dt <- left_join(dt, 
                rbind(dm %>% select(CASEID_1979, BMI_pred),
                      df %>% select(CASEID_1979, BMI_pred)),
                by = "CASEID_1979") %>%
  mutate(sex = ifelse(sex == 1, "Men", "Women"))

ggplot(dt, aes(x = BMI_pred, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Predicted BMI",
       y = "Observed BMI") +
  facet_grid(~sex) +
  theme_ipsum()

ggsave('./Figures/220320_BMI_predicted_observed.png', type = "cairo")

#correlation test
cor.test(dt$BMI[dt$sex %in% "Men"], dt$BMI_pred[dt$sex %in% "Men"])
cor.test(dt$BMI[!dt$sex %in% "Men"], dt$BMI_pred[!dt$sex %in% "Men"])
