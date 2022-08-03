library(tidyverse)
library(survey)
library(srvyr)
library(forcats)
library(ggplot2)

dt <- readRDS('./Data/211201_8670.rds') %>%
  mutate(BMIcat = relevel(BMIcat, ref = "H"),
         racenew = relevel(as.factor(racenew), ref = "Whites"),
         BMIscaled = scale(BMI)) %>%
  filter(!is.na(EarlyParents) &
           !(racenew %in% "Other")) 

levels(droplevels(dt$racenew))

dt <- dt %>%
  filter(EarlyParents == FALSE)

dtw <- dt %>% 
  mutate(sampleweights = dt$sampleweights$V2) %>%
  as_survey(weights = sampleweights) %>%
  group_by(sex, racenew, BMIcat) %>%
  summarize(n = n(),
            childless_unwtd = sum(childless == TRUE)/n,
            childless_wtd = survey_mean(childless == TRUE)
            )

