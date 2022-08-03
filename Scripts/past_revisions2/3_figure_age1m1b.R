dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(201110).RDS")

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(dplyr)
library(cowplot)

sex <- dt$SAMPLE_SEX_1979
bmicat <- dt$BMIcat
childless <- dt$childless
evermarried <- dt$evermarried
women <- sex %in% 2

age1m <- dt$AGE1M_XRND #age at 1st marriage
age1b <- dt$AGE1B_XRND #age at 1st birt
age1m_filt <- age1m > 0 & !is.na(age1m) & !is.na(bmicat)
age1b_filt <- age1b > 0 & !is.na(age1b) & !is.na(bmicat)

desire <- dt$`FER-1B_1982`

desire_cat <- desire
desire_cat[desire >= 3] <- 3

dt$desire_cat <- desire_cat

#remove those who married but age1m not known
filter_out <- !(dt$evermarried & (dt$AGE1M_XRND < 0))