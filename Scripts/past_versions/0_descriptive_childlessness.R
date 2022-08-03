dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")

#prepare desire variable
desire <- dt$`FER-1B_1982`

library(dplyr)
desire_cat <- desire
desire_cat[between(desire, 2, 3)] <- 2
desire_cat[desire > 3] <- 3
dt$desire_cat <- as.factor(desire_cat)

#make desire category 2 as reference
dt$desire_cat <- relevel(dt$desire_cat, ref = '2')

dt$desire_cat <- as.factor(dt$desire_cat)


# this is the final dataset -----------------------------------------------

x <- subset(dt, !is.na(BMIcat) & !is.na(desire_cat))

prop.table(table(x$childless, x$SAMPLE_SEX_1979), 2)
