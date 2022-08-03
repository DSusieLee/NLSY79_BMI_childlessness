
dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(201110).RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_finalset(201110).RDS")

dt$college <- as.factor(dt$college)
dt$SAMPLE_RACE_78SCRN <- relevel(as.factor(dt$SAMPLE_RACE_78SCRN), ref = '3') #1=hispanic, 2=black
dt$race <- as.factor(dt$SAMPLE_RACE_78SCRN)

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

dt$sex <- ifelse(dt$SAMPLE_SEX_1979 %in% 1, 'Men', 'Women')
dt$childless <- ifelse(dt$childless, 'Yes', 'No')

# summary -----------------------------------------------------------------
library(arsenal)
https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html
http://thatdatatho.com/2018/08/20/easily-create-descriptive-summary-statistic-tables-r-studio/
