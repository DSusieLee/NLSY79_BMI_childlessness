library(dplyr)
library(tidyr)
library(reshape2)
library(huxtable)
library(flextable)
library(gt)
library(gtsummary)
library(modelsummary)
library(tidyverse)
library(forcats)

# test (not used): tally, count, percentage ------------------------------------------------
dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")

dt$totfert <- dt$NUMKID_XRND
dt$totfert[dt$NUMKID_XRND>=4] <- 4

x <- dt %>% 
  group_by(sex, race, totfert) %>% 
  tally() %>% 
  group_by(sex, race) %>% 
  mutate(freq = round(n/sum(n), digits = 2)) 

names(x) <- c('sex','race','totalfert','count','percentage')

#print_screen(as_hux(x), 'test.docx')

x1 <- x %>% dcast(totalfert ~ sex + race, value.var = "count")
x2 <- x %>% dcast(totalfert ~ sex + race, value.var = "percentage")
xx <- rbind(x1,x2)
xx$name <- c(rep('count',5), rep('percentage',5))
xx <- xx[order(xx$totalfert),]

gt_tbl <- gt(xx)


# Table 1 -----------------------------------------------------------------
dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")

names(dt)

dt$lifetimefert <- as.character(dt$NUMKID_XRND)
dt$lifetimefert[dt$NUMKID_XRND >= 4] <- '4+'
dt$AGE1M_XRND[dt$AGE1M_XRND == -999] <- NA

names(dt)[1] <- 'BMI category'
names(dt)[3] <- 'Fertility desire'
names(dt)[4] <- 'Race/Ethnicity'
names(dt)[5] <- 'Sex'
names(dt)[6] <- 'Childless'
names(dt)[8] <- 'Not entered college'
names(dt)[10] <- 'Age (yrs) at first marriage'
names(dt)[7] <- 'Ever married'
names(dt)[12] <- 'Lifetime fertility'
names(dt)[13] <- 'Completed fertility'

dt$`Fertility desire`[dt$`Fertility desire` >= 3] <- 3
dt$`Fertility desire` <- as.character(dt$`Fertility desire`)
dt$`Fertility desire`[dt$`Fertility desire` %in% '3']  <- '3+'

dt$`BMI category`[dt$`BMI category` %in% "H"] <- 'Normal'
dt$`BMI category`[dt$`BMI category` %in% "OV"] <- 'Overweight'
dt$`BMI category`[dt$`BMI category` %in% "O"] <- 'Obese'
dt$`BMI category`[dt$`BMI category` %in% "U"] <- 'Underweight'
dt$`BMI category` <- fct_relevel(dt$`BMI category`, c('Underweight','Normal','Overweight','Obese'))

dt$Sex <- ifelse(dt$Sex %in% 1, 'Men','Women')

dt$`Race/Ethnicity`[dt$`Race/Ethnicity` %in% 1] <- 'Hispanic'
dt$`Race/Ethnicity`[dt$`Race/Ethnicity` %in% 2] <- 'Black'
dt$`Race/Ethnicity`[dt$`Race/Ethnicity` %in% 3] <- 'Non-Hispanic Non-black'
dt$`Race/Ethnicity` <- fct_relevel(dt$`Race/Ethnicity`, c('Hispanic','Black','Non-Hispanic Non-black'))

#tbl_summary in the 'gtsummary' package
test <- dt %>% 
  arrange('BMI category') %>%
  select(
    #'Lifetime fertility',
    'Completed fertility',
    'BMI', 
    'BMI category', 
    'Race/Ethnicity', 
    'Ever married', 
    'Age (yrs) at first marriage', 
    'Not entered college', 
    'Fertility desire',
    'Sex') %>%
  tbl_summary(
    by = 'Sex',
    statistic = all_continuous() ~ "{median} ({p25}, {p75})") %>%
  modify_header(label ~ "") %>%
  #modify_spanning_header(c("stat_1","stat_2") ~ "**Sex**") %>%
  bold_labels()
     

gtt <- 
  as_gt(test) %>%
  tab_header(
    title = "Study sample from the NLSY79 cohort")

gtsave(gtt, 'Table1.png')
  