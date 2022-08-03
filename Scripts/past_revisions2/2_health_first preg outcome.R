dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")

library(dplyr)

bmicat <- dt$BMI_cat
bmicat <- dt$BMIcat
age1p <- dt$AGE1P_XRND
out1p <- dt$OUT1P_XRND
out1p_filt <- between(out1p, 1, 4) & !is.na(out1p) 
out1p[out1p < 0] <- 6

table(out1p[out1p_filt])

prop.table(table(out1p[out1p_filt], bmicat[out1p_filt]), 2)

# fitting multinomial probit model ----------------------------------------
library(MNP)
subdt <- subset(dt, out1p_filt)

mnp.out <- mnp(OUT1P_XRND ~ BMIcat + AGE1P_XRND, data = subset(dt, evermarried & out1p_filt),verbose = TRUE)

m <- lm(childless ~ OUT1P_XRND + BMI_cat + AGE1P_XRND, data = subdt)

# wanted to become pregnant before 1st birth ------------------------------
library(stringr)

dt_female <- dt[dt$SAMPLE_SEX_1979 == 2,]
var_list <- names(dt_female)

var <- var_list[!is.na(str_extract(var_list, 'Q9-81_1'))]
#there are only few individuals who ever answered to this question,
#and even among them, answers often differ within individuals

wanted1p_df <- dt_female[, var[c(1:8)]]

wanted <- apply(wanted1p_df, 1, function(x) min(x, na.rm = T))
wanted[is.infinite(wanted)] <- NA
wanted[wanted == 4] <- 3 
#wanted[wanted == 2] <- 1

tb_1v3 <- table(dt_female$BMI_cat[wanted != 2], wanted[wanted != 2])

prop.table(tb_1v3, 1)

subdt <- dt_female[!is.na(wanted),]
subdt$wanted <- wanted[!is.na(wanted)]
mnp.out <- mnp(wanted ~ BMI_cat + AGE1P_XRND, data = subdt, verbose = TRUE)
