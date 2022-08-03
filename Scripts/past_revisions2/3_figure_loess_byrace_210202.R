dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(210202).RDS")

dt$college <- as.factor(dt$college)
dt$SAMPLE_RACE_78SCRN <- relevel(as.factor(dt$SAMPLE_RACE_78SCRN), ref = '3') #1=hispanic, 2=black

#prepare desire variable
desire <- dt$`FER-1B_1982`

library(dplyr)
desire_cat <- desire
desire_cat[desire >= 3] <- 3
dt$desire_cat <- as.factor(desire_cat)

#make desire category 2 as reference
dt$desire_cat <- relevel(dt$desire_cat, ref = '2')

dt$desire_cat <- as.factor(dt$desire_cat)

dt$childless2 <- as.numeric(dt$childless)
dt$evermarried2 <- as.factor(dt$evermarried)

dt$BMIcatnum <- rep(1, rep(nrow(dt)))
dt$BMIcatnum[dt$BMIcat %in% 'H'] <- 2
dt$BMIcatnum[dt$BMIcat %in% 'OV'] <- 3
dt$BMIcatnum[dt$BMIcat %in% 'O'] <- 4

# nonparametric -----------------------------------------------------------
BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1))
BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2))


BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1) & (SAMPLE_RACE_78SCRN %in% 1))
BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2) & (SAMPLE_RACE_78SCRN %in% 1))

par(mfrow = c(1, 2))
plot(BMI1$BMI, predict(loess(BMI1$childless2~BMI1$BMI)), 
     main = 'Men', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))

plot(BMI2$BMI, predict(loess(BMI2$childless2~BMI2$BMI)), 
     main = 'Women', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))


BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1) & (SAMPLE_RACE_78SCRN %in% 2))
BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2) & (SAMPLE_RACE_78SCRN %in% 2))

par(mfrow = c(1, 2))
plot(BMI1$BMI, predict(loess(BMI1$childless2~BMI1$BMI)), 
     main = 'Men', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))

plot(BMI2$BMI, predict(loess(BMI2$childless2~BMI2$BMI)), 
     main = 'Women', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1) & (SAMPLE_RACE_78SCRN %in% 3))
BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2) & (SAMPLE_RACE_78SCRN %in% 3))

par(mfrow = c(1, 2))
plot(BMI1$BMI, predict(loess(BMI1$childless2~BMI1$BMI)), 
     main = 'Men', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))

plot(BMI2$BMI, predict(loess(BMI2$childless2~BMI2$BMI)), 
     main = 'Women', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))
