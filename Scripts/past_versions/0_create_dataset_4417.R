
dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")
dt$ageatmeasurement <- as.factor(dt$AGEATINT_1979+3)

dt1 <- dt[, c('BMIcat',
              'BMI',
              'FER-1B_1982', 
              'SAMPLE_RACE_78SCRN',
              'SAMPLE_SEX_1979',
              'childless',
              'evermarried',
              'college',
              'ageatmeasurement',
              'AGE1M_XRND',
              'AGE1B_XRND')]

dt1 <- subset(dt1, !is.na(dt1$`FER-1B_1982`))
saveRDS(dt1, 'NLSY79_finalset(201110).RDS')
