library(survey)
library(srvyr)
library(forcats)

dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_finalset(210220)_7060.RDS")

#sources of information
#https://stats.stackexchange.com/questions/411026/why-it-is-important-to-make-survey-design-object-svydesign-function-in-r-with-i

dt$totfert <- dt$NUMKID_XRND
dt$totfert[dt$NUMKID_XRND >= 4] <- 4

dt$totfert <- as.factor(dt$totfert)

# specify a survey design -------------------------------------------------
dt$race <- fct_relevel(as.factor(dt$race), c('3','2','1'))


dtw <- svydesign(ids = ~1, 
                 data = dt, 
                 weights = dt$sampleWeights)

# dtw <- svydesign(ids = ~1, 
#                  data = dt,
#                  strata = ~race,
#                  weights = dt$SampleWeights,
#                  fpc = ~fpc)

#total fertility by sex
svyby(~NUMKID_XRND, ~sex, dtw, svymean, vartype = 'ci')
svyby(~NUMKID_XRND, ~sex, dtw, svymean)

svyttest(NUMKID_XRND ~ sex, dtw)

svymean(~interaction(totfert, sex), subset(dtw, sex %in% 1))
svymean(~interaction(totfert, sex), subset(dtw, sex %in% 2))

svyby(~totfert, ~BMIcat, subset(dtw, sex %in%1), svymean, vartype = 'ci')

#total fertility by race
svyby(~NUMKID_XRND, ~race + sex, dtw, svymean, vartype = 'ci')

#childlessness by sex
svymean(~childless, dtw)

svychisq(~childless + sex, dtw, statistic = 'Chisq')

#total fertility by race
svyby(~NUMKID_XRND, ~race + sex, dtw, svymean, vartype = 'ci')

#childlessness by race
svymean(~childless, dtw)
svyby(~childless, ~ sex, dtw, svyciprop)
svyby(~childless, ~race + sex, dtw, svyciprop)

#childless by ever-marrying, sex
svyby(~childless, ~evermarried + sex, dtw, svymean, vartype = 'ci')

#childless by ever-marrying, sex, race
x <- svyby(~childless, ~evermarried + sex + race, dtw, svymean, vartype = 'ci')
x$childlessTRUE

m <- svyglm(childless ~ evermarried + BMIcat*race + college + ageatmeasurement, data = dtw, design = subset(dtw, sex %in% 1), family = gaussian())

#ever marrying by BMI
x <- svyby(~evermarried, ~sex + BMIcat, dtw, svymean, vartype = 'ci')


