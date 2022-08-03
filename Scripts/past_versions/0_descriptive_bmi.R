

# correlation betweeen BMI and age at measurement? ------------------------
dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")
ageatmeasurement <- dt$AGEATINT_1979+3

plot(ageatmeasurement, dt$BMI1982)

plot(ageatmeasurement, dt$last_surveyage)

prop.table(table(ageatmeasurement, dt$childless),1)

# correlation of BMI across years -----------------------------------------

library(PerformanceAnalytics)
bmidt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmidt_long.RDS")

temp <- data.frame(BMI1981 = bmidt$bmi[bmidt$year == 1981],
                   BMI1982 = bmidt$bmi[bmidt$year == 1982],
                   BMI1985 = bmidt$bmi[bmidt$year == 1985])

femaledt <- data.frame(BMI1981 = bmidt$bmi[bmidt$year == 1981 & bmidt$sex == 2],
                       BMI1982 = bmidt$bmi[bmidt$year == 1982 & bmidt$sex == 2],
                       BMI1985 = bmidt$bmi[bmidt$year == 1985 & bmidt$sex == 2])

maledt <- data.frame(BMI1981 = bmidt$bmi[bmidt$year == 1981 & bmidt$sex == 1],
                       BMI1982 = bmidt$bmi[bmidt$year == 1982 & bmidt$sex == 1],
                       BMI1985 = bmidt$bmi[bmidt$year == 1985 & bmidt$sex == 1])

chart.Correlation(femaledt)
chart.Correlation(maledt)

chart.Correlation(temp)