dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp.RDS")

l <- nrow(dt)

bmidt <- data.frame(id = rep(dt$CASEID_1979, 3),
                    sex = rep(dt$SAMPLE_SEX_1979, 3),
                    age = c(dt$AGEATINT_1979+2, 
                            dt$AGEATINT_1979+3,
                            dt$AGEATINT_1979+6),
                    bmi = c(dt$BMI1981, dt$BMI1982, dt$BMI1985),
                    year = c(rep(1981, l), rep(1982, l), rep(1985, l)),
                    college = rep((dt$HGCREV10_2010 >= 16), 3),
                    evermarried = rep(dt$evermarried, 3),
                    lastage = rep(dt$last_surveyage, 3),
                    cf = rep(rep(dt$NUMKID_XRND, 3)))

bmidt$logbmi <- log(bmidt$bmi)
bmidt$logbmi_sq <- bmidt$logbmi^2
bmidt$childless <- bmidt$cf == 0

alreadyparents1981 <- dt$NUMCH81_1981
bmidt$alreadyparents1981 <- rep(alreadyparents1981, 3) #1000 out of 6111 were already parents in 1981
alreadyparents1982 <- dt$NUMCH82_1982
bmidt$alreadyparents1982 <- rep(alreadyparents1982, 3)
alreadyparents1985 <- dt$NUMCH85_1985
bmidt$alreadyparents1985 <- rep(alreadyparents1985, 3)

saveRDS(bmidt, 'NLSY79_bmidt_long.RDS')