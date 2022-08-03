dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp.RDS")

lowfert <- ifelse(dt$NUMKID_XRND < 2, 1, 2) #whether to have it <=2 or <2?
lowfert[dt$childless] <- 0
dt$fert_cat <- lowfert

library(plyr)
x <- count(subset(dt, !is.na(BMI_cat)), c('SAMPLE_SEX_1979', 'BMI_cat', 'evermarried', 'fert_cat'))
x$cat <- rep(NA, nrow(x))

x$cat[x$evermarried & x$fert_cat == 0] <- 3 #married & high fert
x$cat[x$evermarried & x$fert_cat == 1] <- 2 #married & low fert
x$cat[x$evermarried & x$fert_cat == 2] <- 1 #married & childless
x$cat[!x$evermarried & x$fert_cat == 0] <- 4 #not married & childless
x$cat[!x$evermarried & x$fert_cat != 0] <- 5 #not married & have child

library(dplyr)

x_cat <- 
  x %>%
  group_by(cat, SAMPLE_SEX_1979) %>%
  summarise(freq = sum(freq))

x_catm <- subset(x_cat, SAMPLE_SEX_1979 %in% 1)
x_catf <- subset(x_cat, SAMPLE_SEX_1979 %in% 2)

#adding the category variable to dt

dt$cat[dt$evermarried & dt$fert_cat == 2] <- 1 #married & high fert
dt$cat[dt$evermarried & dt$fert_cat == 1] <- 2 #married & low fert
dt$cat[dt$evermarried & dt$fert_cat == 0] <- 3 #married & childless
dt$cat[!dt$evermarried & dt$fert_cat == 0] <- 4 #not married & childless
dt$cat[!dt$evermarried & dt$fert_cat != 0] <- 5 #not married & have child

# MNP ---------------------------------------------------------------------

library(MNP)
subdt <- subset(dt, !is.na(BMI_cat) & !is.na(cat))

mm <- mnp(cat ~ BMI_cat, data = subset(subdt, SAMPLE_SEX_1979 %in% 1), verbose = TRUE)
mf <- mnp(cat ~ BMI_cat, data = subset(subdt, SAMPLE_SEX_1979 %in% 2), verbose = TRUE)

mm <- mnp(cat ~ ROSENBERG_ESTEEM_SCORE_1980, data = subset(subdt, SAMPLE_SEX_1979 %in% 1), verbose = TRUE)
mf <- mnp(cat ~ ROSENBERG_ESTEEM_SCORE_1980, data = subset(subdt, SAMPLE_SEX_1979 %in% 2), verbose = TRUE)
