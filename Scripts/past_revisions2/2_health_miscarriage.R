dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")

library(stringr)

var_list <- names(dt)

miscar <- var_list[!is.na(str_extract(var_list, 'MISCAR'))]

miscar_df <- dt[,miscar[c(1:12, 14:19)]]

miscarried <- apply(miscar_df, 1, function(x) max(x, na.rm = T))
miscarried[is.infinite(miscarried)] <- NA
evermiscarried <- miscarried != 0

dt$evermiscarried <- evermiscarried

x <- table(evermiscarried, dt$BMIcat, dt$evermarried)[,,1]
y <- table(evermiscarried, dt$BMIcat, dt$evermarried)[,,2]

prop.table(x, 2)
prop.table(y, 2)

# models ------------------------------------------------------------------
dt$evermiscarried <- as.factor(dt$evermiscarried)

m1 <- glm(evermiscarried ~ 
            BMIcat +
            evermarried +
            college +
            as.factor(SAMPLE_RACE_78SCRN) +
            desire_cat,
          data = subset(dt, SAMPLE_SEX_1979 == 2),
          family = binomial
)


m2 <- lm(childless ~ 
           BMIcat + 
           college +
           as.factor(SAMPLE_RACE_78SCRN) +
           `FER-1B_1982` +
           evermiscarried, #miscarried is negative?!
         data = subset(dt, SAMPLE_SEX_1979 == 2 & evermarried & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40)
)

library(dplyr)
out1p <- dt$OUT1P_XRND
out1p_filt <- between(out1p, 1, 4) & !is.na(out1p) 

#only restricted to those who ever got pregnant
m2_1 <- lm(childless ~ 
            BMIcat + 
            college +
            as.factor(SAMPLE_RACE_78SCRN) +
           `FER-1B_1982` +
            evermiscarried +
            AGE1P_XRND, #now miscarried is positive
         data = subset(dt, SAMPLE_SEX_1979 == 2 & evermarried & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40 & out1p_filt)
)

# m3 <- lm(childless ~ 
#            BMIcat +
#            college +
#            as.factor(SAMPLE_RACE_78SCRN) +
#            `FER-1B_1982` +
#            !(OUT1P_XRND %in% 1) + AGE1P_XRND,
#          data = subset(dt, SAMPLE_SEX_1979 == 2 & evermarried & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40 & out1p_filt)
# )
# 
# m4 <- lm(childless ~ 
#            BMIcat +
#            college +
#            as.factor(SAMPLE_RACE_78SCRN) +
#            `FER-1B_1982` +
#            evermiscarried +
#            as.factor(OUT1P_XRND) + AGE1P_XRND,
#          data = subset(dt, SAMPLE_SEX_1979 == 2 & evermarried & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40 & out1p_filt)
# )


# miscarried interact with BMI ; never pregnant -------------------------------------------------------------
dt$neverpregnant <- !between(out1p, 1, 4)

m4 <- lm(childless ~ 
           BMIcat*evermiscarried +
           college +
           as.factor(SAMPLE_RACE_78SCRN) +
           `FER-1B_1982` +
           AGE1P_XRND,
         data = subset(dt, SAMPLE_SEX_1979 == 2 & 
                         evermarried & 
                         !(is.na(childless)) & 
                         !is.na(BMI) & 
                         dt$last_surveyage >= 40 & 
                         !neverpregnant)
)

m4_1 <- lm(childless ~ 
           BMIcat*evermiscarried +
           college +
           as.factor(SAMPLE_RACE_78SCRN) +
           `FER-1B_1982` +
           AGE1P_XRND,
         data = subset(dt, SAMPLE_SEX_1979 == 2 & !evermarried & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40 & out1p_filt)
)

mnp <- lm(neverpregnant ~ #note sample size is 341
            BMIcat +
            college +
            as.factor(SAMPLE_RACE_78SCRN) +
            `FER-1B_1982` +
            AGE1M_XRND,
          data = subset(dt, SAMPLE_SEX_1979 == 2 & evermarried)
)

library(sjPlot)

tab_model(m4, 
          order.terms = c(1, 2, 3, 4, 5, 11, 12, 13, 6, 7, 8, 9, 10),
          pred.labels = c('Intercept', 
                          'Obese & Never-miscarried',
                          'Overweight & Never-miscarried',
                          'Underweight & Never-miscarried',
                          'Healthy & Ever-miscarried',
                          'College',
                          'Black', 
                          'Hispanic', 
                          'Fert Desr: 1982',
                          'Age at 1st pregnancy', 
                          'Obese & Ever-miscarried', 
                          'Overweight & Ever-miscarried', 
                          'Underweight & Ever-miscarried'),
          dv.labels = c('Childlessness: Ever-married & Ever-pregnant'))

# stacked plot ------------------------------------------------------------

# percentage stacked barplot ----------------------------------------------

library(ggplot2)
library(ggpubr)

dt_female <- subset(dt, SAMPLE_SEX_1979 == 2 & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40)
#dt_female <- subset(dt, SAMPLE_SEX_1979 == 2 & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40 & !neverpregnant)

x <- data.frame(prop.table(table(dt_female$evermiscarried, dt_female$BMIcat, dt_female$evermarried)[,,1],2))
y <- data.frame(prop.table(table(dt_female$evermiscarried, dt_female$BMIcat, dt_female$evermarried)[,,2],2))

theme_set(theme_bw())

a <- ggplot(x, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(position = 'fill', stat = 'identity', colour = 'black') +
  scale_fill_manual(values = c('white', 'black'), labels = c('never-miscarried','ever-miscarried')) +
  xlab('BMI category') +
  ggtitle('Never-married')

b <- ggplot(y, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(position = 'fill', stat = 'identity', colour = 'black') +
  scale_fill_manual(values = c('white', 'black'), labels = c('never-miscarried','ever-miscarried')) +
  xlab('BMI category') +
  ggtitle('Ever-miscarried: Ever-married & Ever-pregnant')

ggarrange(a, b,
          common.legend = T,
          legend = "right",
          align = 'h')

#colour version

a <-  ggplot(subset(x, evermiscarried %in% "TRUE")) +
  geom_col(aes(x = Var2, y = Freq, fill = Var2)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                    labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                    name = "BMI category") 

b <-  ggplot(subset(y, evermiscarried %in% "TRUE")) +
  geom_col(aes(x = Var2, y = Freq, fill = Var2)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                    labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                    name = "BMI category") 

ggarrange(a, b, 
          common.legend = T, 
          legend = "right",
          align = 'h')


# mosaic plot (not used) -----------------------------------------------------------

packages = c('vcd', 'vcdExtra', 'tidyverse')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

subs_pal <- colorspace::qualitative_hcl(7)
mosaicplot(table(evermiscarried, dt_female$BMI_cat), 
           col = subs_pal,
           legend = TRUE
           )

dt_female$evermiscarried <- as.factor(dt_female$evermiscarried)

mm <- glm(evermiscarried ~ 
            BMI_cat*childless +
            college +
            as.factor(SAMPLE_RACE_78SCRN),
    data = dt_female,
    family = binomial
    )

p <- plot_model(mm,       
           type = "pred", 
           colors = 'bw', 
           title = '')

tab_model(mm, 
          pred.labels = c("Intercept", 
                          "Overweight/Obese", 
                          "Underweight", 
                          "Evermarried",
                          "College",
                          "Black",
                          "Hispanic"),
          dv.labels = c("Ever miscarried across adulthood"),
          string.ci = "CI (95%)",
          transform = NULL, 
          string.p = "P-Value")




mm_female <- glm(childless ~ 
                   log(BMI) + 
                   I(log(BMI)^2) + 
                   evermiscarried + 
                   college +
                   as.factor(SAMPLE_RACE_78SCRN),
                 data = dt_female,
                 family = binomial
)


tab_model(mm_female, 
          pred.labels = c("Intercept", 
                          "Log(BMI)", 
                          "Log(BMI)^2", 
                          "Ever miscarried",
                          "College",
                          "Black",
                          "Hispanic"),
          dv.labels = c("Childlessness: female"),
          string.ci = "CI (95%)",
          transform = NULL, 
          string.p = "P-Value")


# women's clinic ----------------------------------------------------------

miscar <- var_list[!is.na(str_extract(var_list, 'GENHLTH_5B')) | !is.na(str_extract(var_list, 'GENHLTH_4E_F~000001'))]

miscar_df <- dt_female[,miscar]

miscarried <- apply(miscar_df, 1, function(x) sum(x, na.rm = T))
miscarried[is.infinite(miscarried)] <- NA
everbeento <- miscarried != 0

dt_female$beento <- everbeento
dt_female$beento_howmany <- miscarried

mw <- lm(beento_howmany ~ 
     BMI_cat +
     college +
     evermarried,
   data = dt_female)

mw <- glm(beento ~ 
           BMI_cat +
           college,
         data = dt_female,
         family = binomial)

