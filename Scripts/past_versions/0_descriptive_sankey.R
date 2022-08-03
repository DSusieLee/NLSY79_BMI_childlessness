
sex <- dt$SAMPLE_SEX_1979
bmicat <- dt$BMI_cat
bBEFm <- !dt$FL1M1B_XRND

prop.table(table(bBEFm, sex, bmicat)[,,1], 2)
prop.table(table(bBEFm, sex, bmicat)[,,2], 2)
prop.table(table(bBEFm, sex, bmicat)[,,3], 2)

# females -----------------------------------------------------------------

dt_f <- subset(dt, dt$SAMPLE_SEX_1979 %in% 2 & !is.na(dt$BMI_cat))

dt_f$everpreg <- !dt_f$OUT1P_XRND %in% -998

library(plyr)
x <- count(dt_f, c('BMI_cat', 'evermarried', 'everpreg', 'childless'))

x$evermarried <- ifelse(x$evermarried, 'YES', 'NO')
x$everpreg <- ifelse(x$everpreg, 'YES', 'NO')
x$childless <- ifelse(x$childless, 'YES', 'NO')

library(ggalluvial)

ggplot(x, aes(axis1 = BMI_cat, 
              axis2 = evermarried,
              axis3 = everpreg, 
              axis4 = childless,
              y = freq)) +
  scale_x_discrete(limits = c("BMI Category", "Ever married", "Ever pregnant", "Childless"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = BMI_cat)) +
  geom_stratum() + 
  geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  theme(legend.position = 'bottom')

library(alluvial)

color = rep(NA, nrow(x))
color[x$BMI_cat %in% 'H'] = 'blue'
color[x$BMI_cat %in% 'O'] = 'red'
color[x$BMI_cat %in% 'U'] = 'yellow'

alluvial(x[,1:4], 
         freq = x$freq, 
         border = NA,
         col = color)


# males -------------------------------------------------------------------


