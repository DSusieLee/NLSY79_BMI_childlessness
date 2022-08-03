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

# nonparametric: childless -----------------------------------------------------------
BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1))
BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2))

par(mfrow = c(1, 2))
plot(BMI1$BMI, predict(loess(BMI1$childless2~BMI1$BMI)), 
     main = 'Men', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))

plot(BMI2$BMI, predict(loess(BMI2$childless2~BMI2$BMI)), 
     main = 'Women', 
     xlim = c(15, 50),
     ylim = c(0.1, 0.7))

# nonparametric: fertility -----------------------------------------------------------
BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1))
BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2))

plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'Men',
     xlim = c(15, 60),
     ylim = c(0.3, 2.0), 
     xlab = 'BMI',
     ylab = 'Predicted fertility') 
abline(v = c(18.5, 25, 30), lwd=1, lty=2)

plot(BMI2$BMI, predict(loess(BMI2$NUMKID_XRND ~BMI2$BMI)), 
     main = 'Women',
     xlim = c(15, 60),
     ylim = c(0.3, 2.0),
     xlab = 'BMI',
     ylab = 'Predicted fertility')
abline(v = c(18.5, 25, 30), lwd=1, lty=2)

#further by race
par(mfrow = c(1, 3))

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1) & (Race %in% 1))
plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'Hispanic',
     xlim = c(15, 50),
     ylim = c(0.3, 2.0))
abline(v = c(18.5, 25, 30), lwd=1, lty=2)

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1) & (Race %in% 2))
plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'Black',
     xlim = c(15, 50),
     ylim = c(0.3, 2.0))
abline(v = c(18.5, 25, 30), lwd=1, lty=2)

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1) & (Race %in% 3))
plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'NHNB',
     xlim = c(15, 50),
     ylim = c(0.3, 2.0))
abline(v = c(18.5, 25, 30), lwd=1, lty=2)


par(mfrow = c(1, 3))

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2) & (Race %in% 1))
plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'Hispanic',
     xlim = c(15, 50),
     ylim = c(0.3, 2.0))
abline(v = c(18.5, 25, 30), lwd=1, lty=2)

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2) & (Race %in% 2))
plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'Black',
     xlim = c(15, 50),
     ylim = c(0.3, 2.0))
abline(v = c(18.5, 25, 30), lwd=1, lty=2)

BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2) & (Race %in% 3))
plot(BMI1$BMI, predict(loess(BMI1$NUMKID_XRND ~BMI1$BMI)), 
     main = 'NHNB',
     xlim = c(15, 50),
     ylim = c(0.3, 2.0))
abline(v = c(18.5, 25, 30), lwd=1, lty=2)


# semiparametric -------------------------------------------------------------------
# library(mgcv)
#  
# gam1 <- gam(I(childless2)
#             ~ s(BMI, bs = 'cs', k = -1) +
#               ageatmeasurement +
#               SAMPLE_RACE_78SCRN +
#               college,
#             data = subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1)), 
#             family=binomial, 
#             method = 'REML')
# 
# gam2 <- gam(I(childless2)
#             ~ s(BMI, bs = 'cs', k = -1) +
#               ageatmeasurement +
#               SAMPLE_RACE_78SCRN +
#               college,
#             data = subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2)), 
#             family=binomial, 
#             method = 'REML')
# 
# par(mfrow = c(1, 2))
# plot(gam1, 
#      main = 'Men', 
#      xlim = c(15, 50),
#      ylim = c(-1, 4))
# plot(gam2, 
#      main = 'Women', 
#      xlim = c(15, 50),
#      ylim = c(-1, 4))


# BMI1 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 1))
# BMI2 <- subset(dt, !is.na(desire_cat) & (SAMPLE_SEX_1979 %in% 2))
# 
# BMI1$predicted <- predict(gam1)
# BMI2$predicted <- predict(gam2)
# 
# dm <- rbind(BMI1, BMI2)
# 
# library(ggplot2)
# 
# theme_set(theme_bw())
# 
# ggplot(dm, aes(x = BMI, y = predicted))

# model: categories -------------------------------------------------------------------

m <- lm(childless2 ~ 
            BMIcat*SAMPLE_SEX_1979 +
            ageatmeasurement +
            Race +
            college,
          data = subset(dt, !is.na(desire_cat))) 

library(ggeffects)
dm <- ggpredict(m, terms = c('BMIcat [U, H, OV, O]', 'SAMPLE_SEX_1979'))

library(ggplot2)

theme_set(theme_bw())


# a <- ggplot(dm, aes(x = x, y = predicted, colour = x, linetype = group)) +
#   geom_point(aes(y = predicted), position = position_dodge(width = 0.3)) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.3)) +
#   scale_colour_manual(values = rev(c("#39568CFF", "#B8DE29FF", "#440154FF", "#1F968BFF")),
#                       name = 'Early BMI',
#                       labels = rev(c("Obese [O]", 'Overweight [Ov]', 'Normal weight [N]', 'Underweight [U]'))) +
#   scale_x_discrete(labels = c('Underweight','Normal weight','Overweight','Obese'))

b <- ggplot(dm, aes(x = x, y = predicted, colour = x)) +
  geom_point(aes(y = predicted), position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = rev(c("#39568CFF", "#B8DE29FF", "#440154FF", "#1F968BFF")),
                      name = 'Early BMI',
                      labels = rev(c("Obese [O]", 'Overweight [Ov]', 'Normal weight [N]', 'Underweight [U]'))) +
  scale_x_discrete(labels = c('Underweight','Normal weight','Overweight','Obese')) +
  facet_wrap( ~ group)

dm$group_lab <- ifelse(dm$group == 1, 'Men', 'Women')
dm$count <- c(63, 196, 2776, 2405, 870, 407, 183, 160)

b <- ggplot(dm, aes(x = x, y = predicted)) +
  geom_point(aes(y = predicted), position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.3)) +
  # scale_colour_manual(values = rev(c("#39568CFF", "#B8DE29FF", "#440154FF", "#1F968BFF")),
  #                     name = 'Early BMI',
  #                     labels = rev(c("Obese [O]", 'Overweight [Ov]', 'Normal weight [N]', 'Underweight [U]'))) +
  scale_x_discrete(labels = c('Underweight','Normal weight','Overweight','Obese')) +
  labs(y = 'Probability of childlessness',
       x = 'Early BMI') +
  scale_y_continuous(limits = c(0.14, 0.55)) +
  facet_wrap( ~ group_lab) +
  theme(
    axis.text.x = element_text(face = "plain",
                               size = 12,
                               angle = 20, hjust = 1, vjust = 1),
    axis.text.y = element_text(face = "plain", 
                               size = 12),
    axis.title.x =  element_text(face = "bold", size = 14),
    axis.title.y =  element_text(face = "bold", size = 14),
    strip.text = element_text(size = 14))

b + geom_text(data = dm, 
            aes(label = paste0("N = ", dm$count)),
            y = 0.15, colour="grey20", size = 5)
