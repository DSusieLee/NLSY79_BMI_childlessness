dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(201110).RDS")

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


# model -------------------------------------------------------------------

m <- lm(childless2 ~ 
            BMIcat*SAMPLE_SEX_1979 +
            ageatmeasurement +
            SAMPLE_RACE_78SCRN +
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
dm$count <- c(41, 152, 1642, 1619, 516, 238, 118, 91)

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
    axis.title.x = element_text(face = "plain", 
                                size = 14),
    axis.title.y = element_text(face = "plain", 
                                size = 14),
    strip.text = element_text(size = 14))

b + geom_text(data = dm, 
            aes(label = paste0("N = ", dm$count)),
            y = 0.15, colour="grey20", size = 3)
