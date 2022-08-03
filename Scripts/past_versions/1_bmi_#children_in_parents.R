dtt <- subset(dt, 
              SAMPLE_ID_1979 <=8)

bmidt$cf <- rep(dtt$NUMKID_XRND, 3)
bmidt$childless <- bmidt$cf == 0
bmidt$logbmi_sq <- bmidt$logbmi^2


# bmi and # of children in parents ----------------------------------------

bmidt2 <- subset(bmidt, year == 1981 & !childless)
bmidt2$sex <- factor(bmidt2$sex)

library(ggplot2)

theme_set(
  theme_bw()
)


p <- ggplot(bmidt2, aes(x = bmi, y = cf)) + 
  geom_point(aes(shape = sex), position = position_dodge(0.9)) +
  scale_shape_manual(name = 'Sex', 
                     labels = c('Male', 'Female'),
                     values=c(3, 16)) +
  ylab('# of children ever born') +
  xlab('BMI - 1981')

p + facet_grid(rows = vars(sex))


# combining childless too -------------------------------------------------

library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

p1 <- ggplot(subset(bmidt, sex ==1 & cf >0 & year== 1981), aes(x = bmi, y = cf)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0.3), size = 0.3) + 
  theme_bw(base_size = 14) + 
  annotate("rect", xmin = 18.5, xmax = 24.9, ymin = 0.5, ymax = 11, alpha = .5) + 
  annotate("text", x = 22, y = 10, label = 'Healthy BMI') + 
  annotate("segment", x = 30, y = 7.5, xend = 35, yend = 7.5, arrow = arrow(length = unit(2, 'mm'))) +
  annotate("text", x = 32, y = 8, label = 'Obese (BMI = 30 or larger)') + 
  scale_x_continuous(breaks = c(20, 30, 40, 50), limits = c(15,50)) + 
  scale_y_continuous(breaks = c(1:9)) +
  xlab('') + 
  ylab('') + 
  ggtitle('Males')

p2 <- ggplot(subset(bmidt, sex ==2 & cf >0 & year== 1981), aes(x = bmi, y = cf)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0.3), size = 0.3) + 
  theme_bw(base_size = 14) + 
  annotate("rect", xmin = 18.5, xmax = 24.9, ymin = 0.5, ymax = 11, alpha = .5) + 
  annotate("text", x = 22, y = 10, label = 'Healthy BMI') + 
  annotate("segment", x = 30, y = 7.5, xend = 35, yend = 7.5, arrow = arrow(length = unit(2, 'mm'))) +
  annotate("text", x = 32, y = 8, label = 'Obese (BMI = 30 or larger)') + 
  scale_x_continuous(breaks = c(20, 30, 40, 50), limits = c(15,50)) + 
  scale_y_continuous(breaks = c(1:9)) +
  xlab('') + 
  ylab('') + 
  ggtitle('Females')

p <- plot_grid(p1, p2, nrow = 2, align = 'hv')

y.grob <- textGrob("Number of children ever born", 
                   gp=gpar(fontface="bold", col="blue", fontsize=15), rot=90)
x.grob <- textGrob("BMI in 1981", 
                   gp=gpar(fontface="bold", col="blue", fontsize=15))

grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob))
