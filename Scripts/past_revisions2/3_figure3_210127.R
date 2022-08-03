library(ggplot2)
library(ggpubr)
library(dplyr)
library(cowplot)

dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_finalset(210202).RDS")

women <- dt$SAMPLE_SEX_1979 == 2
desire <- dt$`FER-1B_1982`
bmicat <- dt$BMIcat

desire_cat <- desire
desire_cat[desire >= 3] <- 3

dt$desire_cat <- desire_cat

#filter_out <- !(dt$evermarried & (dt$AGE1M_XRND < 0))

theme_set(theme_bw())

#women
x <- data.frame(prop.table(table(desire_cat[women], bmicat[women]), 2))

x$bmicat_num <- as.factor(c(rep(3, 4), rep(1, 4), rep(2, 4), rep(4, 4)))
x$bmicat_num_rev <- as.factor(c(rep(2, 4), rep(4, 4), rep(3, 4), rep(1, 4)))
x$Var1_rev <- rev(x$Var1)

a <- ggplot(x, aes(x = bmicat_num, y = Freq, fill = bmicat_num, alpha = Var1_rev)) +
  geom_bar(position = 'fill', stat = 'identity', colour = 'white') +
  scale_fill_manual(values = c("grey28", "grey28", "grey28", "grey28"),
                    labels = c("Obese", 'Overweight', 'Normal weight', 'Underweight'),
                    name = 'BMI') +
  scale_alpha_manual(values = c(0.1, 0.4, 0.65, 1),
                     labels = rev(c('0','1','2','3+')),
                     name = 'Desired fertility',
                     guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("Obese", 'Overweight', 'Normal weight', 'Underweight')) +
  xlab('Early BMI') +
  ylab('Frequency') +
  theme(
    axis.text.x = element_text(face = "plain", size = 12),
    axis.text.y = element_text(face = "plain", size = 12),
    axis.title.x =  element_text(face = "bold", size = 12),
    axis.title.y =  element_text(face = "bold", size=12),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'grey28'),
    legend.position = 'top') +
  guides(color = guide_legend("BMI"), fill = FALSE) +
  coord_flip() +
  ggtitle('B. Women')


legend <- get_legend(
  # create some space to the top of the legend
  a + theme(legend.box.margin = margin(7, 0, 0, 0))
)

a <- ggplot(x, aes(x = bmicat_num, y = Freq, fill = bmicat_num, alpha = Var1_rev)) +
  geom_bar(position = 'fill', stat = 'identity', colour = 'white') +
  scale_fill_manual(values = c("grey28", "grey28", "grey28", "grey28"),
                    labels = c("Obese", 'Overweight', 'Normal weight', 'Underweight'),
                    name = 'BMI') +
  scale_alpha_manual(values = c(0.1, 0.4, 0.65, 1),
                     labels = rev(c('0','1','2','3+')),
                     name = 'Desired fertility',
                     guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("Obese", 'Overweight', 'Normal weight', 'Underweight')) +
  xlab('') +
  ylab('Freq of desired fertility') +  
  theme(
    axis.text.x = element_text(face = "plain", size = 12),
    axis.text.y = element_text(face = "plain", size = 12),
    #axis.title.x =  element_text(face = "bold", size = 12),
    axis.title.y =  element_text(face = "bold", size=12),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'grey28'),
    legend.position = 'top') +
  guides(color = guide_legend("BMI"), fill = FALSE) +
  geom_text(x = 1, y = 0.06, label = '*', size = 9) +
  geom_text(x = 1, y = 0.2, label = '*', size = 9) +
  geom_text(x = 1, y = 0.9, label = '*', size = 9) +
  geom_text(x = 2, y = 0.06, label = '-', size = 9) +
  #  geom_text(x = 4, y = 0.06, label = '-', size = 9) +
  coord_flip() +
  ggtitle('B. Women')


#men

x <- data.frame(prop.table(table(desire_cat[!women], bmicat[!women]), 2))
x$bmicat_num <- as.factor(c(rep(3, 4), rep(1, 4), rep(2, 4), rep(4, 4)))
x$bmicat_num_rev <- as.factor(c(rep(2, 4), rep(4, 4), rep(3, 4), rep(1, 4)))
x$Var1_rev <- rev(x$Var1)

b <- ggplot(x, aes(x = bmicat_num, y = Freq, fill = bmicat_num, alpha = Var1_rev)) +
  geom_bar(position = 'fill', stat = 'identity', colour = 'white') +
  scale_fill_manual(values = c("grey28", "grey28", "grey28", "grey28"),
                    labels = c("Obese", 'Overweight', 'Normal weight', 'Underweight'),
                    name = 'BMI') +
  scale_alpha_manual(values = c(0.1, 0.4, 0.65, 1),
                     labels = rev(c('0','1','2','3+')),
                     name = 'Desired fertility',
                     guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("Obese", 'Overweight', 'Normal weight', 'Underweight')) +
  xlab('') +
  ylab('Freq of desired fertility') +
  theme(
    axis.text.x = element_text(face = "plain", size = 12),
    axis.text.y = element_text(face = "plain", size = 12),
    #axis.title.x =  element_text(face = "bold", size = 12),
    axis.title.y =  element_text(face = "bold", size=12),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'grey28'),
    legend.position = 'top') +
  guides(color = guide_legend("BMI"), fill = FALSE) +
  geom_text(x = 4, y = 0.06, label = '*', size = 9) +
  coord_flip() +
  ggtitle('A. Men')

prow <- 
  plot_grid(b + theme(legend.position="none"), 
            a + theme(legend.position="none"), 
            nrow = 1)

x <- plot_grid(legend, prow, nrow = 2, rel_heights = c(.4, 3))