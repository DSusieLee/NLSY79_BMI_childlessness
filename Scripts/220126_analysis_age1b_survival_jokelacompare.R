library(tidyverse)
library(survival)
library(survminer)
library(hrbrthemes) 
library(cowplot)

dm <- readRDS('./Data/220118_dm.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         #age1b = age1b-16,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('U','H', 'OV', 'O')))

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         #age1b = age1b-16,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('U','H', 'OV', 'O')))

colors = c("grey82","darkturquoise","grey49","grey5")

# models ------------------------------------------------------------------

#RELEVEL BMI CATEGORIES BEFORE RUNNING THE MODELS
library(sjPlot)

#females
m <- coxph(Surv(age1b, status) ~ BMI_pred_cat, 
           data = df)
m1 <- coxph(Surv(age1b, status) ~ BMI_pred_cat, 
            data = df %>% filter(!jokela_omitted & !birthbefore_1981))
tab_model(m,
          m1, 
          title = "Females: Chance of having first child, relative to healthy BMI", 
          dv.labels = c("Sample including early childbearers", "Sample excluding early childbearers"), 
          pred.labels  = c("Underweight","Overweight","Obese"),
          file = './Results/220125_jokelacompare_women.doc')

#males
m <- coxph(Surv(age1b, status) ~ BMI_pred_cat, 
           data = dm)
m1 <- coxph(Surv(age1b, status) ~ BMI_pred_cat, 
            data = dm %>% filter(!jokela_omitted & !birthbefore_1981))
tab_model(m,
          m1, 
          title = "Males: Chance of having first child, relative to healthy BMI", 
          dv.labels = c("Sample including early childbearers", "Sample excluding early childbearers"), 
          pred.labels  = c("Underweight","Overweight","Obese"),
          file = './Results/220125_jokelacompare_men.doc')

# visualize ---------------------------------------------------------------

#change current datasets
curdt <- df

#plot!
cur_age1b1 <- median(curdt$AGE1B_XRND[curdt$AGE1B_XRND>0])
cur_age1b2 <- median(curdt$AGE1B_XRND[curdt$AGE1B_XRND>0 & !curdt$jokela_omitted & !curdt$birthbefore_1981])

fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = curdt)
p1 <- ggsurvplot(fit, 
                 palette = colors,
                 legend.title = "BMI at 16",
                 legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                 title = "Including early childbearers \n(Anyone giving birth after 16)",
                 fun = "event",
                 xlim = c(16, 45),
                 xlab = "Age (years)")

p1new <- p1$plot +
  scale_x_continuous(limits = c(15, 45),
                     breaks = c(20,30,40)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_vline(xintercept = cur_age1b1, 
             size = 1.5,
             colour = "black",
             linetype = "dotted") +
  xlab("") +
  annotate(geom = "text", 
           label = paste("Median age at 1st birth =", cur_age1b1, sep=""),
           x = cur_age1b1+8.5,
           y = 0.1) +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              base_size = 12,
              plot_margin = margin(10,10,10,10))

fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = curdt %>% filter(!jokela_omitted & !birthbefore_1981))
p2 <- ggsurvplot(fit, 
                 palette = colors,
                 legend.title = "BMI at 16",
                 legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                 title = "Excluding early childbearers \n(Jokela et al's approach)",
                 fun = "event",
                 xlim = c(16, 45),
                 xlab = "Age (years)")
p2new <- p2$plot + 
  scale_x_continuous(limits = c(15, 45),
                     breaks = c(20,30,40)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_vline(xintercept = cur_age1b2, 
             size = 1.5,
             colour = "black",
             linetype = "dotted") +
  ylab("") +
  annotate(geom = "text", 
           label = paste("Median age at 1st birth =", cur_age1b2, sep=""),
           x = cur_age1b2+8.5,
           y = 0.1) +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              base_size = 12,
              plot_margin = margin(10,10,10,10))

#combine
title_theme <- ggdraw() +
  draw_label("Women: Cumulative events of transitioning to first parenthood", 
             #"Men: Cumulative events of transitioning to first parenthood", 
             fontfamily = theme_ipsum()$text$family,
             size = 15,
             x = 0.05,
             hjust = 0)

combined <- cowplot::plot_grid(p1new, p2new, ncol = 2, labels = c("A","B"))
combined_title <- cowplot::plot_grid(title_theme, combined, rel_heights = c(0.1,1), ncol = 1)

ggsave('./Figures/220125_pooled_jokelacompare_women.png',type = "cairo")

ggsave('./Figures/220125_pooled_jokelacompare_men.png',type = "cairo")


