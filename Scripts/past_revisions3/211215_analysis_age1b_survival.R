library(tidyverse)
library(survival)
library(survminer)
library(sjPlot)

dm <- readRDS('./Data/211214_dm.rds') %>% 
  filter(AGE1B_XRND != -999) %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND))

df <- readRDS('./Data/211214_df.rds') %>% 
  filter(AGE1B_XRND != -999) %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND))

racegroup <- c("Blacks","Hispanics","Whites")

dm$BMI_pred_cat <- factor(dm$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))
df$BMI_pred_cat <- factor(df$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))

# females -----------------------------------------------------------------

#compare
m <- coxph(Surv(age1b, status) ~ BMI_pred_cat*evermarried, 
           data = df %>% mutate(BMI_pred_cat = relevel(BMI_pred_cat, ref = "H")))
m1 <- coxph(Surv(age1b, status) ~ BMI_pred_cat*evermarried, 
           data = df %>% 
             mutate(BMI_pred_cat = relevel(BMI_pred_cat, ref = "H")) %>%
             filter(BMI_known_beforeparenthood))
tab_model(m,
          m1, 
          title = "Females: Chance of having first child, relative to healthy BMI", 
          dv.labels = c("Sample including early childbearers", "Sample excluding early childbearers"), 
          pred.labels  = c("Underweight","Overweight","Obese"))

#check for proportionality assumption
test <- cox.zph(m, transform = "km", terms = TRUE, singledf = FALSE, global = TRUE)

#visualize
fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = df)
p1 <- ggsurvplot(fit, 
                 palette = rev(c("#0c2a50ff",
                                 "#6b4596ff",
                                 "#cc6a70ff",
                                 "#f9b641ff")),
                 legend.title = "BMI at 16",
                 legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                 title = "Sample including early childbearers \n(Anyone giving birth after 16)",
                 fun = "event",
                 xlim = c(16, 45),
                 xlab = "Age (years)")

p1new <- p1$plot +
  scale_x_continuous(limits = c(15, 45),
                     breaks = c(20,30,40)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1))

fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = df %>% filter(BMI_known_beforeparenthood))
p2 <- ggsurvplot(fit, 
                 palette = rev(c("#0c2a50ff",
                                 "#6b4596ff",
                                 "#cc6a70ff",
                                 "#f9b641ff")),
                 legend.title = "BMI at 16",
                 legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                 title = "Sample excluding early childbearers \n(Jokela et al's approach)",
                 fun = "event",
                 xlim = c(16, 45),
                 xlab = "Age (years)")
p2new <- p2$plot + 
  scale_x_continuous(limits = c(15, 45),
                     breaks = c(20,30,40)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) 
#  geom_vline(xintercept = 26)

cowplot::plot_grid(p1new, p2new, ncol=2, labels = c("A","B"))

ggsave('./Figures/211219_males_pooled_Jokelacompare.png',type = "cairo")

#differently by race groups

plot_list <- list()
plot_list_subset <- list()

for(i in 1:3){
  
  cur_d <- df %>% filter(racenew %in% racegroup[i])
  
  fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = cur_d)
  fit_subset <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = cur_d %>% filter(BMI_known_beforeparenthood))

  p <- ggsurvplot(fit,
                  palette = rev(c("#0c2a50ff",
                                  "#6b4596ff",
                                  "#cc6a70ff",
                                  "#f9b641ff")),
                  legend.title = "BMI at 16",
                  legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                  fun = "event",
                  xlim = c(16, 45),
                  xlab = "Age (years)",
                  title = racegroup[i]) 
  
  p <- 
    p$plot + 
    scale_x_continuous(limits = c(15, 45),
                       breaks = c(20,30,40)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    geom_vline(xintercept = median(cur_d$AGE1B_XRND[cur_d$AGE1B_XRND>0])) +
    theme_ipsum(axis_title_size = 15,
                strip_text_size = 15,
                base_size = 15) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.position = "top")
  
  plot_list[[i]] <- p
  
  p <- ggsurvplot(fit_subset,
                  palette = rev(c("#0c2a50ff",
                                  "#6b4596ff",
                                  "#cc6a70ff",
                                  "#f9b641ff")),
                  legend.title = "BMI at 16",
                  legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                  fun = "event",
                  xlim = c(16, 45),
                  xlab = "Age (years)",
                  title = racegroup[i]) 
  
  p <- 
    p$plot + 
    scale_x_continuous(limits = c(15, 45),
                       breaks = c(20,30,40)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    geom_vline(xintercept = median(cur_d$AGE1B_XRND[cur_d$BMI_known_beforeparenthood &
                                                      cur_d$AGE1B_XRND>0])) +
    theme_ipsum(axis_title_size = 15,
                strip_text_size = 15,
                base_size = 15) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.position = "top")
  
  plot_list_subset[[i]] <- p
  
}

cowplot::plot_grid(plot_list[[1]],plot_list[[2]],plot_list[[3]],ncol=1)
cowplot::plot_grid(plot_list_subset[[1]],plot_list_subset[[2]],plot_list_subset[[3]],ncol=1)

ggsave('./Figures/211218_females_age1b.png',type = "cairo")


# stratified --------------------------------------------------------------

fit_strata <- survSplit(Surv(age1b, status) ~ ., 
                        data = df,
                        cut = c(20, 25, 35), 
                        episode = "tgroup")

fit_stratified <- coxph(Surv(tstart,age1b,status) ~ BMI_pred_cat:strata(tgroup), data = fit_strata)



# males -------------------------------------------------------------------

#compare
m <- coxph(Surv(age1b, status) ~ BMI_pred_cat, 
           data = dm %>% mutate(BMI_pred_cat = relevel(BMI_pred_cat, ref = "H")))
m1 <- coxph(Surv(age1b, status) ~ BMI_pred_cat, 
            data = dm %>% 
              mutate(BMI_pred_cat = relevel(BMI_pred_cat, ref = "H")) %>%
              filter(BMI_known_beforeparenthood))

tab_model(m,
          m1, 
          title = "Males: Chance of having first child, relative to healthy BMI", 
          dv.labels = c("Sample including early childbearers", "Sample excluding early childbearers"), 
          pred.labels  = c("Underweight","Overweight","Obese"))

#check for proportionality assumption
test <- cox.zph(m, transform = "km", terms = TRUE, singledf = FALSE, global = TRUE)

#visualize
fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = dm)
p1 <- ggsurvplot(fit, 
                 palette = rev(c("#0c2a50ff",
                                 "#6b4596ff",
                                 "#cc6a70ff",
                                 "#f9b641ff")),
                 legend.title = "BMI at 16",
                 legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                 title = "Sample including early childbearers \n(Anyone giving birth after 16)",
                 fun = "event",
                 xlim = c(16, 45),
                 xlab = "Age (years)")

p1new <- p1$plot +
  scale_x_continuous(limits = c(15, 45),
                     breaks = c(20,30,40)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1))

fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = dm %>% filter(BMI_known_beforeparenthood))
p2 <- ggsurvplot(fit, 
                 palette = rev(c("#0c2a50ff",
                                 "#6b4596ff",
                                 "#cc6a70ff",
                                 "#f9b641ff")),
                 legend.title = "BMI at 16",
                 legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                 title = "Sample excluding early childbearers \n(Jokela et al's approach)",
                 fun = "event",
                 xlim = c(16, 45),
                 xlab = "Age (years)")
p2new <- p2$plot + 
  scale_x_continuous(limits = c(15, 45),
                     breaks = c(20,30,40)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) 
#  geom_vline(xintercept = 26)

cowplot::plot_grid(p1new, p2new, ncol=2, labels = c("A","B"))
ggsave('./Figures/211219_males_pooled_Jokelacompare.png', type = "cairo")

#differently by race groups

plot_list <- list()

for(i in 1:3){
  
  cur_d <- dm %>% filter(racenew %in% racegroup[i])
  fit <- survfit(Surv(age1b, status) ~ BMI_pred_cat, data = cur_d)
  p <- ggsurvplot(fit,
                  palette = rev(c("#0c2a50ff",
                                  "#6b4596ff",
                                  "#cc6a70ff",
                                  "#f9b641ff")),
                  legend.title = "BMI at 16",
                  legend.labs = c("Underweight","Healthy","Overweight","Obese"),
                  fun = "event",
                  xlim = c(16, 45),
                  xlab = "Age (years)",
                  title = racegroup[i]) 
  
  p <- 
    p$plot + 
    scale_x_continuous(limits = c(15, 45),
                       breaks = c(20,30,40)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    geom_vline(xintercept = median(cur_d$AGE1B_XRND[cur_d$BMI_known_beforeparenthood &
                                                      cur_d$AGE1B_XRND>0])) +
    theme_ipsum(axis_title_size = 15,
                strip_text_size = 15,
                base_size = 15) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  plot_list[[i]] <- p
  
}

cowplot::plot_grid(plot_list[[1]],plot_list[[2]],plot_list[[3]],ncol=1)
ggsave('./Figures/211218_males_age1b.png',type = "cairo")


fit_strata <- survSplit(Surv(age1b, status) ~ ., 
                        data = dm,
                        cut = c(20, 25, 35), 
                        episode = "tgroup")

fit1_stratified <- coxph(Surv(tstart,age1b,status) ~ BMI_pred_cat:strata(tgroup), data = fit_strata)
