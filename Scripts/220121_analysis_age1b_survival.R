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
         age1b = age1b-16,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('H', 'U', 'OV', 'O')))

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         age1b = age1b-16,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('H', 'U', 'OV', 'O')))

racegroup <- c("Blacks","Hispanics","Whites")
   
bmigroup <- unique(df$BMI_pred_cat)[2:4]
bminame <- c("Underweight","Overweight","Obese")

colors = c("grey82","grey49","grey5")

# visualize ---------------------------------------------------------------

dt <- df

plot_list <- list()
plot_list_byrace <- list()

k <-1

for(i in 1:3){
  
  cur_bmi = as.character(bmigroup[i])
  curdt <- dt %>% filter(BMI_pred_cat %in% c("H",cur_bmi))
  
  cur_color = colors[i]
  
  fit_global <- survfit(Surv(age1b, status) ~ BMI_pred_cat,
                        data = curdt)
  
  
  p1 <- ggsurvplot(fit_global,
                   conf.int = TRUE,
                   xlab = "",
                   ylab = "Probability",
                   #title = "All women",
                   title = "",
                   legend.title = "",
                   palette = c("darkturquoise", cur_color),
                   xlim = c(0, max(df$age1b)),
                   surv.median.line = "hv",
                   legend.labs = c("Healthy BMI", bminame[i])) 
  
  p1_1 <-
    p1$plot + 
    scale_x_continuous(breaks = c(4,14,24,34,44),
                       labels = c(20,30,40,50,60)) +
    theme_ipsum(axis_title_size = 15,
                plot_title_size = 15,
                strip_text_size = 15, 
                base_size = 12,
                plot_margin = margin(10,10,10,10)) +
    theme(legend.position = c(0.7, 0.9),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15))
  
  plot_list[[i]] <- p1_1
  
  for(j in 1:3){
    
    cur_race = racegroup[j]
    fit_race <- survfit(Surv(age1b, status) ~ BMI_pred_cat,
                        data = curdt %>% filter(racenew %in% cur_race))
    
    if(j != 3){p1 <- ggsurvplot(fit_race,
                     conf.int = TRUE,
                     xlab = "",
                     ylab = "",
                     title = cur_race,
                     palette = c("darkturquoise",cur_color),
                     xlim = c(0, max(df$age1b)))
    }
    
    else{p1 <- ggsurvplot(fit_race,
                          conf.int = TRUE,
                          title = cur_race,
                          xlab = "Age at first birth",
                          ylab = "",
                          palette = c("darkturquoise",cur_color),
                          xlim = c(0, max(df$age1b)))
    }
    
    p1_1 <- p1$plot + 
      scale_x_continuous(breaks = c(4,14,24,34,44),
                         labels = c(20,30,40,50,60)) +
      theme_ipsum(axis_title_size = 15,
                  plot_title_size = 15,
                  strip_text_size = 15, 
                  base_size = 12,
                  plot_margin = margin(10,10,10,10)) +
      theme(legend.position = "none") 
    
    plot_list_byrace[[k]] <- p1_1
    k <- k+1

  }
  
}

#combine
title_theme <- ggdraw() +
  draw_label("Women: Probability of remaining childless over time",
             #"Men: Probability of remaining childless over time",
             fontfamily = theme_ipsum()$text$family,
             size = 15,
             x = 0,
             hjust = 0)

#mainplot
#women
mainplot1w <- plot_grid(plot_list[[1]], plot_list_byrace[[1]], plot_list_byrace[[2]], plot_list_byrace[[3]],
                      nrow = 1)

mainplot2w <- plot_grid(plot_list[[2]], plot_list_byrace[[4]], plot_list_byrace[[5]], plot_list_byrace[[6]],
                      nrow = 1)

mainplot3w <- plot_grid(plot_list[[3]], plot_list_byrace[[7]], plot_list_byrace[[8]], plot_list_byrace[[9]],
                      nrow = 1)

plot_grid(title_theme, 
          mainplot1w, 
          mainplot2w,
          mainplot3w,
          nrow = 4, 
          rel_heights = c(0.15,1,1,1), 
          labels = c("","A","B","C"))
ggsave('./Figures/220126_age1b_byBMI_women.png', type = "cairo")

#men
mainplot1m <- plot_grid(plot_list[[1]], plot_list_byrace[[1]], plot_list_byrace[[2]], plot_list_byrace[[3]],
                        nrow = 1)

mainplot2m <- plot_grid(plot_list[[2]], plot_list_byrace[[4]], plot_list_byrace[[5]], plot_list_byrace[[6]],
                        nrow = 1)

mainplot3m <- plot_grid(plot_list[[3]], plot_list_byrace[[7]], plot_list_byrace[[8]], plot_list_byrace[[9]],
                        nrow = 1)
plot_grid(title_theme, 
          mainplot1m, 
          mainplot2m,
          mainplot3m,
          nrow = 4, rel_heights = c(0.15,1,1,1), labels = c("","A","B","C"))
ggsave('./Figures/220126_age1b_byBMI_men.png', type = "cairo")
