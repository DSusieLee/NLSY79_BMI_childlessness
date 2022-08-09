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
racename <- c("(b) Black", "(c) Hispanic", "(d) White")

bmigroup <- unique(df$BMI_pred_cat)[2:4]
bminame <- c("Underweight","Overweight","Obese")

title_label = c("A. ","B. ","C. ")

# visualize: women ---------------------------------------------------------------

dt <- df

plot_list <- list()
plot_list_byrace <- list()

k <-1

for(i in 1:3){
  
  cur_bmi = as.character(bmigroup[i])
  curdt <- dt %>% filter(BMI_pred_cat %in% c("H",cur_bmi))

  fit_global <- survfit(Surv(age1b, status) ~ BMI_pred_cat,
                        data = curdt)
  
  p1 <- ggsurvplot(fit_global,
                   conf.int = TRUE,
                   xlab = "",
                   ylab = "P(remaining childless)",
                   title = paste(title_label[i], bminame[i], "(grey) compared to", "healthy BMI (black)"),
                   legend.title = "",
                   palette = c("black", "grey48"),
                   xlim = c(0, max(df$age1b)),
                   surv.median.line = "hv") 
  
  p1_1 <-
    p1$plot + 
    scale_x_continuous(breaks = c(4,14,24,34,44),
                       labels = c(20,30,40,50,60)) +
    annotate(
      "text",
      family  = "serif",
      label    = "(a) All women",
      size = 4,
      x = 23, 
      y = 0.95) +
    theme_ipsum(
      base_family = "serif",
      axis_title_size = 13,
      plot_title_size = 15, 
      base_size = 12,
      plot_margin = margin(10,10,10,10),
      grid = FALSE,
      axis = "xy") +
    theme(
      text = element_text(family = "serif"),
      legend.position = "none")
  
  plot_list[[i]] <- p1_1
  
  for(j in 1:3){
    
    cur_race = racegroup[j]
    cur_racename = racename[j]
    fit_race <- survfit(Surv(age1b, status) ~ BMI_pred_cat,
                        data = curdt %>% filter(racenew %in% cur_race))
    
    if(j != 3){
      
      p1 <- ggsurvplot(fit_race,
                     conf.int = TRUE,
                     xlab = "",
                     ylab = "",
                     title = "",
                     palette = c("black", "grey48"),
                     xlim = c(0, max(df$age1b)))
    }
    
    else{
      
      p1 <- ggsurvplot(fit_race,
                       conf.int = TRUE,
                       xlab = "Age (years)",
                       ylab = "",
                       title = "",
                       palette = c("black", "grey48"),
                       xlim = c(0, max(df$age1b)))
    }
    
    p1_1 <- p1$plot + 
      scale_x_continuous(breaks = c(4,14,24,34,44),
                         labels = c(20,30,40,50,60)) +
      annotate(
        "text",
        family  = "serif",
        label    = paste(cur_racename, "women"),
        size = 4,
        x = 23, 
        y = 0.95) +
      theme_ipsum(
        base_family = "serif",
        axis_title_size = 13,
        plot_title_size = 15,
        strip_text_size = 15, 
        base_size = 12,
        plot_margin = margin(10,10,10,10),
        grid = FALSE,
        axis = "xy") +
      theme(legend.position = "none",
            text = element_text(family = "serif")) 
    
    plot_list_byrace[[k]] <- p1_1
    k <- k+1

  }
  
}

#mainplot
#women
mainplot1w <- plot_grid(plot_list[[1]], 
                        plot_list_byrace[[1]], 
                        plot_list_byrace[[2]], 
                        plot_list_byrace[[3]],
                      nrow = 1)

mainplot2w <- plot_grid(plot_list[[2]], 
                        plot_list_byrace[[4]], 
                        plot_list_byrace[[5]], 
                        plot_list_byrace[[6]],
                      nrow = 1)

mainplot3w <- plot_grid(plot_list[[3]], 
                        plot_list_byrace[[7]], 
                        plot_list_byrace[[8]], 
                        plot_list_byrace[[9]],
                      nrow = 1)

plot_grid(mainplot1w, 
          mainplot2w,
          mainplot3w,
          ncol = 1)

ggsave('./Figures/Figure6.png', 
       type = "cairo")
#ggsave('./Figures/220806_age1b_byBMI_women.eps')

# visualize: men ----------------------------------------------------------

dt <- dm

plot_list <- list()
plot_list_byrace <- list()

k <-1

for(i in 1:3){
  
  cur_bmi = as.character(bmigroup[i])
  curdt <- dt %>% filter(BMI_pred_cat %in% c("H",cur_bmi))
  
  fit_global <- survfit(Surv(age1b, status) ~ BMI_pred_cat,
                        data = curdt)
  
  p1 <- ggsurvplot(fit_global,
                   conf.int = TRUE,
                   xlab = "",
                   ylab = "P(remaining childless)",
                   title = paste(title_label[i], bminame[i], "(grey) compared to", "healthy BMI (black)"),
                   legend.title = "",
                   palette = c("black", "grey48"),
                   xlim = c(0, max(df$age1b)),
                   surv.median.line = "hv") 
  
  p1_1 <-
    p1$plot + 
    scale_x_continuous(breaks = c(4,14,24,34,44),
                       labels = c(20,30,40,50,60)) +
    annotate(
      "text",
      family  = "serif",
      label    = "(a) All men",
      size = 4,
      x = 24, 
      y = 0.95) +
    theme_ipsum(
      base_family = "serif",
      axis_title_size = 13,
      plot_title_size = 15, 
      base_size = 12,
      plot_margin = margin(10,10,10,10),
      grid = FALSE,
      axis = "xy") +
    theme(
      text = element_text(family = "serif"),
      legend.position = "none")
  
  plot_list[[i]] <- p1_1
  
  for(j in 1:3){
    
    cur_race = racegroup[j]
    cur_racename = racename[j]
    fit_race <- survfit(Surv(age1b, status) ~ BMI_pred_cat,
                        data = curdt %>% filter(racenew %in% cur_race))
    
    if(j != 3){
      
      p1 <- ggsurvplot(fit_race,
                       conf.int = TRUE,
                       xlab = "",
                       ylab = "",
                       title = "",
                       palette = c("black", "grey48"),
                       xlim = c(0, max(df$age1b)))
    }
    
    else{
      
      p1 <- ggsurvplot(fit_race,
                       conf.int = TRUE,
                       xlab = "Age (years)",
                       ylab = "",
                       title = "",
                       palette = c("black", "grey48"),
                       xlim = c(0, max(df$age1b)))
    }
    
    p1_1 <- p1$plot + 
      scale_x_continuous(breaks = c(4,14,24,34,44),
                         labels = c(20,30,40,50,60)) +
      annotate(
        "text",
        family  = "serif",
        label    = paste(cur_racename, "men"),
        size = 4,
        x = 24, 
        y = 0.95) +
      theme_ipsum(
        base_family = "serif",
        axis_title_size = 13,
        plot_title_size = 15,
        strip_text_size = 15, 
        base_size = 12,
        plot_margin = margin(10,10,10,10),
        grid = FALSE,
        axis = "xy") +
      theme(legend.position = "none",
            text = element_text(family = "serif")) 
    
    plot_list_byrace[[k]] <- p1_1
    k <- k+1
    
  }
  
}

#men
mainplot1m <- plot_grid(plot_list[[1]], plot_list_byrace[[1]], plot_list_byrace[[2]], plot_list_byrace[[3]],
                        nrow = 1)

mainplot2m <- plot_grid(plot_list[[2]], plot_list_byrace[[4]], plot_list_byrace[[5]], plot_list_byrace[[6]],
                        nrow = 1)

mainplot3m <- plot_grid(plot_list[[3]], plot_list_byrace[[7]], plot_list_byrace[[8]], plot_list_byrace[[9]],
                        nrow = 1)

plot_grid(mainplot1m, 
          mainplot2m,
          mainplot3m,
          ncol = 1)

ggsave('./Figures/Figure7.png', 
       type = "cairo")
#ggsave('./Figures/220806_age1b_byBMI_men.eps')
