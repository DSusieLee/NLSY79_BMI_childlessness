library(tidyverse)
library(rstanarm)
library(hrbrthemes) 
library(bayestestR)
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

# stan_surv models --------------------------------------------------------

#current data
dt <- dm %>%
  mutate(age1b = age1b + 1)

mod1 <- stan_surv(formula = Surv(age1b, status) ~ BMI_pred_cat,
                  data = dt,
                  basehaz = "exp",
                  iter = 5000)

saveRDS(mod1, './Results/220122_rstansurv_men.rds')


for(i in 1:3){
  
  cur_race <- racegroup[i]
  
  curdt <- dt %>% filter(racenew %in% cur_race)
  
  #model
  mod1 <- stan_surv(formula = Surv(age1b, status) ~ BMI_pred_cat,
                    data = curdt,
                    basehaz = "exp",
                    iter = 5000)
  
  filename = paste('./Results/220122_rstansurv_men_',cur_race, '.rds', sep = "")
  saveRDS(mod1, filename)
  
}


# figures:women -----------------------------------------------------------------

plot_list <- list()

#women
xposition = -0.85

for(i in 1:3){
  
  cur_race <- racegroup[i]
  filename = paste('./Results/220122_rstansurv_women_', cur_race, '.rds', sep = "")
  mod1 <- readRDS(filename)
  
  pdraws <- as.data.frame(mod1$stanfit) %>%
    pivot_longer(cols = c("BMI_pred_catU","BMI_pred_catOV","BMI_pred_catO")) %>% 
    select(name, value) %>% 
    arrange(name) %>% 
    mutate(zeronot = ifelse(value>0, "Positive", "Negative"),
           name = ifelse(name %in% "BMI_pred_catU", "A",
                         ifelse(name %in% "BMI_pred_catOV", "B", "C")))
  
  annotate_data <- pdraws %>% 
    group_by(name) %>% 
    summarize(n= sum(zeronot %in% "Negative")/length(zeronot)) %>%
    mutate(x = rep(xposition,3),
           y = rep(0.9,3),
           n = paste(round(n, 2)*100, "%", sep="")
    )
  
    p <- ggplot(pdraws, aes(x=value, ..scaled..)) +
      geom_density(aes(colour = name), fill = "white", size = 0.8, alpha = 0, show.legend = FALSE) +
      # geom_histogram(aes(fill = name), bins = 70) +
      # geom_histogram(data = pdraws %>% filter(zeronot %in% "Higher"), bins = 50) +
      # geom_histogram(data = pdraws %>% filter(zeronot %in% "Negative"), bins = 50) +
      scale_colour_manual(values = colors) +
      scale_x_continuous(limits = c(-1, 1), breaks = c(-1,-0.5,0,0.5,1)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
      geom_hline(yintercept = 0, colour = "white", size = 0.9) +
      # scale_y_continuous(limits = c(0,2300),
      #                    breaks = c(0,500,1000,1500,2000)) +
      ggtitle(cur_race) +
      xlab("") +
      ylab("")
      
      
    if(i == 1){
      
      annotate_data$n[1] <- paste(annotate_data$n[1], "of the posterior \nestimates are negative", sep = "")
      annotate_data$y[1] <- 0.8
      #women
      annotate_data$x[1] <- -0.34

      newlabs = c("Underweight","Overweight","Obese")
      names(newlabs) = c("A","B","C")
      
      p <- p +
        facet_wrap(~name,  
                   ncol = 1,
                   labeller = labeller(name = newlabs)
                   ) +
        geom_text(
          data    = annotate_data,
          mapping = aes(x = x, y = y, label = n),
        ) +
        theme_ipsum(axis_title_size = 15,
                    plot_title_size = 15,
                    strip_text_size = 15, 
                    base_size = 10,
                    plot_margin = margin(0,10,10,0)) +
        theme(panel.spacing.y = unit(0, "lines"),
              legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        ylab("Density")
      
      plot_list[[i]] <- p
      
    }
    
    else{
      
      newlabs = c("","","")
      names(newlabs) = c("A","B","C")
      
      p <- p +
        facet_wrap(~name, 
                   ncol = 1,
                   labeller = labeller(name = newlabs)
        ) +
        geom_text(
          data    = annotate_data,
          mapping = aes(x = x, y = y, label = n),
        ) +
        theme_ipsum(axis_title_size = 15,
                    plot_title_size = 15,
                    strip_text_size = 15, 
                    base_size = 10,
                    plot_margin = margin(0,10,10,0)) +
        theme(panel.spacing.y = unit(0, "lines"),
              legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
      
      if(i == 2){      
        plot_list[[i]] <- p}
      
      else{
        plot_list[[i]] <- p + xlab("Hazard rate minus the rate of healthy BMI")
        
      }
      
    }
}

    # #ridge plot?
    # ggplot(pdraws, aes(x = value, y = name, fill = zeronot)) +
    #   geom_density_ridges2(scale = 4) +
    #   ylab("") +
    #   xlab("Relative risks compared to healthy BMI") +
    #   scale_fill_manual(name = "Effect direction",
    #                     values = c("#10a53dFF",  "#541352FF")) +
    #   geom_vline(xintercept = 0, linetype = "dashed") 

title_theme <- ggdraw() +
  draw_label("Women: Hazard rate difference with healthy BMI\n Negative difference means slower transition to first parenthood than healthy BMI",
    fontfamily = theme_ipsum()$text$family,
    size = 15,
    x = 0,
    hjust = 0)

combined <- plot_grid(plot_list[[1]],
                      plot_list[[2]],
                      plot_list[[3]],
                      nrow = 1,
                      labels = c("A","B","C"))

combined_title <- plot_grid(title_theme, 
                            combined,
                            nrow = 2, rel_heights = c(0.2,1))

ggsave('./Figures/220122_age1b_posterior_women.png', type = "cairo")  

# men ---------------------------------------------------------------------

plot_list <- list()

#men
xposition = -1

for(i in 1:3){
  
  cur_race <- racegroup[i]
  filename = paste('./Results/220122_rstansurv_men_', cur_race, '.rds', sep = "")
  mod1 <- readRDS(filename)
  
  pdraws <- as.data.frame(mod1$stanfit) %>%
    pivot_longer(cols = c("BMI_pred_catU","BMI_pred_catOV","BMI_pred_catO")) %>% 
    select(name, value) %>% 
    arrange(name) %>% 
    mutate(zeronot = ifelse(value>0, "Positive", "Negative"),
           name = ifelse(name %in% "BMI_pred_catU", "A",
                         ifelse(name %in% "BMI_pred_catOV", "B", "C")))
  
  annotate_data <- pdraws %>% 
    group_by(name) %>% 
    summarize(n= sum(zeronot %in% "Negative")/length(zeronot)) %>%
    mutate(x = rep(xposition,3),
           y = rep(0.9,3),
           n = paste(round(n, 2)*100, "%", sep="")
    )
  
  p <- ggplot(pdraws, aes(x=value, ..scaled..)) +
    geom_density(aes(colour = name), fill = "white", size = 0.8, alpha = 0, show.legend = FALSE) +
    # geom_histogram(aes(fill = name), bins = 70) +
    # geom_histogram(data = pdraws %>% filter(zeronot %in% "Higher"), bins = 50) +
    # geom_histogram(data = pdraws %>% filter(zeronot %in% "Negative"), bins = 50) +
    scale_colour_manual(values = colors) +
    scale_x_continuous(limits = c(-1.4, 1), breaks = c(-1,-0.5,0,0.5,1)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
    geom_hline(yintercept = 0, colour = "white", size = 0.9) +
    # scale_y_continuous(limits = c(0,2300),
    #                    breaks = c(0,500,1000,1500,2000)) +
    ggtitle(cur_race) +
    xlab("") +
    ylab("")
  
  if(i == 1){
    
    annotate_data$n[1] <- paste(annotate_data$n[1], "of the posterior \nestimates are negative", sep = "")
    annotate_data$y[1] <- 0.8

    #men
    annotate_data$x[1] <- 0.25
    
    newlabs = c("Underweight","Overweight","Obese")
    names(newlabs) = c("A","B","C")
    
    p <- p +
      facet_wrap(~name,  
                 ncol = 1,
                 labeller = labeller(name = newlabs)
      ) +
      geom_text(
        data    = annotate_data,
        mapping = aes(x = x, y = y, label = n),
      ) +
      theme_ipsum(axis_title_size = 15,
                  plot_title_size = 15,
                  strip_text_size = 15, 
                  base_size = 10,
                  plot_margin = margin(0,10,10,0)) +
      theme(panel.spacing.y = unit(0, "lines"),
            legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      ylab("Density")
    
    plot_list[[i]] <- p
    
  }
  
  else{
    
    newlabs = c("","","")
    names(newlabs) = c("A","B","C")
    
    p <- p +
      facet_wrap(~name, 
                 ncol = 1,
                 labeller = labeller(name = newlabs)
      ) +
      geom_text(
        data    = annotate_data,
        mapping = aes(x = x, y = y, label = n),
      ) +
      theme_ipsum(axis_title_size = 15,
                  plot_title_size = 15,
                  strip_text_size = 15, 
                  base_size = 10,
                  plot_margin = margin(0,10,10,0)) +
      theme(panel.spacing.y = unit(0, "lines"),
            legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
    if(i == 2){      
      plot_list[[i]] <- p}
    
    else{
      plot_list[[i]] <- p + xlab("Hazard rate minus the rate of healthy BMI")
      
    }
    
  }
}

title_theme <- ggdraw() +
  draw_label("Men: Hazard rate difference with healthy BMI\n Negative difference means slower transition to first parenthood than healthy BMI",
             fontfamily = theme_ipsum()$text$family,
             size = 15,
             x = 0,
             hjust = 0)

combined <- plot_grid(plot_list[[1]],
                      plot_list[[2]],
                      plot_list[[3]],
                      nrow = 1,
                      labels = c("A","B","C"))

combined_title <- plot_grid(title_theme, 
                            combined,
                            nrow = 2, rel_heights = c(0.15,1))

ggsave('./Figures/220122_age1b_posterior_men.png', type = "cairo")  


# posterior draws ---------------------------------------------------------

#newdata
nd <- data.frame(BMI_pred_cat = c("H",curbmi))

numsim = 1e4

for(j in 1:numsim){
  
  m <- posterior_survfit(mod1, type = "surv", newdata = nd) %>%
    mutate(id = ifelse(id == 1, "Healthy BMI", curbmi))
  
  if(j == 1){
    p <- ggplot(m, aes(x = time, y = median, group = id, colour = id)) +
      geom_line()
  }
  
  else{
    p <- p + 
      geom_line(aes(x = time, y = median, group = id, colour = id), data = m)
    
  }
    
  
}

