library('tidyverse')
library('cowplot')

racegroup <- c("Blacks","Hispanics","Whites")

# figures:women -----------------------------------------------------------------

plot_list <- list()
racegroup_title <- c("A. Black women",
                     "B. Hispanic women",
                     "C. White women")

#women
xposition = -0.85

for(i in 1:3){
  
  cur_race <- racegroup[i]
  cur_race_title <- racegroup_title[i]
  
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
    mutate(x = rep(xposition, 3),
           y = rep(0.9, 3),
           n = paste(round(n, 2)*100, "%", sep="")
    )
  
  p <- 
    ggplot(pdraws, aes(x=value, ..scaled..)) +
    geom_density(size = 0.7, show.legend = FALSE) +
    scale_x_continuous(limits = c(-1, 1), breaks = c(-1,-0.5,0,0.5,1)) +
    geom_vline(xintercept = 0, linetype = "solid", colour = "black", size = 0.5) +
 #   geom_hline(yintercept = 0, colour = "white", size = 0.9) +
    ggtitle(cur_race_title) +
    xlab("") +
    ylab("")
  
  if(i == 1){
    
    annotate_data$n[1] <- paste(annotate_data$n[1], " of the posterior \nestimates are negative", sep = "")
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
        family  = "serif",
        data    = annotate_data,
        mapping = aes(x = x, y = y, label = n),
      ) +
      theme_ipsum(
        base_family = "serif",
        axis_title_size = 15,
        plot_title_size = 15,
        strip_text_size = 15, 
        base_size = 10,
        plot_margin = margin(0,10,10,0)) +
      theme(
        text = element_text(family = "serif"),
        panel.spacing.y = unit(0, "lines"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
      ylab("Density")
    
    plot_list[[i]] <- p +
      xlab(" \n ")
    
  }
  
  else{
    
    newlabs = c("Underweight","Overweight","Obese")
    names(newlabs) = c("A","B","C")
    
    p <- p +
      facet_wrap(~name, 
                 ncol = 1,
                 labeller = labeller(name = newlabs)
      ) +
      geom_text(
        family  = "serif",
        data    = annotate_data,
        mapping = aes(x = x, y = y, label = n),
      ) +
      theme_ipsum(
        base_family = "serif",
        axis_title_size = 15,
        plot_title_size = 15,
        strip_text_size = 15, 
        base_size = 10,
        plot_margin = margin(0,10,10,0)) +
      theme(
        text = element_text(family = "serif"),
        panel.spacing.y = unit(0, "lines"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
    
    if(i == 2){      
      plot_list[[i]] <- p +
        xlab(" \n ")}
    
    else{
      plot_list[[i]] <- p + 
        xlab("Hazard rate for BMI group \nminus hazard rate for healthy BMI reference group (|)")
      
    }
    
  }
}

combined <- plot_grid(plot_list[[1]],
                      plot_list[[2]],
                      plot_list[[3]],
                      nrow = 1)

ggsave('./Figures/Figure4.png', 
       type = "cairo")
#ggsave('./Figures/220805_age1b_posterior_women.eps')  

# men ---------------------------------------------------------------------

plot_list <- list()
racegroup_title <- c("A. Black men",
                     "B. Hispanic men",
                     "C. White men")

#men
xposition = -0.85

for(i in 1:3){
  
  cur_race <- racegroup[i]
  cur_race_title <- racegroup_title[i]
  
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
    mutate(x = rep(xposition, 3),
           y = rep(0.9, 3),
           n = paste(round(n, 2)*100, "%", sep="")
    )
  
  p <- ggplot(pdraws, aes(x=value, ..scaled..)) +
    geom_density(size = 0.7, show.legend = FALSE) +
    # geom_density(aes(linetype = name), size = 0.8, show.legend = FALSE) +
    # scale_linetype_manual(values = c("dotdash","dotted","dashed")) +
    scale_x_continuous(limits = c(-1.4, 1), breaks = c(-1,-0.5,0,0.5,1)) +
    geom_vline(xintercept = 0, linetype = "solid", colour = "black", size = 0.5) +
    # geom_hline(yintercept = 0, colour = "white", size = 0.9) +
    ggtitle(cur_race_title) +
    xlab("") +
    ylab("")
  
  if(i == 1){
    
    annotate_data$n[1] <- paste(annotate_data$n[1], " of the posterior \nestimates are negative", sep = "")
    annotate_data$y[1] <- 0.4
    
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
        family = "serif",
        data    = annotate_data,
        mapping = aes(x = x, y = y, label = n),
      ) +
      theme_ipsum(
        base_family = "serif",        
        axis_title_size = 15,
                  plot_title_size = 15,
                  strip_text_size = 15, 
                  base_size = 10,
                  plot_margin = margin(0,10,10,0)) +
      theme(
        text = element_text(family = "serif"),
        panel.spacing.y = unit(0, "lines"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
      ylab("Density")
    
    plot_list[[i]] <- p +
      xlab(" \n ")
    
  }
  
  else{
    
    newlabs = c("Underweight","Overweight","Obese")
    names(newlabs) = c("A","B","C")
    
    p <- p +
      facet_wrap(~name, 
                 ncol = 1,
                 labeller = labeller(name = newlabs)
      ) +
      geom_text(
        family  = "serif",
        data    = annotate_data,
        mapping = aes(x = x, y = y, label = n),
      ) +
      theme_ipsum(
        base_family = "serif",
        axis_title_size = 15,
        plot_title_size = 15,
        strip_text_size = 15, 
        base_size = 10,
        plot_margin = margin(0,10,10,0)) +
      theme(
        text = element_text(family = "serif"),
        panel.spacing.y = unit(0, "lines"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
  
    if(i == 2){      
      plot_list[[i]] <- p +
        xlab(" \n ")}
    
    else{
      plot_list[[i]] <- p + 
        xlab("Hazard rate for BMI group \nminus hazard rate for healthy BMI reference group (|)")
    }
    
  }
}

combined <- plot_grid(plot_list[[1]],
                      plot_list[[2]],
                      plot_list[[3]],
                      nrow = 1)

ggsave('./Figures/Figure5.png', 
       type = "cairo")
#ggsave('./Figures/220805_age1b_posterior_men.eps')  
