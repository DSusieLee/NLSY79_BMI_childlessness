library(tidyverse)
library(hrbrthemes)

racegroup <- c("A. All", "B. Blacks","C. Hispanics","D. Whites")
bmigroup <- c("U","H","OV","O")

#women
simdt <- readRDS('./Data/2022-03-01_simdt_women_bmirace.rds') %>%
  mutate(race = ifelse(race %in% "Blacks", "B. Blacks",
                       ifelse(race %in% "Hispanics", "C. Hispanics", "D. Whites")))

simdt <- rbind(simdt, 
               readRDS('./Data/2022-03-03_simdt_women_bmionly.rds') %>%
                 mutate(race = "A. All"))

within_race_average <- simdt %>% 
  group_by(race) %>% 
  summarize(p = mean(healthy))
simdt$p <- within_race_average$p

vline.dat <- data.frame(race = racegroup, 
                        v = within_race_average$p)

p1<-
  ggplot(simdt %>% 
           pivot_longer(cols = c("underweight","healthy","overweight","obese")) %>%
           mutate(name = factor(name, levels =  c("underweight","healthy","overweight","obese"))),
         aes(x = value, ..scaled.., group = name, colour = name, fill = name, alpha = name)) +
  geom_density(aes(colour = name, fill = name, alpha = name), size = 0.8, show.legend = FALSE) +
  stat_density(aes(x = value, colour = name, fill = name, alpha = name),
               geom = "line", position = "identity") +
  scale_colour_manual(values = c("grey82","darkturquoise","grey49","grey5"),
                      name = "",
                      labels = rev(c("Obese","Overweight","Healthy","Underweight"))) +
  scale_fill_manual(values = c("grey82","darkturquoise","grey49","grey5")) +
  scale_alpha_manual(values = c(0, 0.5, 0, 0)) +
  scale_x_continuous(breaks = seq(0, 0.4, by = 0.05)) +
  geom_vline(aes(xintercept = v), 
             data = vline.dat,
             color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "white", size = 0.9) +
  # xlab("") +
  xlab("Posterior predicted probability") +
  ylab("Density") +
  ggtitle("Women: Probability of remaining childless at age 40+") +
  facet_wrap(~race, 
             ncol = 1,
             strip.position = "top") +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              strip_text_face = 2,
              base_size = 10,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = c(0.9, .55),
        legend.text = element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  guides(colour = guide_legend(override.aes = list(size = 1.5)),
         fill = "none",
         alpha = "none")

ggsave('./Figures/220302_posterior_contrast_women.png', type = "cairo")


#men
simdt <- readRDS('./Data/2022-03-01_simdt_men_bmirace.rds') %>%
  mutate(race = ifelse(race %in% "Blacks", "B. Blacks",
                       ifelse(race %in% "Hispanics", "C. Hispanics", "D. Whites")))

simdt <- rbind(simdt, 
               readRDS('./Data/2022-03-03_simdt_men_bmionly.rds') %>%
                 mutate(race = "A. All"))

within_race_average <- simdt %>% 
  group_by(race) %>% 
  summarize(p = mean(healthy))
simdt$p <- within_race_average$p

vline.dat <- data.frame(race = racegroup, 
                        v = within_race_average$p)

p2<-
  ggplot(simdt %>% 
           pivot_longer(cols = c("underweight","healthy","overweight","obese")) %>%
           mutate(name = factor(name, levels =  c("underweight","healthy","overweight","obese"))),
         aes(x = value, ..scaled.., group = name, colour = name, fill = name, alpha = name)) +
  geom_density(aes(colour = name, fill = name, alpha = name), size = 0.8, show.legend = FALSE) +
  stat_density(aes(x = value, colour = name, fill = name, alpha = name),
               geom = "line", position = "identity") +
  scale_colour_manual(values = c("grey82","darkturquoise","grey49","grey5"),
                      name = "",
                      labels = rev(c("Obese","Overweight","Healthy","Underweight"))) +
  scale_fill_manual(values = c("grey82","darkturquoise","grey49","grey5")) +
  scale_alpha_manual(values = c(0, 0.5, 0, 0)) +
  scale_x_continuous(breaks = seq(0.15, 0.45, by = 0.05)) +
  geom_vline(aes(xintercept = v), 
             data = vline.dat,
             color = "black", linetype = "dashed") +
  geom_hline(yintercept=0, colour="white", size = 0.9) +
  xlab("Posterior predicted probability") +
  ylab("Density") +
  ggtitle("Men: Probability of remaining childless at age 40+") +
  facet_wrap(~race, 
             ncol = 1,
             strip.position = "top") +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              strip_text_face = 2,
              base_size = 10,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = c(0.9, .55),
        legend.text = element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  guides(colour = guide_legend(override.aes = list(size = 1.5)),
         fill = "none",
         alpha = "none")

ggsave('./Figures/220302_posterior_contrast_men.png', type = "cairo")   

# taking the difference... ------------------------------------------------

p<-
  ggplot(simdt %>% 
           mutate(UD_H = UD_H+average,
                  OV_H = OV_H+average,
                  OB_H = OB_H+average) %>%
           pivot_longer(cols = c("UD_H","healthy","OV_H","OB_H")) %>%
           mutate(name = factor(name, levels =  c("UD_H","healthy","OV_H","OB_H"))),
         aes(x = value, group = name, colour = name, fill = name, alpha = name)) +
  geom_density(aes(colour = name, fill = name, alpha = name), size = 0.8, show.legend = FALSE) +
  stat_density(aes(x = value, colour = name, fill = name, alpha = name),
               geom = "line", position = "identity") +
  scale_colour_manual(values = c("grey82","darkturquoise","grey49","grey5"),
                      name = "",
                      labels = rev(c("Obese","Overweight","Healthy","Underweight"))) +
  scale_fill_manual(values = c("grey82","darkturquoise","grey49","grey5")) +
  scale_alpha_manual(values = c(0, 0.5, 0, 0)) +
  geom_vline(aes(xintercept = v), 
             data = vline.dat,
             color = "black", linetype = "dashed") +
  geom_hline(yintercept=0, colour="white", size = 0.9) +
  xlab("Posterior predicted childlessness") +
  ylab("Density") +
  ggtitle("Men") +
  facet_wrap(~race, 
             ncol = 1,
             strip.position = "top") +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              base_size = 10,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = c(0.7,0.55),
        legend.text = element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 1.5)),
         fill = "none",
         alpha = "none")

ggsave('./Figures/220119_posterior_contrast_women.png', type = "cairo")     