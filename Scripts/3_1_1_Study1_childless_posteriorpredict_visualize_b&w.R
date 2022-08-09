library(tidyverse)
library(hrbrthemes)

# women -------------------------------------------------------------------

racegroup <- c("A. All women", "B. Black women","C. Hispanic women","D. White women")
bmigroup <- c("U","H","OV","O")

simdt <- readRDS('./Data/2022-03-01_simdt_women_bmirace.rds') %>%
  mutate(race = ifelse(race %in% "Blacks", racegroup[2],
                       ifelse(race %in% "Hispanics", racegroup[3], racegroup[4])))

simdt <- rbind(simdt, 
               readRDS('./Data/2022-03-03_simdt_women_bmionly.rds') %>%
                 mutate(race = racegroup[1]))

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
         aes(x = value, ..scaled.., group = name, linetype = name)) +
  geom_density(aes(linetype = name), size = 0.8, show.legend = FALSE) +
  stat_density(aes(x = value),
               geom = "line", position = "identity") +
  scale_linetype_manual(values = c("dotdash","solid","dotted","dashed"),
                      name = "Early BMI",
                      labels = rev(c("Obese","Overweight","Healthy","Underweight"))) +
  scale_x_continuous(breaks = seq(0, 0.4, by = 0.05)) +
  geom_vline(aes(xintercept = v), 
             data = vline.dat,
             color = "black", 
             linetype = "solid") +
  geom_hline(yintercept = 0, 
             colour = "white", 
             size = 0.9) +
  xlab("Posterior predicted probability") +
  ylab("Density") +
  facet_wrap(~race, 
             ncol = 1,
             strip.position = "top") +
  theme_ipsum(
    base_family = "serif",
    axis_title_size = 15,
    strip_text_size = 15,
    strip_text_face = 2,
    base_size = 10,
    plot_margin = margin(10,10,10,10)) +
  theme(
    text = element_text(family = "serif"),
    legend.position = c(0.87, .45),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.key.width = unit(2,"cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(0, "lines")) +
  guides(linetype = guide_legend(override.aes = list(size = 0.7)))

ggsave('./Figures/Figure2.png', 
       type = "cairo")

# ggsave(
#   plot = p1,
#   filename = './Figures/Figure2.eps',
#   device = "eps")

# men ---------------------------------------------------------------------

racegroup <- c("A. All men", "B. Black men","C. Hispanic men","D. White men")
bmigroup <- c("U","H","OV","O")

simdt <- readRDS('./Data/2022-03-01_simdt_men_bmirace.rds') %>%
  mutate(race = ifelse(race %in% "Blacks", racegroup[2],
                       ifelse(race %in% "Hispanics", racegroup[3], racegroup[4])))

simdt <- rbind(simdt, 
               readRDS('./Data/2022-03-03_simdt_men_bmionly.rds') %>%
                 mutate(race = racegroup[1]))

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
         aes(x = value, ..scaled.., group = name, linetype = name)) +
  geom_density(aes(linetype = name), size = 0.8, show.legend = FALSE) +
  stat_density(aes(x = value),
               geom = "line", position = "identity") +
  scale_linetype_manual(values = c("dotdash","solid","dotted","dashed"),
                        name = "Early BMI",
                        labels = rev(c("Obese","Overweight","Healthy","Underweight"))) +
  scale_x_continuous(breaks = seq(0.15, 0.45, by = 0.05)) +  
  geom_vline(aes(xintercept = v), 
             data = vline.dat,
             color = "black", linetype = "solid") +
  geom_hline(yintercept = 0, colour = "white", size = 0.9) +
  xlab("Posterior predicted probability") +
  ylab("Density") +
  facet_wrap(~race, 
             ncol = 1,
             strip.position = "top") +
  theme_ipsum(
    base_family = "serif",
    axis_title_size = 15,
    strip_text_size = 15,
    strip_text_face = 2,
    base_size = 10,
    plot_margin = margin(10,10,10,10)) +
  theme(
    text = element_text(family = "serif"),
    legend.position = c(0.85, .45),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.key.width = unit(3,"cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(0, "lines")) +
  guides(linetype = guide_legend(override.aes = list(size = 0.7)))

ggsave('./Figures/Figure3.png', type = "cairo")   
ggsave('./Figures/Figure3.eps')
