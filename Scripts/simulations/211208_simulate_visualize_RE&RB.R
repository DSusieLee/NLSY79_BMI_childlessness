library(ggplot2)
library(cowplot)
library(tidyverse)
library(hrbrthemes)

sim_enabled_f <- readRDS('./Results/211208_sim_REenabeled_f.rds')
sim_disabled_f <- readRDS('./Results/211208_sim_REdisabeled_f.rds')
sim_enabled_m <- readRDS('./Results/211208_sim_REenabeled_m.rds')
sim_disabled_m <- readRDS('./Results/211208_sim_REdisabeled_m.rds')

#women
df1 <- rbind(sim_enabled_f %>% mutate(re = TRUE), 
            sim_disabled_f %>% mutate(re = FALSE)) 
df1$actual = rep(c(0.038, -0.022, -0.06), nrow(df)/3)

p1 <- ggplot(df1, aes(x = diff, fill = re)) +
  geom_histogram(color="#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values=c("grey60", "grey27"), labels = c("No","Yes")) +
  scale_x_continuous(limits = c(-0.12, 0.15),
                     breaks = c(-0.1, 0, 0.1)) +
  geom_vline(aes(xintercept = 0),
             color="black", linetype="dashed", size=1) +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              plot_title_face = 1,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  labs(fill = "Early childbearing differs by race/ethnicity?") +
  xlab("") +
  ylab("Count") +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.direction='vertical') +
  facet_wrap(~ pairwise)

#men
dm1 <- rbind(sim_enabled_m %>% mutate(re = TRUE), 
            sim_disabled_m %>% mutate(re = FALSE)) 
dm1$actual = rep(c(0.038, -0.022, -0.06), nrow(dm1)/3)

p2 <- ggplot(dm1, aes(x = diff, fill = re)) +
  geom_histogram(color="#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values=c("grey60", "grey27"), labels = c("No","Yes")) +
  scale_x_continuous(limits = c(-0.12, 0.15),
                     breaks = c(-0.1, 0, 0.1)) +
  geom_vline(aes(xintercept = 0),
             color="black", linetype="dashed", size=1) +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              plot_title_face = 1,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  labs(fill = "Early childbearing differs by race/ethnicity?") +
  xlab("Difference in P(childless)") +
  ylab("") +
  theme(legend.position = "top",
        legend.justification = 'left',
        legend.direction='vertical') +
  facet_wrap(~ pairwise)

sim_enabled_f <- readRDS('./Results/211208_sim_RBenabeled_f.rds')
sim_disabled_f <- readRDS('./Results/211208_sim_RBdisabeled_f.rds')
sim_enabled_m <- readRDS('./Results/211208_sim_RBenabeled_m.rds')
sim_disabled_m <- readRDS('./Results/211208_sim_RBdisabeled_m.rds')

#women
df <- rbind(sim_enabled_f %>% mutate(re = TRUE), 
            sim_disabled_f %>% mutate(re = FALSE)) 
df$actual = rep(c(0.038, -0.022, -0.055), nrow(df)/3)

p3 <- ggplot(df, aes(x = diff, fill = re)) +
  geom_histogram(color="#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values=c("grey60", "grey27"), labels = c("No","Yes")) +
  scale_x_continuous(limits = c(-0.12, 0.15),
                     breaks = c(-0.1, 0, 0.1)) +
  geom_vline(aes(xintercept = 0),
             color="black", linetype="dashed", size=1) +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              plot_title_face = 1,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  labs(fill = "Early BMI differs by race/ethnicity?") +
  xlab("Difference in P(childless)") +
  ylab("") +
  theme(legend.position = "top",
        legend.justification = 'left',
        legend.direction='vertical') +
  facet_wrap(~ pairwise)

#men
dm <- rbind(sim_enabled_m %>% mutate(re = TRUE), 
            sim_disabled_m %>% mutate(re = FALSE)) 
dm$actual = rep(c(-0.002, -0.038, -0.036), nrow(dm)/3)

p4 <- ggplot(dm, aes(x = diff, fill = re)) +
  geom_histogram(color="#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values=c("grey60", "grey27"), labels = c("No","Yes")) +
  scale_x_continuous(limits = c(-0.12, 0.15),
                     breaks = c(-0.1, 0, 0.1)) +
  geom_vline(aes(xintercept = 0),
             color="black", linetype="dashed", size=1) +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              plot_title_face = 1,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  labs(fill = "Early BMI differs by race/ethnicity?") +
  xlab("Difference in P(childless)") +
  ylab("") +
  theme(legend.position = "top",
        legend.justification = 'left',
        legend.direction='vertical') +
  facet_wrap(~ pairwise)

# combine -----------------------------------------------------------------

title <- ggdraw() + 
  draw_label(
    "Women: Racial difference in childlessness",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, plot_grid(p1,p3, ncol = 1),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)


ggsave('./Figures/211208_simulate_women.png',type = "cairo")
ggsave('./Figures/211208_simulate_men.png',type = "cairo")
