library(tidyverse)
library(survey)
library(srvyr)
library(forcats)
library(ggplot2)
library(hrbrthemes)

dt <- readRDS('./Data/211214_dm.rds') %>% 
  bind_rows(readRDS('./Data/211214_df.rds')) %>%
  mutate(sex = c(rep('Males', 5369),
                 rep('Females', 5226)))

#BMI distribution
p <- 
  ggplot(dt, 
         aes(x = BMI_pred, linetype = racenew)) +
  scale_linetype_manual(values = c("solid","dotted","dashed")) +
  geom_density(aes(y=..density..), size = 0.5) +
  facet_wrap(vars(sex)) + 
  #labs(title = "") +
  xlab("Early BMI") +
  ylab("Density") + 
  scale_color_jco() +
  geom_vline(aes(xintercept = 18.5),
             color="grey", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = 24.9),
             color="grey", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = 29.9),
             color="grey", linetype="dashed", size=1) +
  theme_ipsum(axis_title_size = 15,
              strip_text_size = 15,
              base_size = 15,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = "none")

# childlessness -----------------------------------------------------------

dtw <- dt %>% 
  mutate(sampleweights = sampleweights$V2) %>%
  as_survey(weights = sampleweights) %>%
  group_by(sex, racenew) %>%
  summarize(n = n(),
            childless_unwtd = sum(childless == TRUE)/n,
            childless_wtd = survey_mean(childless == TRUE)) %>%
  rbind(
    dt %>% 
      filter(EarlyParents == FALSE) %>%
      mutate(sampleweights = sampleweights$V2) %>%
      as_survey(weights = sampleweights) %>%
      group_by(sex, racenew) %>%
      summarize(n = n(),
                childless_unwtd = sum(childless == TRUE)/n,
                childless_wtd = survey_mean(childless == TRUE)
      ) 
    ) 

dtw$ep <- c(rep("Excl.", nrow(dtw)/2), rep("Incl.", nrow(dtw)/2))

p1 <- 
  ggplot(dtw, aes(fill = racenew, y = childless_wtd, x = ep)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Early Chiledbearers: Excluded vs. Included") +
  ylab("Proportion") +
  labs(fill = '',
       title = "Racial difference in childlessness and early BMI") +
  scale_fill_jco() +
  facet_wrap(vars(sex)) +
  theme_ipsum(axis_title_size = 15,
              strip_text_size = 15,
              base_size = 15,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = "top")

# combine -----------------------------------------------------------------

cowplot::plot_grid(p1, p, ncol = 1, rel_heights = c(1,0.8))
ggsave('./Figures/211208_racialdifference.png', type = "cairo")
