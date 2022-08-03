library(tidyverse)
library(hrbrthemes)
library(cowplot)

dm <- readRDS('./Data/220118_dm.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         age1b = age1b-15,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('H', 'U', 'OV', 'O')),
         bcohort = as.factor(AGEATINT_1979))

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(AGE1B_XRND != -999 &
           !racenew %in% "Other") %>%
  mutate(status = ifelse(AGE1B_XRND == -998, 1, 2), #1=event not observed, 2=observed
         age1b = ifelse(status == 1, last_surveyage, AGE1B_XRND),
         age1b = age1b-15,
         BMI_pred_cat = factor(BMI_pred_cat, levels = c('H', 'U', 'OV', 'O')),
         bcohort = as.factor(AGEATINT_1979))

#STUDY SAMPLE 10,571
dt <- rbind(dm,df)
#JOKELA SAMPLE
dt_jokela <- dt %>% filter(!jokela_omitted)

#how much % more data points now with early childbearers
(nrow(dt)-nrow(dt_jokela))/nrow(dt_jokela)

# sample descriptives -----------------------------------------------------
dt$sex <- c(rep("Men", nrow(dm)),
            rep("Women", nrow(df)))

dt_childless <- 
  rbind(dt %>% 
        filter(last_surveyage >=40) %>%
        group_by(sex, racenew) %>%
        summarize(childless = sum(childless)/n()) %>%
        mutate(earlyincluded = 'Yes'),
      dt %>% 
        filter(last_surveyage >=40 & !jokela_omitted) %>%
        group_by(sex, racenew) %>%
        summarize(childless = sum(childless)/n()) %>%
        mutate(earlyincluded = 'No') 
      )

#childless
p1 <- 
  ggplot(dt_childless,
         aes(fill = racenew, y = childless, x = earlyincluded)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Early childbearers in sample?") +
  ylab("Proportion") +
  labs(fill = '',
       title = "A. Childlessness at the age 40+") +
  facet_wrap(vars(sex)) +
  theme_ipsum(axis_title_size = 15,
              strip_text_size = 15,
              base_size = 15,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = "top")

#bmi
p2 <- 
  ggplot(dt, 
         aes(x = BMI_pred, colour = racenew)) +
  geom_density(aes(y=..density..), size = 0.7) +
  facet_wrap(vars(sex)) + 
  #labs(title = "") +
  xlab("Body mass index") +
  ylab("Density") + 
  labs(title = "C. Early BMI") +
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

p3 <- ggplot(dt %>% filter(AGE1B_XRND > 0), 
             aes(x = AGE1B_XRND, colour = racenew)) + 
  geom_density(aes(y=..density..), size = 0.7) +
  facet_wrap(vars(sex)) + 
  xlab("Age (years) at birth") +
  ylab("Density") +
  #  xlab("Age at first birth") +
  labs(title = "B. Age at 1st birth") +
  scale_x_continuous(limits = c(16,55)) +
  # scale_fill_manual(values = c("Red","#FFAA00","Green"),
  #                   name = "") +
  theme_ipsum(axis_title_size = 15,
              strip_text_size = 15, 
              base_size = 15,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = "none",
        legend.text = element_text(size = 15))
  
plot_grid(p1, 
          plot_grid(p3, p2, nrow=2),
          nrow = 1)

ggsave('./Figures/220428_descriptives.png', type = "cairo")            
            

