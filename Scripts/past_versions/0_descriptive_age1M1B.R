library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(dplyr)
library(cowplot)

dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")
dt <- readRDS("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp_finalset(201023).RDS")

sex <- dt$SAMPLE_SEX_1979
bmicat <- dt$BMI_cat
childless <- dt$childless
evermarried <- dt$evermarried

age1m <- dt$AGE1M_XRND #age at 1st marriage
age1b <- dt$AGE1B_XRND #age at 1st birt
age1m_filt <- age1m > 0 & !is.na(age1m) & !is.na(bmicat)
age1b_filt <- age1b > 0 & !is.na(age1b) & !is.na(bmicat)

library(dplyr)
desire_cat <- desire
desire_cat[between(desire, 2, 3)] <- 2
desire_cat[desire > 3] <- 3
dt$desire_cat <- as.factor(desire_cat)

#make desire category 2 as reference
dt$desire_cat <- relevel(dt$desire_cat, ref = '2')

dt$desire_cat <- as.factor(dt$desire_cat)

# plotting age1m ----------------------------------------------------------

mu_age1m <- 
  dt %>% 
  subset(age1m_filt & !(is.na(desire_cat))) %>%
  group_by(BMI_cat, SAMPLE_SEX_1979) %>% 
  summarize(mean = mean(AGE1M_XRND),
            age1m_median = median(AGE1M_XRND),
            n = length(AGE1M_XRND))
mu_age1m$label <- paste('Median ', mu_age1m$age1m_median, ' (n=', mu_age1m$n, ')', sep = '')


#age1m_f
mu <- subset(mu_age1m, SAMPLE_SEX_1979 == 2)
dat_text <- data.frame(
  label = mu$label,
  BMI_cat   = c('H','O','U')
)

age1m_f <- 
  dt %>%
  subset((dt$SAMPLE_SEX_1979 %in% 2) & age1m_filt) %>%
  ggplot(aes(x = AGE1M_XRND,
             color = BMI_cat,
             fill = BMI_cat)) +
  geom_histogram(binwidth = 2,
                 aes(y = stat(count / sum(count)))) + 
  scale_x_continuous(minor_breaks = seq(15, 50, 5), 
                     breaks = c(20, 30, 40, 50)) +
  geom_vline(data = mu, 
             aes(xintercept = age1m_median),
             linetype="dashed") +
  geom_vline(data = mu, 
             aes(xintercept = mean),
             linetype="solid") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                     name = "BMI category") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  # scale_fill_viridis(discrete = T) +
  # scale_color_viridis(discrete = T) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(~ BMI_cat, 
             dir = 'v') +
  geom_text(data =  dat_text,
            mapping = aes(x = -Inf, y = -Inf, label = label),
            # hjust = 0.1,
            # vjust = -1,
            x = 44,
            y = 0.05,
            colour = 'black') +
  ggtitle('Age at first marriage: Female')


#age1m_m
mu <- subset(mu_age1m, SAMPLE_SEX_1979 == 1)
dat_text <- data.frame(
  label = mu$label,
  BMI_cat   = c('H','O','U')
)


age1m_m <- 
  dt %>%
  subset((dt$SAMPLE_SEX_1979 %in% 1) & age1m_filt) %>%
  ggplot(aes(x = AGE1M_XRND,
             color = BMI_cat,
             fill = BMI_cat)) +
  geom_histogram(binwidth = 2,
                 aes(y = stat(count / sum(count)))) + 
  scale_x_continuous(minor_breaks = seq(15, 50, 5), 
                     breaks = c(20, 30, 40, 50)) +
  geom_vline(data = mu, 
             aes(xintercept = age1m_median),
             linetype="dashed") +
  geom_vline(data = mu, 
             aes(xintercept = mean),
             linetype="solid") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                     name = "BMI category") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  # scale_fill_viridis(discrete = T) +
  # scale_color_viridis(discrete = T) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(~ BMI_cat, 
             dir = 'v') +
  geom_text(data =  dat_text,
            mapping = aes(x = -Inf, y = -Inf, label = label),
            # hjust = 0.1,
            # vjust = -1,
            x = 44,
            y = 0.05,
            colour = 'black') +
  ggtitle('Age at first marriage: Male')


# plotting age1b ----------------------------------------------------------

mu_age1b <- 
  dt %>% 
  subset(age1b_filt & !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40) %>%
  group_by(BMI_cat, SAMPLE_SEX_1979) %>% 
  summarize(mean = mean(AGE1B_XRND),
            age1b_median = median(AGE1B_XRND),
            n = length(AGE1B_XRND))
mu_age1b$label <- paste('Median ', mu_age1b$age1b_median, ' (n=', mu_age1b$n, ')', sep = '')

#age1b_f
mu <- subset(mu_age1b, SAMPLE_SEX_1979 == 2)
dat_text <- data.frame(
  label = mu$label,
  BMI_cat   = c('H','O','U')
)

age1b_f <- 
  dt %>%
  subset((dt$SAMPLE_SEX_1979 %in% 2) & age1b_filt) %>%
  ggplot(aes(x = AGE1B_XRND,
             color = BMI_cat,
             fill = BMI_cat)) +
  geom_histogram(binwidth = 2,
                 aes(y = stat(count / sum(count)))) + 
  scale_x_continuous(minor_breaks = seq(15, 50, 5), 
                     breaks = c(20, 30, 40, 50)) +
  geom_vline(data = mu, 
             aes(xintercept = age1b_median),
             linetype="dashed") +
  geom_vline(data = mu, 
             aes(xintercept = mean),
             linetype="solid") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                     name = "BMI category") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(~ BMI_cat, 
             dir = 'v') +
  geom_text(data =  dat_text,
            mapping = aes(x = -Inf, y = -Inf, label = label),
            # hjust = 0.1,
            # vjust = -1,
            x = 44,
            y = 0.05,
            colour = 'black') +
  ggtitle('Age at first child birth: Female')

#age1b_m
mu <- subset(mu_age1b, SAMPLE_SEX_1979 == 1)
dat_text <- data.frame(
  label = mu$label,
  BMI_cat   = c('H','O','U')
)

age1b_m <- 
  dt %>%
  subset((dt$SAMPLE_SEX_1979 %in% 1) & age1b_filt) %>%
  ggplot(aes(x = AGE1B_XRND,
             color = BMI_cat,
             fill = BMI_cat)) +
  geom_histogram(binwidth = 2,
                 aes(y = stat(count / sum(count)))) + 
  scale_x_continuous(minor_breaks = seq(15, 50, 5), 
                     breaks = c(20, 30, 40, 50)) +
  geom_vline(data = mu, 
             aes(xintercept = age1b_median),
             linetype="dashed") +
  geom_vline(data = mu, 
             aes(xintercept = mean),
             linetype="solid") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                     name = "BMI category") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_ipsum() +
  theme(
    #legend.position="none",
    #panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(~ BMI_cat, 
             dir = 'v') +
  geom_text(data =  dat_text,
            mapping = aes(x = -Inf, y = -Inf, label = label),
            # hjust = 0.1,
            # vjust = -1,
            x = 44,
            y = 0.05,
            colour = 'black') +
  ggtitle('Age at first child birth: Male')

plot_grid(age1m_f, age1m_m, age1b_f, age1b_m, 
          nrow = 2, 
          ncol = 2, 
          align = 'h')



# age at first pregnancy for women ----------------------------------------
age1p <- dt$AGE1P #age at 1st pregnancy
age1p_filt <- age1p > 0 & !is.na(age1p) & !is.na(bmicat)

mu <- 
  dt %>% 
  subset(age1p_filt) %>%
  group_by(BMI_cat, SAMPLE_SEX_1979) %>% 
  summarize(mean = mean(AGE1P_XRND),
            median = median(AGE1P_XRND),
            n = length(AGE1P_XRND))

mu$label <- paste('Median ', mu$median, ' (n=', mu$n, ')', sep = '')

dat_text <- data.frame(
  label = mu$label,
  BMI_cat   = c('H','O','U')
)


age1p_f <- 
  dt %>%
  subset((dt$SAMPLE_SEX_1979 %in% 2) & age1p_filt) %>%
  ggplot(aes(x = AGE1P_XRND,
             color = BMI_cat,
             fill = BMI_cat)) +
  geom_histogram(binwidth = 2,
                 aes(y = stat(count / sum(count)))) + 
  scale_x_continuous(minor_breaks = seq(15, 50, 5), 
                     breaks = c(20, 30, 40, 50)) +
  geom_vline(data = mu, 
             aes(xintercept = median),
             linetype="dashed") +
  geom_vline(data = mu, 
             aes(xintercept = mean),
             linetype="solid") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                     name = "BMI category") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(~ BMI_cat, 
             dir = 'v') +
  geom_text(data =  dat_text,
            mapping = aes(x = -Inf, y = -Inf, label = label),
            # hjust = 0.1,
            # vjust = -1,
            x = 44,
            y = 0.05,
            colour = 'black') +
  ggtitle('Age at first pregnancy: Female')


# -------------------------------------------------------------------------
dtt <- subset(dt, 
              (!is.na(AGE1M_XRND) & 
                 AGE1M_XRND > 0 & 
                 !is.na(AGE1B_XRND) & 
                 AGE1B_XRND > 0))

x <- 
  dtt %>% 
  group_by(BMI_cat, SAMPLE_SEX_1979) %>% 
  summarize(firstsex = mean(firstsex, na.rm = T),
            age1m = mean(AGE1M_XRND, na.rm = T), 
            age1b = mean(AGE1B_XRND, na.rm = T))

x <- x[1:6, ]
x$order <- c(3, 4, 5, 6, 1, 2)

x_long <- gather(x, event, age, firstsex:age1b, factor_key=TRUE)
x_long$SAMPLE_SEX_1979 <- as.factor(x_long$SAMPLE_SEX_1979)

ggplot(x_long, aes(x = order, y = age)) +
  geom_point(aes(colour = event, shape = SAMPLE_SEX_1979), size = 3) +
  scale_shape_manual(values = c(17, 3)) 