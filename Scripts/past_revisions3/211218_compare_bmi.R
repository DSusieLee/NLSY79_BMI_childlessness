library(tidyverse)
library(sjPlot)

df <- readRDS('./Data/211214_df.rds') %>%
  filter(last_surveyage >=40)
df$BMI_pred_cat <- factor(df$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))

f_df <- rbind(
  as.data.frame(prop.table(
    table(df$childless[df$BMI_known_beforeparenthood], 
        df$BMI_pred_cat[df$BMI_known_beforeparenthood]),2)),
  as.data.frame(prop.table(
    table(df$childless, 
          df$BMI_pred_cat), 2)
    )
  ) %>%
  mutate(subset = c(rep(TRUE, 8), rep(FALSE, 8)))

ggplot(f_df %>% filter(Var1 == TRUE), 
       aes(x = Var2,
           y = Freq,
           group = subset)) +
  geom_point() +
  geom_line()

# males -------------------------------------------------------------------

df <- readRDS('./Data/211214_dm.rds')
df$BMI_pred_cat <- factor(df$BMI_pred_cat, 
                          levels = c('U', 'H', 'OV', 'O'))

f_df <- rbind(
  as.data.frame(prop.table(
    table(df$childless[df$BMI_known_beforeparenthood], 
          df$BMI_pred_cat[df$BMI_known_beforeparenthood]),2)),
  as.data.frame(prop.table(
    table(df$childless, 
          df$BMI_pred_cat), 2)
  )
) %>%
  mutate(subset = c(rep(TRUE, 8), rep(FALSE, 8)))

ggplot(f_df %>% filter(Var1 == TRUE), 
       aes(x = Var2,
           y = Freq,
           group = subset)) +
  geom_point() +
  geom_line()