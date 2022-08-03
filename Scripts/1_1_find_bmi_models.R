library('sjPlot')
library(hrbrthemes)
library(ggplot2)

dt <- readRDS('./Data/211209_bmidf_long.rds') %>%
  mutate(SAMPLE_ID_1979 = ifelse(SAMPLE_ID_1979 %in% c(1:8), "cross-sectional",
                                 ifelse(SAMPLE_ID_1979 %in% c(9:14), "supplemental", "military")))

dtm <- dt[dt$sex == 1,]
dtf <- dt[dt$sex == 2,]

m1 <- lm(BMI ~ age + as.factor(after_parenthood) + AGEATINT_1979 + SAMPLE_ID_1979, data = dtm)
m1_1 <- update(m1, .~. + I(age^2))
m1_2 <- update(m1, .~. + age*after_parenthood + I(age^2)*after_parenthood)
m2 <- update(m1_1, .~. + racenew)
m2_1 <- update(m2, .~. + racenew*after_parenthood)
m3 <- update(m2, .~. + residence + religion + mother_hgc + father_hgc + residence_withwhom)
m4 <- update(m3, .~. + (1|CASEID_1979))

AIC(m1,m1_1,m1_2)
AIC(m2,m2_1)
AIC(m1_1,m2,m3,m4)

m_men <- m1_1 #for visualization

# women -------------------------------------------------------------------

m1 <- lm(BMI ~ age + transition + AGEATINT_1979 + SAMPLE_ID_1979, data = dtf)
m1_1 <- update(m1, .~. + I(age^2))
m1_2 <- update(m1, .~. + age*transition)
m2 <- update(m1, .~. + racenew)
m2_1 <- update(m2, .~. + racenew*transition)
m3 <- update(m2, .~. + residence + religion + mother_hgc + father_hgc + residence_withwhom)
m4 <- update(m3, .~. + (1|CASEID_1979))

AIC(m1, m1_1, m1_2)
AIC(m2,m2_1)
AIC(m1,m2,m3)

m_women <- m1 #for visualization

# visualization -----------------------------------------------------------

p1 <- plot_model(m_men, 
           type = "pred", 
           terms = "age", 
           show.data = TRUE,
           jitter = TRUE, dot.size = 0.3, line.size = 1.5,
           axis.title = c("Age in years", "BMI"),
           title = "Males") + 
  theme_ipsum(axis_title_size = 15,
              strip_text_size = 15,
              base_size = 15)

p1 + 
  scale_y_continuous(limits = c(10, 55), breaks = seq(10, 50, by = 10)) +
  scale_x_continuous(breaks = seq(16, 28, by = 2))

ggsave('./Figures/211216_predicted_BMI_men.png',type = "cairo")

p2 <- plot_model(m_women, 
           type = "pred", 
           terms = "age", 
           show.data = TRUE,
           jitter = TRUE, dot.size = 0.3, line.size = 1.5,
           axis.title = c("Age in years", "BMI"),
           title = "Females") +
  theme_ipsum(axis_title_size = 15,
              strip_text_size = 15,
              base_size = 15)

p2 + 
  scale_y_continuous(limits = c(10, 55), breaks = seq(10, 50, by = 10)) +
  scale_x_continuous(breaks = seq(16, 28, by = 2))

ggsave('./Figures/211216_predicted_BMI_women.png',type = "cairo")
