library(bayesplot)

#men
curdt <- readRDS('./Results/220227_stanglm_men_withbcohort.rds')

fit <- as.array(curdt)
dimnames(fit)[[2]] <- c("chain1", "chain2", "chain3", "chain4")
dimnames(fit)[[3]] <- c("Intercept \n(Healthy BMI & Whtes)",
                        "Underweight", 
                        "Overweight", 
                        "Obese",
                        "Birth cohort 1964",
                        "Birth cohort 1963",
                        "Birth cohort 1962",
                        "Birth cohort 1961",
                        "Birth cohort 1960",
                        "Birth cohort 1959",
                        "Birth cohort 1958",
                        "Birth cohort 1957",
                        "Blacks",
                        "Hispanics",
                        "Underweight & Blacks",
                        "Overweight & Blacks",
                        "Obese & Blacks",
                        "Underweight & Hispanics",
                        "Overweight & Hispanics",
                        "Obese & Hispanics")

rhat(curdt)
neff_ratio(curdt)
mcmc_trace(fit)
ggsave('./Figures/220228_study1_traceplot_men.png',type = "cairo")

mcmc_areas(fit,
           prob = 2/3) +
  labs(
    title = "Posterior distributions",
    subtitle = "with medians and 2/3 of probability mass (shaded) intervals"
  )
ggsave('./Figures/220228_study1_posterior_men.png',type = "cairo")


x <- mcmc_intervals_data(fit, rhat = numeric())
write.csv(x, './Results/220227_stanglm_men_outputs.csv')

#women
curdt <- readRDS('./Results/220227_stanglm_women.rds')

fit <- as.array(curdt)
dimnames(fit)[[2]] <- c("chain1", "chain2", "chain3", "chain4")
dimnames(fit)[[3]] <- c("Intercept (Healthy BMI & Blacks)", "Underweight", "Overweight", "Obese",
                        "Hispanics", "Whites",
                        "Underweight & Hispanics","Overweight & Hispanics","Obese & Hispanics",
                        "Underweight & Whites","Overweight & Whites","Obese & Whites")

rhat(curdt)
neff_ratio(curdt)
mcmc_trace(fit)
ggsave('./Figures/220227_study1_traceplot_women.png',type = "cairo")

mcmc_areas(fit,
           prob = 2/3
) +
  labs(
    title = "Posterior distributions",
    subtitle = "with medians and 90% intervals"
  )

x <- mcmc_intervals_data(fit, rhat = numeric())
write.csv(x, './Results/220227_stanglm_women_outputs.csv')
