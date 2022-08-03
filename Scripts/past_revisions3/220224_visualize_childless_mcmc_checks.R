library(bayesplot)

#men
curdt <- readRDS('./Results/220119_stanglm_men.rds')
curdt <- readRDS('./Results/220227_stanglm_men.rds')

fit <- as.array(curdt)
dimnames(fit)[[2]] <- c("chain1", "chain2", "chain3", "chain4")
dimnames(fit)[[3]] <- c("Intercept \n(Underweight X Blacks)", "Healthy BMI", "Overweight", "Obese","Hispanics","Whites","Healthy BMI X Hispanics","Overweight X Hispanics","Obese X Hispanics","Healthy BMI X Whites","Overweight X Whites","Obese X Whites")

rhat(curdt)
neff_ratio(curdt)
mcmc_trace(fit)

mcmc_areas(fit,
           prob = 2/3
) +
  labs(
    title = "Men: Posterior distributions with medians and 2/3 of probability mass (shaded)"
  )

x <- mcmc_intervals_data(fit, rhat = numeric())
write.csv(x, './Results/220227_stanglm_men_outputs.csv')

#women
curdt <- readRDS('./Results/220119_stanglm_women.rds')

fit <- as.array(curdt)
dimnames(fit)[[2]] <- c("chain1", "chain2", "chain3", "chain4")
dimnames(fit)[[3]] <- c("Intercept (Underweight X Blacks", "Healthy BMI", "Overweight", "Obese","Hispanics","Whites","Healthy BMI X Hispanics","Overweight X Hispanics","Obese X Hispanics","Healthy BMI X Whites","Overweight X Whites","Obese X Whites")

rhat(curdt)
neff_ratio(curdt)
mcmc_trace(fit)
mcmc_areas(fit,
           prob = 2/3
) +
  labs(
    title = "Posterior distributions",
    subtitle = "with medians and 90% intervals"
  )

x <- mcmc_intervals_data(fit, rhat = numeric())
write.csv(x, './Results/220224_stanglm_women_outputs.csv')
