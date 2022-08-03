library(tidyverse)
library(ggplot2)
library(hrbrthemes)

dt <- readRDS('./Data/211014_7810.rds') %>%
  mutate(BMIcat = relevel(BMIcat, ref = "H"),
         racenew = relevel(as.factor(racenew), ref = "Whites"),
         BMIscaled = scale(BMI)) %>%
  filter(!is.na(EarlyParents) &
           !(racenew %in% "Other")) %>%
  mutate(racenew = droplevels(racenew))

dtm <- dt %>% filter(sex == 1)
dtf <- dt %>% filter(sex == 2)

# prepare simulated outputs df --------------------------------------------

race <- c("Whites","Blacks","Hispanics")

newdtm <- data.frame(racenew = rep(race,
                                   times = as.numeric(table(dtm$racenew))))
newdtf <- data.frame(racenew = rep(race,
                                   times = as.numeric(table(dtf$racenew))))

# -------------------------------------------------------------------------
m1 <- glm(EarlyParents ~ racenew,
          data = dtm,
          family = "binomial")
m2 <- lm(BMIscaled ~ racenew + EarlyParents, 
         data = dtm)
m3 <- glm(childless ~  poly(BMIscaled, 2),
          data = dtm %>% filter(!EarlyParents),
          family = "binomial")
f1 <- glm(EarlyParents ~ racenew,
          data = dtf,
          family = "binomial")
f2 <- lm(BMIscaled ~ racenew*EarlyParents, 
         data = dtf)
f3 <- glm(childless ~  racenew + BMIscaled,
          data = dtf %>% filter(!EarlyParents),
          family = "binomial")

simfun <- function(option, models, dt, newdt){
  
  #1. EarlyParents
  model1 <- models[[1]]
  prob <- predict.glm(model1, newdt, type = "response")
  newdt$EarlyParents <- rbinom(n = length(prob), 
                               size = 1, 
                               prob = prob) %>% as.logical()
  
  #2. BMI
  model2 <- models[[2]]
  model2$model$racenew <- newdt$racenew
  model2$model$EarlyParents <- newdt$EarlyParents
  
  newdt$BMIscaled <- simulate(model2, nsim = 1)[,1]
  
  #3. childless
  if(option == 1){#Edge from BMI to childlessness enabled
    
    model3 <- models[[3]]
    prob = predict.glm(model3, newdt, type = "response")
    newdt$childless <- rbinom(n = length(prob), 
                              size = 1, 
                              prob = prob) %>% as.logical()
    newdt$childless[newdt$EarlyParents] <- FALSE
    
  }
  
  else{#Edge from BMI to childlessness disabled
    
    model3 <- glm(childless ~ racenew, dt)
    prob = predict.glm(model3, newdt, type = "response")
    newdt$childless <- rbinom(n = length(prob), 
                              size = 1, 
                              prob = prob) %>% as.logical()
    newdt$childless[newdt$EarlyParents] <- FALSE
    
  }
  
  return(newdt)

    }

# repeat! -------------------------------------------------------------------
simnum = 1000

groupbyfun <- function(d){
  d <- d %>%
    group_by(racenew) %>% 
    summarize(n = n(), childless = sum(childless)/n) %>%
    select(-n) %>%
    mutate(pairwise = c("Blacks-Hispanics",
                        "Blacks-Whites",
                        "Hispanics-Whites"),
           diff = c(childless[racenew %in% "Blacks"]-childless[racenew %in% "Hispanics"],
                    childless[racenew %in% "Blacks"]-childless[racenew %in% "Whites"],
                    childless[racenew %in% "Hispanics"]-childless[racenew %in% "Whites"]
           ))
  return(d)
}

for(i in 1:simnum){
  
  set.seed(i)
  
  if(i == 1){
    
    sim_enabled_f <- simfun(1, list(f1,f2,f3), dtf, newdtf) %>% groupbyfun()
    sim_disabled_f <- simfun(2, list(f1,f2,f3), dtf, newdtf) %>% groupbyfun()
    sim_enabled_m <- simfun(1, list(m1,m2,m3), dtm, newdtm) %>% groupbyfun()
    sim_disabled_m <- simfun(2, list(m1,m2,m3), dtm, newdtm) %>% groupbyfun()
    
  }
  
  else{
    
    sim_enabled_f <- rbind(sim_enabled_f, 
                             simfun(1, list(f1,f2,f3), dtf, newdtf) %>% groupbyfun()) 
    sim_disabled_f <- rbind(sim_disabled_f,
                              simfun(2, list(f1,f2,f3), dtf, newdtf) %>% groupbyfun())
    sim_enabled_m <- rbind(sim_enabled_m, 
                             simfun(1, list(m1,m2,m3), dtm, newdtm) %>% groupbyfun()) 
    sim_disabled_m <- rbind(sim_disabled_m,
                              simfun(2, list(m1,m2,m3), dtm, newdtm) %>% groupbyfun())
    
  }
  
}

saveRDS(sim_disabled_f, './Results/211016_sim_BCdisabeled_f.rds')
saveRDS(sim_disabled_m, './Results/211016_sim_BCdisabeled_m.rds')
saveRDS(sim_enabled_f, './Results/211016_sim_BCenabeled_f.rds')
saveRDS(sim_enabled_m, './Results/211016_sim_BCenabeled_m.rds')


# visualize ---------------------------------------------------------------

#women
df <- rbind(sim_enabled_f %>% mutate(re = TRUE), 
           sim_disabled_f %>% mutate(re = FALSE)) 
df$actual = rep(c(0.038, -0.022, -0.055), nrow(df)/3)

ggplot(df, aes(x = diff, fill = re)) +
  geom_histogram(color="#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  geom_vline(aes(xintercept = 0),
             color="black", linetype="dashed", size=1) +
  theme_ipsum() +
  labs(fill = "Race-BMI Enabeled?") +
  xlab("Diff P(childless)") +
  facet_wrap(~ pairwise)


#men
dm <- rbind(sim_enabled_m %>% mutate(re = TRUE), 
            sim_disabled_m %>% mutate(re = FALSE)) 
dm$actual = rep(c(-0.002, -0.038, -0.036), nrow(dm)/3)

ggplot(dm, aes(x = diff, fill = re)) +
  geom_histogram(color="#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  geom_vline(aes(xintercept = 0),
             color="black", linetype="dashed", size=1) +
  theme_ipsum() +
  labs(fill = "Race-BMI Enabeled?") +
  xlab("Diff P(childless)") +
  facet_wrap(~ pairwise)
