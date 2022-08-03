dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp.RDS")

library(stringr)

var_list <- names(dt)

health1 <- var_list[!is.na(str_extract(var_list, 'Q11-4_'))] #DOES HEALTH LIMIT KIND OF WORK R CAN DO?
health2 <- var_list[!is.na(str_extract(var_list, 'Q11-5_'))] #DOES HEALTH LIMIT AMOUNT OF WORK R CAN DO?

health_df <- dt[, c(health1[1:5], health2[1:5])]

earlyhealth <- apply(health_df, 1, function(x) sum(x, na.rm = T))
earlyhealth[is.infinite(earlyhealth)] <- NA
eversaid <- earlyhealth != 0

dt$earlyhealth <- earlyhealth
dt$eversaid <- eversaid

x <- table(eversaid, dt$BMI_cat, dt$SAMPLE_SEX_1979)[,,1]
y <- table(eversaid, dt$BMI_cat, dt$SAMPLE_SEX_1979)[,,2]

prop.table(x, 2)
prop.table(y, 2)

subs_pal <- colorspace::qualitative_hcl(7)

dt_male <- subset(dt, SAMPLE_SEX_1979 %in% 1)
dt_female <- subset(dt, SAMPLE_SEX_1979 %in% 2)

packages = c('vcd', 'vcdExtra', 'tidyverse')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

subs_pal <- colorspace::qualitative_hcl(7)

mosaicplot(table(dt_female$eversaid, dt_female$BMI_cat), 
           col = subs_pal,
)

mosaicplot(table(dt_male$eversaid, dt_male$BMI_cat), 
           col = subs_pal,
)

mh1 <- glm(eversaid ~ 
            log(BMI) + 
            I(log(BMI)^2) +
            college +
            as.factor(SAMPLE_RACE_78SCRN),
          data = subset(dt, SAMPLE_SEX_1979 %in% 1), 
          family = binomial
)

mh2 <- glm(eversaid ~ 
             log(BMI) + 
             I(log(BMI)^2) +
             college +
             as.factor(SAMPLE_RACE_78SCRN),
           data = subset(dt, SAMPLE_SEX_1979 %in% 2), 
           family = binomial
)



tab_model(mh, 
          pred.labels = c("Intercept", 
                          "Overweight/Obese", 
                          "Underweight", "Female","Black","Hispanic"),
          dv.labels = c("Health limited during early adulthood"),
          string.ci = "CI (95%)", transform = NULL, 
          string.p = "P-Value")


mh_male <- glm(childless ~ 
                 eversaid + 
                 log(BMI) + 
                 I(log(BMI)^2) +
                 college +
                 as.factor(SAMPLE_RACE_78SCRN),
               data = subset(dt, SAMPLE_SEX_1979 %in% 1),
               family = binomial
)

mh_female <- glm(childless ~ 
                   eversaid + 
                   log(BMI) + 
                   I(log(BMI)^2) + 
                   college +
                   as.factor(SAMPLE_RACE_78SCRN),
                 data = subset(dt, SAMPLE_SEX_1979 %in% 2),
                 family = binomial
)

tab_model(mh_female,
          mh_male,
          me_female,
          me_male,
          transform = NULL, 
          # pred.labels = c("eversaid","self-esteem", "Intercept", "Log(BMI)", "Log(BMI)^2",
          #                 "College", "Black", "Hispanic"), 
          dv.labels = c("","","",""),
          string.ci = "CI (95%)",
          string.p = "P-Value")


# across time -------------------------------------------------------------

bmih <- dt$BMI_cat %in% 'H'
bmio <- dt$BMI_cat %in% 'O'
bmiu <- dt$BMI_cat %in% 'U'
female <- dt$SAMPLE_SEX_1979 %in% '2'

bmicat <- c('H','O','U')

percent_unhealth <- rep(NA, length(health)*2*3)
l <- 1

for(i in 1:27){
  
  cur_col <- dt[, health[i]]
  
  for(j in 1:2){
    
    cur_sex_filt <- dt$SAMPLE_SEX_1979 %in% j
    
    for(k in 1:3){
      
      cur_bmi_filt <- dt$BMI_cat %in% bmicat[k]
      
      percent_unhealth[l] <- sum(cur_col[cur_sex_filt & cur_bmi_filt], na.rm = T)/sum(!is.na(cur_col[cur_sex_filt & cur_bmi_filt]))
      
      l <- l + 1
      
    }
    
  }
  
}

year <- c(1979:1994, seq(1996,2016, by = 2))

healthdt <- data.frame(year = rep(year, each = 6),
                       sex = rep(rep(c("Male","Female"), each = 3), 27),
                       bmi = rep(c("H", "O", "U"), 27),
                       health = percent_unhealth)

ggplot(healthdt, aes(x = year, y = health, group = bmi)) + 
  geom_point(aes(colour = bmi), alpha = 0.3) +
  geom_smooth(aes(group = bmi, colour = bmi), se = FALSE) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labels = c('Healthy', 'Overweight/Obese', 'Underweight'),
                     name = "BMI category") +
  facet_grid(. ~ sex) 


#by childlessness
l <- 1

for(i in 1:27){
  
  cur_col <- dt[, health[i]]
  
  for(j in 1:2){
    
    if(j == 1){
      
      cur_sex_filt <- dt$childless %in% 'TRUE'
    }
    
    if(j == 2){
      
      cur_sex_filt <- dt$childless %in% 'FALSE'
    }

    
    for(k in 1:3){
      
      cur_bmi_filt <- dt$BMI_cat %in% bmicat[k]
      
      percent_unhealth[l] <- sum(cur_col[cur_sex_filt & cur_bmi_filt], na.rm = T)/sum(!is.na(cur_col[cur_sex_filt & cur_bmi_filt]))
      
      l <- l + 1
      
    }
    
  }
  
}

year <- c(1979:1994, seq(1996,2016, by = 2))

healthdt2 <- data.frame(year = rep(year, each = 6),
                       sex = rep(rep(c("childless","not childless"), each = 3), 27),
                       bmi = rep(c("H", "O", "U"), 27),
                       health = percent_unhealth)


