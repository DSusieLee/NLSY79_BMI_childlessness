library(stringr)

dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_201013.RDS")
dt_simple <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_1pwanted_ageinterview.RDS")
  
#reduce data
filter <- dt$SAMPLE_ID_1979 <= 8 #excluding supplementary subjects 

dt1 <- cbind(dt[filter,], dt_simple[filter,])

#setwd("C:/Users/vlab/Nextcloud/Codes/BMI_fertility/Data")
setwd("U:/cloud/Codes/BMI_fertility/Data/")
saveRDS(dt1, 'NLSY79_bmifertility_nosupp.RDS')

# create new variables ----------------------------------------------------
dt <- dt1

var_list <- names(dt)
l <- nrow(dt)

survey_round <- c(c(1979:1994), seq(1996, 2016, 2))

# 1. BMI ------------------------------------------------------------------
height <- var_list[!is.na(str_extract(var_list, 'HEIGHT'))]
weight <- var_list[!is.na(str_extract(var_list, 'Q11.9'))]
weight_early <- weight[1:3]

BMI1981 <- rep(NA, l)
BMI1982 <- rep(NA, l)
BMI1985 <- rep(NA, l)

k <- 1

for(i in 1:3){
  
  cur_height_vector <- dt[, height[i]]
  cur_weight_vector <- dt[, weight_early[i]]
  
  any_na_rows <- unique(c(which(is.na(cur_height_vector)), 
                          which(is.na(cur_weight_vector))))
  
  if(i == 1){
    
    wt <- cur_weight_vector[-any_na_rows]*0.453592
    ht <- cur_height_vector[-any_na_rows]
    ht_m <- (as.numeric(substring(ht, first = 1, last = 1))*30 + as.numeric(substring(ht, first = 2, last = 3))*2.54)/100
    
    BMI1981[-any_na_rows] <- wt/(ht_m^2)
    
  }
  
  if(i == 2){
    
    wt <- cur_weight_vector[-any_na_rows]*0.453592
    ht <- cur_height_vector[-any_na_rows]
    ht_m <- (ht*2.54)/100
    
    BMI1982[-any_na_rows] <- wt/(ht_m^2)
    
  }
  
  if(i == 3){
    
    wt <- cur_weight_vector[-any_na_rows]*0.453592
    ht <- cur_height_vector[-any_na_rows]
    ht_m <- (ht*2.54)/100
    
    BMI1985[-any_na_rows] <- wt/(ht_m^2)
    
  }
  
}

dt$BMI1981 <- BMI1981
dt$BMI1982 <- BMI1982
dt$BMI1985 <- BMI1985

dt$BMI1981[dt$NUMCH81_1981 >0] <- NA #excluding those who already had child
dt$BMI1982[dt$NUMCH82_1982 >0] <- NA

BMIearly <- apply(data.frame(dt$BMI1981, dt$BMI1982), 1, function(x) mean(x, na.rm = T))

BMI_cat <- ifelse(BMIearly < 18.5, 'U', NA)
BMI_cat[is.na(BMI_cat)] <- ifelse(BMIearly[is.na(BMI_cat)] < 25, 'H', 'O')

dt$BMI_cat <- BMI_cat
dt$BMI <- BMIearly

library(dplyr)
dt$BMIcat <- dt$BMI_cat
dt$BMIcat[between(dt$BMI, 25, 29.9)] <- 'OV'

# 2. Evermarried & Survey year at last observation & first sex----------------------------------------------------------
maritalstatus <- var_list[!is.na(str_extract(var_list, 'MARSTAT-COL'))][2:27]

x <- dt[, maritalstatus]
evermarried <- apply(x, 1, function(n) any(n %in% 2)) #ever married at or since 1981
lastage <- apply(x, 1, function(n) max(which(!is.na(n)), na.rm = T)) 
dropped_after_1979 <- unique(which(is.infinite(lastage)))

dt$evermarried <- evermarried
dt$last_surveyround[!is.na(lastage)] <- survey_round[lastage[!is.na(lastage)] + 1]
dt$last_surveyround[dropped_after_1979] <- 1979
dt$last_surveyage <- dt$last_surveyround - 1979 + dt$AGEATINT_1979

#not using firstsex info because only available 1983, 84, 85
# male_firstsex <- cbind(dt$`MFER-15_1983` , dt$`MFER-15_1984`, dt$`MFER-15_1985`)
# female_firstsex <- cbind(dt$`FFER-92_1983`, dt$`FFER-92_1984`, dt$`FFER-92_1985`)
# 
# dt$firstsex[dt$SAMPLE_SEX_1979 %in% 1] <- male_firstsex[dt$SAMPLE_SEX_1979 %in% 1]
# dt$firstsex[dt$SAMPLE_SEX_1979 %in% 2] <- dt$`FFER-92_1985`[dt$SAMPLE_SEX_1979 %in% 2]

# 3. Entered college or not -----------------------------------------------
educ <- var_list[!is.na(str_extract(var_list, 'HGCREV'))]
educ_df <- dt[,educ]

highest_degree <- apply(educ_df, 1, function(x) max(x, na.rm = T))

dt$college <- highest_degree <=12

# 4. Childlessness --------------------------------------------------------
dt$childless <- dt$NUMKID_XRND == 0 #number of children ever born

# 5. Race -----------------------------------------------------------------
dt$SAMPLE_RACE_78SCRN <- rev(dt$SAMPLE_RACE_78SCRN) #1: non-black non-hispanic; 2: black; 3: hispanic

saveRDS(dt, 'NLSY79_bmifertility_nosupp.RDS')

# 6. final set of subjects ------------------------------------------------
dt_sub <- subset(dt, !(is.na(childless)) & !is.na(BMI) & dt$last_surveyage >= 40)

saveRDS(dt_sub, 'NLSY79_bmifertility_nosupp_finalset(201023).RDS')