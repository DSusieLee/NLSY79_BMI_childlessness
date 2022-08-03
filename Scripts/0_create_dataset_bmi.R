library(tidyverse)
library(data.table)

# Reading in datasets - combine ----------------------------------------

dt1  <- readRDS("./Data/NLSY79_bmifertility_201013_ORIGINAL.RDS")
dt2 <- readRDS("./Data/NLSY79_bmifertility_1pwanted_ageinterview.RDS")
dt3 <- as.data.frame(read_csv("./Data/210220_race_details.csv"))
dt4 <- readRDS("./Data/NLSY79_bmifertility_211209_add.RDS")

#race:
names(dt3)[2] <- 'racecode'

#combine all data frames:
dt_new <- cbind(dt1, dt2, dt3, dt4)
dt <- dt_new[, !duplicated(colnames(dt_new))]

#sample weights:
wt <- 
  read.table("./Data/customweight_nlsy79_8409/customweight_nlsy79_6037bc071fad1.dat", 
             header = F) %>%
  select(V2) 

dt$weights <- wt/100

# Preparing to create new variables ----------------------------------------------------

var_list <- names(dt)
l <- nrow(dt)

# Background -------------------------------------------------------------

#race
dt <- dt %>%
  mutate(racenew = ifelse(racecode == 1, "Blacks", 
                          ifelse(between(racecode, 15, 21), "Hispanics", 
                                 ifelse(racecode %in% c(3, 5:7, 11,12, 21:25, 27), "Whites", "Other"))),
         race = ifelse(SAMPLE_RACE_78SCRN == 1, "Hispanics",
                       ifelse(SAMPLE_RACE_78SCRN == 2, "Blacks", "Whites")))

#residence types
dt$residence <- as.factor(ifelse(is.na(dt$`FAM-6_1979`), 4, dt$`FAM-6_1979`))
#1: urban, 2: rural, 3: farm/ranch
dt$residence_withwhom <- rep(NA, nrow(dt))
dt$residence_withwhom[dt$`FAM-7_1979` == 11] <- 'both'
dt$residence_withwhom[dt$`FAM-7_1979` %in% c(21, 41)] <- 'stepf'
dt$residence_withwhom[dt$`FAM-7_1979` %in% c(12, 14)] <- 'stepm'
dt$residence_withwhom[dt$`FAM-7_1979` %in% c(51, 91)] <- 'singlem'
dt$residence_withwhom[dt$`FAM-7_1979` %in% c(15, 19)] <- 'singlef'
dt$residence_withwhom[is.na(dt$residence_withwhom)] <- 'other'

#religion
dt$religion <- dt$`R_REL-1_COL_1979`
dt$religion[is.na(dt$religion)] <- 10
dt$religion <- as.factor(dt$religion)

#parents
dt$mother_hgc <- ifelse(is.na(dt$`HGC-MOTHER_1979`), 30, dt$`HGC-MOTHER_1979`)
dt$mother_hgc <- cut(dt$mother_hgc, c(-1,0,6,12,21,31))
dt$mother_living <- dt$`H40-BPAR-6_1979` #many missing/skips
dt$father_hgc <- ifelse(is.na(dt$`HGC-FATHER_1979`), 30, dt$`HGC-FATHER_1979`)
dt$father_hgc <- cut(dt$father_hgc, c(-1,0,6,12,21,31))
dt$father_living <- dt$`H40-BPAR-1_1979` #many missing/skips

# Ever married ------------------------------------------------------------

maritalstatus <- var_list[!is.na(str_extract(var_list, 'MARSTAT-COL'))]

m_ts <- dt[, maritalstatus]

dt$evermarried <- apply(m_ts, 1, function(n) any(n %in% 2)) #ever married at least once

# Age at last survey ------------------------------------------------------

lastsurvey <- apply(m_ts, 1, function(n) max(which(!is.na(n)), na.rm = T)) 

survey_round <- c(c(1979:1994), seq(1996, 2016, 2))

dt$last_surveyround <- survey_round[lastsurvey]
dt$last_surveyage <- dt$AGEATINT_1979 + (dt$last_surveyround - 1979)

# Childless ---------------------------------------------------------------

dt$childless = dt$NUMKID_XRND == 0 

# Entered college or not -----------------------------------------------
educ <- var_list[!is.na(str_extract(var_list, 'HGCREV'))]
educ_df <- dt[,educ]

highest_degree <- apply(educ_df, 1, function(x) max(x, na.rm = T))

dt$college <- highest_degree >= 13 #entered college

# BMI ------------------------------------------------------------------
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

# childbearing at BMI measurement -----------------------------------------

dt$AGEATINT_1981 <- dt$AGEATINT_1979 + 2

# dt$birthbefore_1981_7 <- (dt$`C7DOB~Y_XRND`<=1981 & !is.na(dt$`C7DOB~Y_XRND`))
# dt$birthbefore_1981_6 <- (dt$`C6DOB~Y_XRND`<=1981 & !is.na(dt$`C6DOB~Y_XRND`)) & !dt$birthbefore_1981_7
# dt$birthbefore_1981_5 <- (dt$`C5DOB~Y_XRND`<=1981 & !is.na(dt$`C5DOB~Y_XRND`)) & !dt$birthbefore_1981_6
# dt$birthbefore_1981_4 <- (dt$`C4DOB~Y_XRND`<=1981 & !is.na(dt$`C4DOB~Y_XRND`)) & !dt$birthbefore_1981_5
# dt$birthbefore_1981_4p <- dt$birthbefore_1981_4 | dt$birthbefore_1981_5 | dt$birthbefore_1981_6 | dt$birthbefore_1981_7
# dt$birthbefore_1981_3 <- (dt$`C3DOB~Y_XRND`<=1981 & !is.na(dt$`C3DOB~Y_XRND`)) & (!dt$birthbefore_1981_4p | is.na(dt$`C4DOB~Y_XRND`) | is.na(dt$`C5DOB~Y_XRND`) | is.na(dt$`C6DOB~Y_XRND`) | is.na(dt$`C7DOB~Y_XRND`))
# dt$birthbefore_1981_2 <- (dt$`C2DOB~Y_XRND`<=1981 & !is.na(dt$`C2DOB~Y_XRND`)) & (!dt$birthbefore_1981_3 | is.na(dt$`C3DOB~Y_XRND`)) 
# dt$birthbefore_1981_1 <- (dt$`C1DOB~Y_XRND`<=1981 & !is.na(dt$`C1DOB~Y_XRND`)) & (!dt$birthbefore_1981_2 | is.na(dt$`C2DOB~Y_XRND`))
# 
# dt$birthbefore_1981 <- dt$birthbefore_1981_1 | dt$birthbefore_1981_2 | dt$birthbefore_1981_3 | dt$birthbefore_1981_4p
#note, above is the same is this single line, 
dt$birthbefore_1981 <- (dt$`C1DOB~Y_XRND`<=1981) & !is.na(dt$`C1DOB~Y_XRND`)

#	Q9-69_1980, 1981: HAS R HAD ANY CHILDREN SINCE LAST INT?
# Q9-72_1979: HAS R EVER HAD ANY CHILDREN?
# dt$birthbefore_1981_2 <- dt$`Q9-72_1979` == 1 | dt$`Q9-69_1980` == 1 | dt$`Q9-69_1981` == 1 
# dt$birthbefore_1981_2[dt$childless & (dt$last_surveyage >= dt$AGEATINT_1981)] <- FALSE

bmidf <- dt %>% 
  select(CASEID_1979, 
         childless, college, evermarried, 
         BMI1981, BMI1982, BMI1985, birthbefore_1981,
         AGEATINT_1979, AGEATINT_1981, AGEATINT_1982, AGEATINT_1985, AGE1B_XRND, AGE2B_XRND, AGE3B_XRND, 
         NUMKID82_1982, NUMKID83_1983, NUMKID85_1985, NUMKID86_1986, 
         SAMPLE_ID_1979, SAMPLE_SEX_1979, racenew, MO1M1B_XRND, AGE1M_XRND,
         last_surveyage, last_surveyround,
         residence, residence_withwhom, religion, mother_hgc, father_hgc) %>%
  rename(sex = SAMPLE_SEX_1979)

bmidf1 <- bmidf %>% 
  mutate(
    AGE1B_XRND = ifelse(is.na(AGE1B_XRND), -999, AGE1B_XRND),
    AGE2B_XRND = ifelse(is.na(AGE2B_XRND), -999, AGE2B_XRND),
    AGE3B_XRND = ifelse(is.na(AGE3B_XRND), -999, AGE3B_XRND),
    AGEATINT_1982 = AGEATINT_1979+3,
    AGEATINT_1983 = AGEATINT_1979+4,
    AGEATINT_1985 = AGEATINT_1979+6,
    AGEATINT_1986 = AGEATINT_1979+7,
    NUMKID82_1982 = ifelse(is.na(NUMKID82_1982) & (AGE1B_XRND > 0),
                           ifelse((AGE1B_XRND <= AGEATINT_1982), 1, 0), NUMKID82_1982),
    NUMKID82_1982 = ifelse((AGE1B_XRND <= AGEATINT_1982) & (NUMKID82_1982 == 0) & AGE1B_XRND >0,
                           1,
                           NUMKID82_1982),
    NUMKID82_1982 = ifelse(childless & (last_surveyage >= AGEATINT_1982), 0, NUMKID82_1982),
    NUMKID83_1983 = ifelse(is.na(NUMKID83_1983) & (AGE1B_XRND > 0),
                           ifelse((AGE1B_XRND <= AGEATINT_1983), 1, 0), NUMKID83_1983),
    NUMKID83_1983 = ifelse((AGE1B_XRND <= AGEATINT_1983) & (NUMKID83_1983 == 0) & AGE1B_XRND >0,
                           1,
                           NUMKID83_1983),
    NUMKID83_1983 = ifelse(childless & (last_surveyage >= AGEATINT_1983), 0, NUMKID83_1983),
    NUMKID85_1985 = ifelse(is.na(NUMKID85_1985) & (AGE1B_XRND > 0),
                           ifelse((AGE1B_XRND <= AGEATINT_1985), 1, 0), NUMKID85_1985),
    NUMKID85_1985 = ifelse((AGE1B_XRND <= AGEATINT_1985) & (NUMKID85_1985 == 0) & AGE1B_XRND >0,
                           1,
                           NUMKID85_1985),
    NUMKID85_1985 = ifelse(childless & (last_surveyage >= AGEATINT_1985), 0, NUMKID85_1985),
    NUMKID86_1986 = ifelse(is.na(    NUMKID86_1986) & (AGE1B_XRND > 0),
                           ifelse((AGE1B_XRND <= AGEATINT_1986), 1, 0), NUMKID86_1986),
    NUMKID86_1986 = ifelse((AGE1B_XRND <= AGEATINT_1986) & (NUMKID86_1986 == 0) & AGE1B_XRND >0,
                           1,
                           NUMKID86_1986),
    NUMKID86_1986 = ifelse(childless & (last_surveyage >= AGEATINT_1986), 0, NUMKID86_1986),

    # NUMKID86_1986 = ifelse(is.na(NUMKID86_1986) & !is.na(AGE1B_XRND) & (AGE1B_XRND != -998),
    #                        ifelse(!is.na(AGE1B_XRND) & (AGE1B_XRND <= AGEATINT_1986), 1, 0), NUMKID86_1986),
    # NUMKID86_1986 = ifelse((AGE1B_XRND <= AGEATINT_1986) & (NUMKID86_1986 == 0) & AGE1B_XRND != -998,
    #                        1,
    #                        NUMKID86_1986),
    # NUMKID86_1986 = ifelse(childless & (last_surveyage >= AGEATINT_1986), 0, NUMKID86_1986),
    # birthbefore_1981 = ((AGE1B_XRND <= AGEATINT_1981) & (AGE1B_XRND > 0)) |
    #   ((AGE2B_XRND <= AGEATINT_1981) & (AGE2B_XRND > 0)) |
    #   ((AGE3B_XRND <= AGEATINT_1981) & (AGE3B_XRND > 0)),
    # birthbefore_1982 = AGEATINT_1981 < AGE1B_XRND &
    #   AGE1B_XRND <= AGEATINT_1982,
    birthbefore_1982 = birthbefore_1981 == FALSE & NUMKID82_1982 >0,
    # birthbefore_1985 = AGEATINT_1982 < AGE1B_XRND &
    #   AGE1B_XRND <= AGEATINT_1985,
    birthbefore_1985 = NUMKID82_1982 == 0 & NUMKID85_1985 >0,
    # birthbefore_1986 = AGEATINT_1985 < AGE1B_XRND &
    #   AGE1B_XRND <= AGEATINT_1986,
    birth1982_1983 = NUMKID82_1982 == 0 & NUMKID83_1983 > 0,
    birth1985_1986 = NUMKID85_1985 == 0 & NUMKID86_1986 > 0,
    jokela_omitted = is.na(BMI1981) | last_surveyround == 1981 | birthbefore_1981 | between(AGE1B_XRND, 0, 17)) %>%
  select(-c("AGEATINT_1983", "AGEATINT_1986", "NUMKID82_1982","NUMKID83_1983", "NUMKID85_1985", "NUMKID86_1986")) %>%
  relocate(AGEATINT_1981, .before = AGEATINT_1982) %>%
  relocate(c('birthbefore_1981','birthbefore_1982','birthbefore_1985','birth1985_1986'), .after = AGEATINT_1985) %>%
  mutate(AGEATINT_1979 = as.factor(AGEATINT_1979),
         SAMPLE_ID_1979 = as.factor(SAMPLE_ID_1979)) %>%
  setDT()

saveRDS(bmidf1, './Data/220118_bmidf.rds')

# create bmidf long data --------------------------------------------------

colA = paste("BMI", c(1981,1982,1985), sep = "")
colB = paste("birthbefore_", c(1981,1982,1985), sep = "")
colC = paste("AGEATINT_", c(1981,1982,1985), sep = "")

bmidf1_long <- melt(bmidf1, 
                    measure = list(colA, colB, colC), 
                    value.name = c("BMI", "after_parenthood", "age")) %>%
  arrange(CASEID_1979)

uniq_id <- unique(bmidf1_long$CASEID_1979)
afterparenthood <- bmidf1_long$after_parenthood
wave <- bmidf1_long$variable
birth1982_1983 <- bmidf1_long$birth1982_1983
birth1985_1986 <- bmidf1_long$birth1985_1986

wave1 <- bmidf1_long$variable == 1
wave2 <- bmidf1_long$variable == 2
wave3 <- bmidf1_long$variable == 3

birth_within1 <- rep(FALSE, nrow(bmidf1_long))

for(i in uniq_id){
  
  filt <- bmidf1_long$CASEID_1979 == i
  
  wave1filt <- wave1 & filt
  wave2filt <- wave2 & filt
  wave3filt <- wave3 & filt
  
  if(afterparenthood[wave1filt] == TRUE & 
     !is.na(afterparenthood[wave1filt])){
    
    afterparenthood[wave2filt] <- TRUE
    afterparenthood[wave3filt] <- TRUE
    
  }
  
  if(afterparenthood[wave1filt] == FALSE & 
     !is.na(afterparenthood[wave1filt]) &
     afterparenthood[wave2filt] == TRUE & 
     !is.na(afterparenthood[wave2filt])){
    
    afterparenthood[wave3filt] <- TRUE
    birth_within1[wave1filt] <- TRUE
    
  }
  
  if(afterparenthood[wave2filt] == FALSE & 
     !is.na(afterparenthood[wave2filt]) &
     afterparenthood[wave3filt] == TRUE &
     !is.na(afterparenthood[wave3filt])){
    
    
    
    if(!is.na(unique(birth1982_1983[filt])) &
      unique(birth1982_1983[filt]) == TRUE){
      
      birth_within1[wave2filt] <- TRUE
      
    }
    
    if(!is.na(unique(birth1985_1986[filt])) &
       unique(birth1985_1986[filt]) == TRUE){
      
      birth_within1[wave3filt] <- TRUE 
         
         }
    }
}
  
bmidf1_long$after_parenthood <- afterparenthood
bmidf1_long$birth_within1 <- birth_within1
bmidf1_long$birth_within1[is.na(bmidf1_long$after_parenthood) |
                          (is.na(bmidf1_long$birth1985_1986) & bmidf1_long$variable == 3)] <- NA

bmidf1_long <- bmidf1_long %>%
  mutate(transition = ifelse(after_parenthood == FALSE & birth_within1 == FALSE, 'before',
                             ifelse(after_parenthood == FALSE & birth_within1 == TRUE,
                                    'intransit',
                                    ifelse(after_parenthood == TRUE & birth_within1 == FALSE,
                                           'after', NA))))

saveRDS(bmidf1_long,'./Data/220118_bmidf_long.rds')

# BMI known before parenthood? --------------------------------------------

id <- bmidf1_long$CASEID_1979
bmi <- bmidf1_long$BMI
uniq_id <- unique(id)
transition <- bmidf1_long$transition
before_parenthood <- !bmidf1_long$after_parenthood
bmi <- bmidf1_long$BMI
age <- bmidf1_long$age
sex <- bmidf1_long$sex

BMI_known <- rep(NA, length(uniq_id))

for(i in uniq_id){
  
  filt <- id %in% i
  cur_sex <- unique(sex[filt])
  
  if(cur_sex == 1){ 
    BMI_known[i] <- any(before_parenthood[filt] & !is.na(bmi[filt]))
  }
  
  else{  #in women, before parenthood means before pregnancy
    BMI_known[i] <- any(transition[filt] %in% 'before' & (!is.na(bmi[filt])))
  }
  
}

#bring the info to bmidf1
bmidf1$BMI_known_beforeparenthood <- BMI_known
bmidf1$BMI_none <- is.na(bmidf1$BMI1981) & is.na(bmidf1$BMI1982) & is.na(bmidf1$BMI1985)

saveRDS(bmidf1, './Data/220118_bmidf.rds')
