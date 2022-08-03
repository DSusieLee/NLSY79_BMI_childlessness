library(stringr)
library(tidyverse)

# Reading in datasets - combine ----------------------------------------

dt1  <- readRDS("./Data/NLSY79_bmifertility_201013_ORIGINAL.RDS")
dt2 <- readRDS("./Data/NLSY79_bmifertility_1pwanted_ageinterview.RDS")
dt3 <- as.data.frame(read_csv("./Data/210220_race_details.csv"))

#race:
names(dt3)[2] <- 'racecode'

#combine all data frames:
dt_new <- cbind(dt1, dt2, dt3)
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

# Race --------------------------------------------------------------------

dt$race <- dt$SAMPLE_RACE_78SCRN
#1: non-black non-hispanic; 2: black; 3: hispanic

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


# Making bmidf ------------------------------------------------------------

bmidf <- dt %>% 
  select(SAMPLE_ID_1979,
         BMI1981, BMI1982, BMI1985, 
         AGEATINT_1979, AGEATINT_1982, AGEATINT_1985, AGE1B_XRND, 
         NUMKID82_1982, NUMKID85_1985, 
         SAMPLE_SEX_1979, race) %>%
  mutate(ID = c(1:nrow(dt)))

#all samples
bmidf1 <- bmidf %>% 
  mutate(
    AGEATINT_1981 = AGEATINT_1979+2,
    AGEATINT_1982 = ifelse(is.na(AGEATINT_1982), AGEATINT_1979+3, AGEATINT_1982),
    NUMKID82_1982 = ifelse((AGE1B_XRND <= AGEATINT_1982) & (NUMKID82_1982 == 0) & AGE1B_XRND != -998,
                               1,
                               NUMKID82_1982),
    NUMKID82_1982 = ifelse(is.na(NUMKID82_1982) & (AGE1B_XRND != -998),
                           ifelse(AGE1B_XRND <= AGEATINT_1982, 1, 0), NUMKID82_1982),
    AGEATINT_1985 = ifelse(is.na(AGEATINT_1985), AGEATINT_1979+6, AGEATINT_1985),
    NUMKID85_1985 = ifelse((AGE1B_XRND <= AGEATINT_1985) & (NUMKID85_1985 == 0) & AGE1B_XRND != -998,
                            1,
                            NUMKID85_1985),
    NUMKID85_1985 = ifelse(is.na(NUMKID85_1985) & (AGE1B_XRND != -998),
                           ifelse(AGE1B_XRND <= AGEATINT_1985, 1, 0), NUMKID85_1985)) %>%
  select(-AGEATINT_1979) %>%
  relocate(c("ID","SAMPLE_SEX_1979","race"), .before = SAMPLE_ID_1979)

#only those whose BMI is available BOTH before and after childbearing
bmidf2 <- bmidf1 %>% 
  filter((NUMKID82_1982 > 0 | NUMKID85_1985 > 0) &
           AGE1B_XRND > AGEATINT_1979) %>%
  filter(AGE1B_XRND > AGEATINT_1981 & AGE1B_XRND <= AGEATINT_1985) %>%
  relocate(AGEATINT_1981, .after = AGEATINT_1979) %>%
  mutate(btw12 = AGEATINT_1981 < AGE1B_XRND &
           AGE1B_XRND <= AGEATINT_1982,
         btw23 = AGEATINT_1982 < AGE1B_XRND &
           AGE1B_XRND <= AGEATINT_1985) %>% #1981이랑 1982 사이냐, 1982이랑 1985 사이냐. 각 경우 닫는 년도와는 같아도 되지만 시작하는 년도보다는 커야 함.
  relocate(btw12, .after = BMI1985) %>%
  relocate(btw23, .after = btw12)


# bmidf1 ------------------------------------------------------------------


