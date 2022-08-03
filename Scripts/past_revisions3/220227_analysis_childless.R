library(tidyverse)
library(rstanarm)
library(loo)

SEED = 52421

###
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19041)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United Kingdom.1252
# [2] LC_CTYPE=English_United Kingdom.1252
# [3] LC_MONETARY=English_United Kingdom.1252
# [4] LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.1252
# system code page: 949
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets
# [6] methods   base
# 
# other attached packages:
#   [1] loo_2.4.1       rstanarm_2.21.1 Rcpp_1.0.8
# [4] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7
# [7] purrr_0.3.4     readr_2.1.1     tidyr_1.1.4
# [10] tibble_3.1.6    ggplot2_3.3.5   tidyverse_1.3.1
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-153         matrixStats_0.61.0
# [3] fs_1.5.1             xts_0.12.1
# [5] lubridate_1.8.0      threejs_0.3.3
# [7] httr_1.4.2           rstan_2.21.2
# [9] tools_4.1.2          backports_1.3.0
# [11] utf8_1.2.2           R6_2.5.1
# [13] DT_0.20              DBI_1.1.1
# [15] colorspace_2.0-2     withr_2.4.3
# [17] tidyselect_1.1.1     gridExtra_2.3
# [19] prettyunits_1.1.1    processx_3.5.2
# [21] curl_4.3.2           compiler_4.1.2
# [23] cli_3.1.0            rvest_1.0.2
# [25] xml2_1.3.3           shinyjs_2.0.0
# [27] colourpicker_1.1.1   checkmate_2.0.0
# [29] scales_1.1.1         dygraphs_1.1.1.6
# [31] ggridges_0.5.3       callr_3.7.0
# [33] StanHeaders_2.21.0-7 digest_0.6.29
# [35] minqa_1.2.4          base64enc_0.1-3
# [37] pkgconfig_2.0.3      htmltools_0.5.2
# [39] lme4_1.1-27.1        dbplyr_2.1.1
# [41] fastmap_1.1.0        htmlwidgets_1.5.4
# [43] rlang_0.4.12         readxl_1.3.1
# [45] rstudioapi_0.13      shiny_1.7.1
# [47] generics_0.1.1       zoo_1.8-9
# [49] jsonlite_1.7.2       crosstalk_1.2.0
# [51] gtools_3.9.2         inline_0.3.19
# [53] magrittr_2.0.1       bayesplot_1.8.1
# [55] Matrix_1.3-4         munsell_0.5.0
# [57] fansi_0.5.0          lifecycle_1.0.1
# [59] stringi_1.6.1        MASS_7.3-54
# [61] pkgbuild_1.2.1       plyr_1.8.6
# [63] grid_4.1.2           parallel_4.1.2
# [65] promises_1.2.0.1     crayon_1.4.2
# [67] miniUI_0.1.1.1       lattice_0.20-45
# [69] splines_4.1.2        haven_2.4.3
# [71] hms_1.1.1            ps_1.6.0
# [73] pillar_1.6.4         igraph_1.2.9
# [75] boot_1.3-28          markdown_1.1
# [77] shinystan_2.5.0      codetools_0.2-18
# [79] reshape2_1.4.4       stats4_4.1.2
# [81] rstantools_2.1.1     reprex_2.0.1
# [83] glue_1.5.1           V8_3.6.0
# [85] RcppParallel_5.1.4   modelr_0.1.8
# [87] nloptr_1.2.2.3       vctrs_0.3.8
# [89] tzdb_0.2.0           httpuv_1.6.3
# [91] cellranger_1.1.0     gtable_0.3.0
# [93] assertthat_0.2.1     mime_0.12
# [95] xtable_1.8-4         broom_0.7.10
# [97] later_1.3.0          survival_3.2-13
# [99] rsconnect_0.8.25     shinythemes_1.2.0
# [101] ellipsis_0.3.2


# data prep ---------------------------------------------------------------

df <- readRDS('./Data/220118_df.rds') %>% 
  filter(!(racenew %in% "Other") &
           last_surveyage>=40 &
         !is.na(childless)) %>%
  mutate(childless = as.factor(childless),
         bcohort = as.factor(AGEATINT_1979))

dm <- readRDS('./Data/220118_dm.rds') %>% 
  filter(!(racenew %in% "Other") &
           last_surveyage>=40 &
         !is.na(childless)) %>%
  mutate(childless = as.factor(childless),
         bcohort = as.factor(AGEATINT_1979))

# models: women ------------------------------------------------------------------

post1 <- stan_glm(childless ~ BMI_pred_cat, 
                  data = df,
                  family = binomial(link = "logit"), 
                  iter = 5000,
                  seed = SEED)

post2 <- update(post1, .~. + racenew)
post3 <- update(post1, .~. + BMI_pred_cat*racenew)
post4 <- update(post3, .~. + bcohort)

loo_compare(loo(post1, cores = 2),
            loo(post2, cores = 2),
            loo(post3, cores = 2))
# elpd_diff se_diff
# post3  0.0       0.0   
# post2  0.0       4.1   
# post1 -8.0       6.1    

saveRDS(post1, './Results/220227_stanglm_women_onlybmi.rds')
saveRDS(post3, './Results/220227_stanglm_women.rds')
saveRDS(post4, './Results/220227_stanglm_women_cohortadded.rds')

# models: men ---------------------------------------------------------------------

post11 <- stan_glm(childless ~ BMI_pred_cat, 
                  data = dm,
                  family = binomial(link = "logit"), 
                  iter = 5000,
                  seed = SEED)

post21 <- update(post11, .~. + racenew)
post31 <- update(post11, .~. + BMI_pred_cat*racenew)
post41 <- update(post31, .~. + bcohort)

loo_compare(loo(post11, cores = 2),
            loo(post21, cores = 2),
            loo(post31, cores = 2))

saveRDS(post11, './Results/220227_stanglm_men_onlybmi.rds')
saveRDS(post31, './Results/220227_stanglm_men.rds')
saveRDS(post41, './Results/220227_stanglm_men_cohortadded.rds')

# elpd_diff se_diff
# post21  0.0       0.0   
# post11 -0.8       2.4   
# post31 -6.0       3.6   