dt <- readRDS("U:/cloud/Codes/BMI_fertility/Data/NLSY79_bmifertility_nosupp.RDS")

library(stringr)

var_list <- names(dt)

miscar <- var_list[!is.na(str_extract(var_list, 'ABORTS'))]

miscar_df <- dt[,miscar]

miscarried <- apply(miscar_df, 1, function(x) max(x, na.rm = T))
miscarried[is.infinite(miscarried)] <- NA
evermiscarried <- miscarried != 0

dt$everaborted <- evermiscarried

x <- table(dt$everaborted, dt$BMIcat, dt$evermarried)[,,1]
y <- table(dt$everaborted, dt$BMIcat, dt$evermarried)[,,2]

prop.table(x, 2)
prop.table(y, 2)
