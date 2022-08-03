
dt <- readRDS('NLSY79_bmifertility_simple.RDS')
dtt <- subset(dt, 
              SAMPLE_ID_1979 <=8)


# childless ---------------------------------------------------------------

table(dtt$NUMKID_XRND, dtt$SAMPLE_SEX_1979)


