##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Regression analysis modelling
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-19
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)
require(readr)
require(geobr)
require(ggplot2)
require(rgeos)
require(classInt)
require(gridExtra)
require(spdep)
require(car)
require(xlsx)

options( scipen = 999 )
##################################################################

### 2. Read data #------------------------------------------------

# 2.1 votes and demographics
datVote <- 
  fread( 
    'DATA/demographic_elections_02_06_10_14_18_data.csv',
    dec = ','
  )

# 2.2 compute Totals and % of vote and scale pop for vote data
datVote[ ,
         `:=`(
           total10 = brancos10 + nulos10 + uteis10,
           total14 = brancos14 + nulos14 + uteis14,
           total18 = brancos18 + nulos18 + uteis18
         )
         ]

datVote[ ,
         `:=`(
           brancos10.p = brancos10 / total10,
           nulos10.p   = nulos10 / total10,
           pt10.p      = pt10 / uteis10,
           brancos14.p = brancos14 / total14,
           nulos14.p   = nulos14 / total14,
           pt14.p      = pt14 / uteis14,
           brancos18.p = brancos18 / total18,
           nulos18.p   = nulos18 / total18,
           pt18.p      = pt18 / uteis18,
           psl18.p    = psl18 / uteis18
         )]


datVote$pop.scale = datVote$pop/1000000

summary_dt <-
  rbind(
    datVote[ , lapply( .SD, function(x) max( x ) ),
             .SDcols = names(datVote)[7:ncol(datVote)] ] %>%
      .[ , type := 'max' ],
    datVote[ , lapply( .SD, function(x) min( x ) ),
             .SDcols = names(datVote)[7:ncol(datVote) ] ] %>%
      .[ , type := 'min' ],
    datVote[ , lapply( .SD, function(x) mean( x ) ),
             .SDcols = names(datVote)[7:ncol(datVote) ] ] %>%
      .[ , type := 'mean' ],
    datVote[ , lapply( .SD, function(x) median( x ) ),
             .SDcols = names(datVote)[7:ncol(datVote) ] ] %>%
      .[ , type := 'median' ],
    datVote[ , lapply( .SD, function(x) quantile( x, p = 1/4 ) ),
             .SDcols = names(datVote)[7:ncol(datVote) ] ] %>%
      .[ , type := '1st quartile' ],
    datVote[ , lapply( .SD, function(x) quantile( x, p = 3/4 ) ),
             .SDcols = names(datVote)[7:ncol(datVote) ] ] %>%
      .[ , type := '3rd quartile' ]
  ) %>% as.data.frame

row.names(summary_dt) = summary_dt$type

write.xlsx(
  summary_dt[, -ncol(summary_dt)],
  file = 'OUTPUTS/summary_stats_dataset.xlsx',
  sheetName = 'summary',
  row.names = TRUE,
  append = FALSE
)

# 2.4 microrregions shape files
microMapDat <- 
  read_micro_region(  year = 2013 )

# 2.4 state shape files
stateMapDat <- 
  read_state(  year = 2010 )

# 2.5 extract microregion map centroids
microMap.centroids <-
  fread( 'DATA/coord_centroids.csv' )

datVote <- 
  merge(
    datVote,
    microMap.centroids,
    by.x = 'microcode',
    by.y = 'CODMICRO'
  )

# 2.6 merge map and vote data
map.dat <- 
  merge(
    microMapDat,
    datVote,
    by.x = 'code_micro',
    by.y = 'microcode'
  )


# 2.6 neighborhood  structure
neigh.micro.r <-
  poly2nb(
    map.dat,
    queen = F
    ) 

listw.map <- 
  nb2listw(
    neigh.micro.r,
    style = "B", 
    zero.policy = TRUE
    )
####################################################################

### 3. TFR ~ PSL #---------------------------

# 3.1 Vif tests

vif_test <- 
  vif(
    lm(
      TFR ~ 
        psl18.p + brancos18.p + nulos18.p + 
        pop.scale + pbf + 
        educSecd.fem + religPent + depRatio.elder + 
        womenHead + share_adolbirths_2010 + 
        x + y,
      data = map.dat
    )
  ) %>% 
  as.data.frame 

names(vif_test) = c( 'VIF' )
vif_test$var <- row.names(vif_test)
row.names(vif_test) = NULL

write.xlsx(
  vif_test,
  file = 'OUTPUTS/TFR_PSL_Models.xlsx',
  sheetName = 'vif_test',
  row.names = FALSE,
  append = FALSE
)

# 3.2 - 1) TFR ~ PSL + NULOS + BRANCOS + POP.SCALE
mod1 <- 
  lm(
    TFR ~ 
      psl18.p + brancos18.p + nulos18.p + pop.scale,
    data = map.dat
  )

resmod1 <- 
  summary( mod1 )

res_betas <- 
  resmod1$coefficients %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod1$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]

res_betas$r2 <- resmod1$r.squared %>% round( 5 )

write.xlsx(
  res_betas,
  file = 'OUTPUTS/TFR_PSL_Models.xlsx',
  sheetName = 'model1_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.3 - 2) TFR ~ PSL + (1) + pbf + educSecd.fem + religPent + depRatio.elder
mod2 <- 
  lm(
    TFR  ~ 
      psl18.p + brancos18.p + nulos18.p + pop.scale +
      pbf + educSecd.fem + religPent + depRatio.elder,
    data = map.dat
  )

resmod2 <- 
  summary( mod2 )

res_betas <- 
  resmod2$coefficients %>% 
  as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod2$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]


res_betas$r2 <- resmod2$r.squared %>% round( 5 )

write.xlsx(
  res_betas,
  file = 'OUTPUTS/TFR_PSL_Models.xlsx',
  sheetName = 'model2_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.4 - 3) TFR ~ PSL + (1) + pbf + educSecd.fem + religPent + depRatio.elder + womenHead + shareadolbirths
mod3 <- 
  lm(
    TFR  ~ 
      psl18.p + brancos18.p + nulos18.p + pop.scale +
      pbf + educSecd.fem + religPent + depRatio.elder +
      womenHead + share_adolbirths_2010,
    data = map.dat
  )

resmod3 <- 
  summary( mod3 )

res_betas <- 
  resmod3$coefficients %>% 
  as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod3$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]


res_betas$r2 <- resmod3$r.squared %>% round( 5 )

write.xlsx(
  res_betas,
  file = 'OUTPUTS/TFR_PSL_Models.xlsx',
  sheetName = 'model3_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.5 - 4) TFR ~ PSL + (2) + spatial
# 
# summary(
#   lm.LMtests(
#     lm(
#       TFR ~ 
#         psl18.p + brancos18.p + nulos18.p + 
#         pop.scale + pbf +  educSecd.fem + religPent + depRatio.elder +
#         x + y,
#       data = map.dat
#     ),
#     listw.map, 
#     test = c( "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA" ),
#     zero.policy = TRUE
#   )
# )

mod4 <- 
  errorsarlm(
    as.vector( TFR ) ~
      psl18.p + brancos18.p + nulos18.p + pop.scale + 
      pbf + educSecd.fem + religPent + depRatio.elder +
      x + y ,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
  )

resmod4 <- 
  summary( mod4 )

res_betas <- 
  resmod4$Coef %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod4$Coef ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|z|)` %>% round( 5 ) ) ]
  
write.xlsx(
  res_betas,
  file = 'OUTPUTS/TFR_PSL_Models.xlsx',
  sheetName = 'model4_spatial_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.6 - 5) (4) + share births and headWomen

mod5 <- 
  errorsarlm(
    as.vector( TFR ) ~
      psl18.p + brancos18.p + nulos18.p + pop.scale + 
      pbf + educSecd.fem + religPent + depRatio.elder +
      womenHead + share_adolbirths_2010 +
      x + y ,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
  )

resmod5 <- 
  summary( mod5 )

res_betas <- 
  resmod5$Coef %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod5$Coef ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|z|)` %>% round( 5 ) ) ]

write.xlsx(
  res_betas,
  file = 'OUTPUTS/TFR_PSL_Models.xlsx',
  sheetName = 'model5_spatial_betas',
  row.names = FALSE,
  append = TRUE
)
####################################################################

### 4. PSL ~ TFR #---------------------------

# 4.1 Vif tests

vif_test <- 
  vif(
    lm(
      psl18.p ~ 
        TFR + brancos18.p + nulos18.p + 
        pop.scale + pbf + 
        educSecd.fem + religPent + depRatio.elder + 
        womenHead + share_adolbirths_2010 + 
        x + y,
      data = map.dat
    )
  ) %>% 
  as.data.frame 

names(vif_test) = c( 'VIF' )
vif_test$var <- row.names(vif_test)
row.names(vif_test) = NULL

write.xlsx(
  vif_test,
  file = 'OUTPUTS/PSL_TFR_Models.xlsx',
  sheetName = 'vif_test',
  row.names = FALSE,
  append = FALSE
)

# 3.2 - 1) TFR ~ PSL + NULOS + BRANCOS + POP.SCALE
mod1 <- 
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + pop.scale,
    data = map.dat
  )

resmod1 <- 
  summary( mod1 )

res_betas <- 
  resmod1$coefficients %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod1$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]

res_betas$r2 <- resmod1$r.squared %>% round( 5 )

write.xlsx(
  res_betas,
  file = 'OUTPUTS/PSL_TFR_Models.xlsx',
  sheetName = 'model1_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.3 - 2) TFR ~ PSL + (1) + pbf + educSecd.fem + religPent + depRatio.elder
mod2 <- 
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + pop.scale +
      pbf + educSecd.fem + religPent + depRatio.elder,
    data = map.dat
  )

resmod2 <- 
  summary( mod2 )

res_betas <- 
  resmod2$coefficients %>% 
  as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod2$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]


res_betas$r2 <- resmod2$r.squared %>% round( 5 )

write.xlsx(
  res_betas,
  file = 'OUTPUTS/PSL_TFR_Models.xlsx',
  sheetName = 'model2_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.4 - 3) TFR ~ PSL + (1) + pbf + educSecd.fem + religPent + depRatio.elder + womenHead + shareadolbirths
mod3 <- 
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + pop.scale +
      pbf + educSecd.fem + religPent + depRatio.elder +
      womenHead + share_adolbirths_2010,
    data = map.dat
  )

resmod3 <- 
  summary( mod3 )

res_betas <- 
  resmod3$coefficients %>% 
  as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod3$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]


res_betas$r2 <- resmod3$r.squared %>% round( 5 )

write.xlsx(
  res_betas,
  file = 'OUTPUTS/PSL_TFR_Models.xlsx',
  sheetName = 'model3_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.5 - 4) TFR ~ PSL + (2) + spatial
# 
# summary(
#   lm.LMtests(
#     lm(
#       TFR ~ 
#         psl18.p + brancos18.p + nulos18.p + 
#         pop.scale + pbf +  educSecd.fem + religPent + depRatio.elder +
#         x + y,
#       data = map.dat
#     ),
#     listw.map, 
#     test = c( "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA" ),
#     zero.policy = TRUE
#   )
# )

mod4 <- 
  errorsarlm(
    as.vector( psl18.p ) ~ 
      TFR + brancos18.p + nulos18.p + pop.scale + 
      pbf + educSecd.fem + religPent + depRatio.elder +
      x + y ,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
  )

resmod4 <- 
  summary( mod4 )

res_betas <- 
  resmod4$Coef %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod4$Coef ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|z|)` %>% round( 5 ) ) ]

write.xlsx(
  res_betas,
  file = 'OUTPUTS/PSL_TFR_Models.xlsx',
  sheetName = 'model4_spatial_betas',
  row.names = FALSE,
  append = TRUE
)

# 3.6 - 5) (4) + share births and headWomen

mod5 <- 
  errorsarlm(
    as.vector( psl18.p ) ~
      TFR + brancos18.p + nulos18.p + pop.scale + 
      pbf + educSecd.fem + religPent + depRatio.elder +
      womenHead + share_adolbirths_2010 +
      x + y ,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
  )

resmod5 <- 
  summary( mod5 )

res_betas <- 
  resmod5$Coef %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod5$Coef ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|z|)` %>% round( 5 ) ) ]

write.xlsx(
  res_betas,
  file = 'OUTPUTS/PSL_TFR_Models.xlsx',
  sheetName = 'model5_spatial_betas',
  row.names = FALSE,
  append = TRUE
)
####################################################################



