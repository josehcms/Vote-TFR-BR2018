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

options( scipen = 999 )
##################################################################

### 2. Read data #------------------------------------------------

# 2.1 votes and demographics
datVote <- 
  fread( 
    'DATA/demographic_elections_10_14_18_data.csv',
    dec = ','
  )

head( datVote )

# 2.2 microrregions shape files
microMapDat <- 
  read_micro_region(  year = 2013 )

# 2.3 state shape files
stateMapDat <- 
  read_state(  year = 2010 )

# 2.4 extract microregion map centroids
microMap.centroids <-
  fread( 'DATA/coord_centroids.csv' )


datVote <- 
  merge(
    datVote,
    microMap.centroids,
    by.x = 'microcode',
    by.y = 'CODMICRO'
  )

# 2.5 compute Totals and % of vote and scale pop for vote data
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
map.dat$pop.scale = map.dat$pop/1000000

#2.6 merge map and vote data
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

### 3. Check correlation and vif tests #---------------------------

# 3.1 Vif tests

vif(
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + 
      pop.scale + pbf + 
      educSecd.fem + religPent + depRatio.elder + 
      x + y,
    data = map.dat
  )
)

# 3.2 - 1) PSL ~ TRF + NULOS + BRANCOS + POP.SCALE
mod1 <- 
  lm(
    TFR ~ 
      psl18.p + brancos18.p + nulos18.p + pop.scale,
    data = map.dat
  )

plot( mod1 )

resmod1 <- 
  summary( mod1 )

resmod1$coefficients %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod1$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]

resmod1$r.squared %>% round( 5 )

# 3.3 - 2) PSL ~ (1) + pbf + educSecd.fem + religPent + depRatio.elder
mod2 <- 
  lm(
    TFR  ~ 
      psl18.p + brancos18.p + nulos18.p + pop.scale +
      pbf + educSecd.fem + religPent + depRatio.elder,
    data = map.dat
  )

plot( mod2 )

resmod2 <- 
  summary( mod2 )

resmod2$coefficients %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod2$coefficients ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|t|)` %>% round( 5 ) ) ]

resmod2$r.squared %>% round( 5 )


# 3.4 - 3) PSL ~ (2) + spatial

summary(
  lm.LMtests(
    lm(
      TFR ~ 
        psl18.p + brancos18.p + nulos18.p + 
        pop.scale + pbf +  educSecd.fem + religPent + depRatio.elder +
        x + y,
      data = map.dat
    ),
    listw.map, 
    test = c( "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA" ),
    zero.policy = TRUE
  )
)

mod3 <- 
  errorsarlm(
    as.vector( TFR ) ~
      psl18.p + brancos18.p + nulos18.p + pop.scale + 
      pbf + educSecd.fem + religPent + depRatio.elder +
      x + y ,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
  )

plot( mod3 )

resmod3 <- 
  summary( mod3 )

resmod3$Coef %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod3$Coef ),
    Estimate = Estimate %>% round( 5 ), 
    Pval     = `Pr(>|z|)` %>% round( 5 ) ) ]
  


####################################################################

### 4 Test PSL ~ VOTE #---------------------------------------------

# 4.1 Vif tests

vif(
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + 
      pop.scale + pbf + 
      educSecd.fem + religPent + depRatio.elder + 
      x + y,
    data = map.dat
  )
)

# 3.2 - 1) PSL ~ TRF + NULOS + BRANCOS + POP.SCALE
mod1 <- 
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + pop.scale,
    data = map.dat
  )

plot( mod1 )

resmod1 <- 
  summary( mod1 )

resmod1$coefficients %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod1$coefficients ),
    Estimate = Estimate %>% round( 3 ), 
    Pval     = `Pr(>|t|)` %>% round( 4 ) ) ]

resmod1$r.squared %>% round( 5 )

# 3.3 - 2) PSL ~ (1) + pbf + educSecd.fem + religPent + depRatio.elder
mod2 <- 
  lm(
    psl18.p  ~ 
      TFR + brancos18.p + nulos18.p + pop.scale +
      pbf + educSecd.fem + religPent + depRatio.elder,
    data = map.dat
  )

plot( mod2 )

resmod2 <- 
  summary( mod2 )

resmod2$coefficients %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod2$coefficients ),
    Estimate = Estimate %>% round( 3 ), 
    Pval     = `Pr(>|t|)` %>% round( 4 ) ) ]

resmod2$r.squared %>% round( 5 )


# 3.4 - 3) PSL ~ (2) + spatial

summary(
  lm.LMtests(
    lm(
      psl18.p ~ 
        TFR + brancos18.p + nulos18.p + 
        pop.scale + pbf +  educSecd.fem + religPent + depRatio.elder +
        x + y,
      data = map.dat
    ),
    listw.map, 
    test = c( "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA" ),
    zero.policy = TRUE
  )
)

mod3 <- 
  errorsarlm(
    as.vector( psl18.p ) ~
      TFR + brancos18.p + nulos18.p + pop.scale + 
      pbf + educSecd.fem + religPent + depRatio.elder +
      x + y ,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
  )

plot( mod3 )

resmod3 <- 
  summary( mod3 )

resmod3$Coef %>% as.data.table %>%
  .[ , .( 
    Var      = row.names( resmod3$Coef ),
    Estimate = Estimate %>% round( 3 ), 
    Pval     = `Pr(>|z|)` %>% round( 4 ) ) ]



####################################################################



