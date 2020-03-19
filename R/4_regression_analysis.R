##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Regression analysis modelling
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-18
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

##################################################################

### 2. Read data #------------------------------------------------

# 2.1 votes and demographics
datVote <- 
  fread( 
    'DATA/demographic_elections_10_14_18_data.csv',
    dec = ','
  )

datcomp <- 
  fread( 
    'DATA/dados_completo.csv',
    dec = ','
  )

datcomp$renda %>% as.numeric %>% summary
datVote$meanInc %>% as.numeric %>% summary

?fread
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

### 4. Check correlation and vif tests #---------------------------

# 4.1 Vif tests

vif(
  lm(
    TFR ~ 
      pt10.p + brancos10.p + nulos10.p + log( meanInc ) + educSecd.fem + religPent + x + y,
    weights = pop.scale,
    data = datVote
  )
)

vif(
  lm(
    pt10.p ~ 
      TFR + brancos10.p + nulos10.p + log( meanInc ) + educSecd.fem + religPent + depRatio.elder + sexRatio + x + y,
    weights = pop.scale,
    data = datVote
  )
)


# 4.2 LM tests
summary(
  lm.LMtests(
    lm(
      pt10.p ~ 
        TFR + brancos10.p + nulos10.p + log( meanInc ) + educSecd.fem + religPent + depRatio.elder + sexRatio + x + y,
      weights = pop.scale,
      data = map.dat
      ),
    listw.map, 
    test = c( "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA" ),
    zero.policy = TRUE
    )
  )

# 4.3 Models - 1. no spatial, 2. spatial large scale, 3. spatial small scale

mod1a.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + religPent ,
    weights = pop.scale,
    data    = map.dat
    )

mod1b.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + sexRatio + depRatio.elder + religPent,
    weights = pop.scale,
    data    = map.dat
  )

mod1c.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + sexRatio + depRatio.elder + religPent + qAdult.mal,
    weights = pop.scale,
    data    = map.dat
  )

AIC(mod1a.psl)
AIC(mod1b.psl)
AIC(mod1c.psl)

mod2a.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + religPent + x + y ,
    weights = pop.scale,
    data    = map.dat
  )

mod2b.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + sexRatio + depRatio.elder + religPent + x + y ,
    weights = pop.scale,
    data    = map.dat
  )

mod2c.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + sexRatio + depRatio.elder + religPent + 
      qAdult.mal + x + y ,
    weights = pop.scale,
    data    = map.dat
  )

mod2d.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + pbf + educSecd.fem + religPent + x + y ,
    weights = pop.scale,
    data    = map.dat
  )

mod2e.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + pbf + educSecd.fem + sexRatio + depRatio.elder + religPent + x + y ,
    weights = pop.scale,
    data    = map.dat
  )

mod2f.psl <- 
  lm(
    psl18.p ~
      TFR + brancos18.p + nulos18.p + pbf + educSecd.fem + sexRatio + depRatio.elder + religPent + 
      qAdult.mal + x + y ,
    weights = pop.scale,
    data    = map.dat
  )

AIC(mod2a.psl)
vif(mod2b.psl)
summary(mod2c.psl)
AIC(mod2d.psl)
vif(mod2e.psl)
summary(mod2f.psl)


vif(
  lm(
    psl18.p ~ 
      TFR + brancos18.p + nulos18.p + log( meanInc ) + educSecd.fem + sexRatio + depRatio.elder  + religPent + x + y ,
    weights = pop.scale,
    data    = map.dat
  ))
# spatial trend
summary(lm(TFT~psl18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(TFT)~psl18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=mapa.1))  #Fitting a SAR-Model#

summary(
  errorsarlm(
    as.vector(pt10.p) ~
      TFR + log( meanInc ) + educSecd.fem + religPent  + sexRatio + x + y,
    listw       = listw.map, 
    zero.policy = TRUE, 
    data        = map.dat
    )
  )  #Fitting a SAR-Model#


summary(errorsarlm(as.vector(pt10.p)~brancos10.p + nulos10.p + propW25_34 + log( meanInc ) + educSecd.fem +
                     religPent + x + y,listw=listw.map, zero.policy=TRUE, weights = pop.scale, data=datVote))  #Fitting a SAR-Model#




################################################
######### Constru??o dos modelos ###############
################################################

# mod.1 = no control for spatial trend
# mod.2 = control for spatial trend, large scale
# mod.3 = control for spatial autocorrelation, small scale

## Modelos para o PSL
## general controls
summary(lm(TFT~psl18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(TFT~psl18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(TFT)~psl18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=mapa.1))  #Fitting a SAR-Model#

## Modelos para o PT em 2018
## general controls
summary(lm(TFT~pt18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(TFT~pt18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(TFT)~pt18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=mapa.1))  #Fitting a SAR-Model#

## Modelos para o PT em 2014
## general controls
summary(lm(TFT~pt14+brancos14+nulos14+pop.1+propW25_34+log(renda)+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(TFT~pt14+brancos14+nulos14+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(TFT)~pt14+brancos14+nulos14+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=mapa.1))  #Fitting a SAR-Model#

## Modelos para o PT em 2010
## general controls
summary(lm(TFT~pt10+brancos10+nulos10+pop.1+propW25_34+log(renda)+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(TFT~pt10+brancos10+nulos10+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(TFT)~pt10+brancos10+nulos10+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=mapa.1))  #Fitting a SAR-Model#

### Invertendo a rela??o -- voto como resposta ####
## Modelos para o PSL
## general controls
summary(lm(psl18~brancos18+nulos18+TFT+pop.1+pbf+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(psl18~brancos18+nulos18+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(psl18)~brancos18+nulos18+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=dat))  #Fitting a SAR-Model#

## Modelos para o PT em 2018
## general controls
summary(lm(pt18~brancos18+nulos18+TFT+pop.1+pbf+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(pt18~brancos18+nulos18+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(pt18)~brancos18+nulos18+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=dat))  #Fitting a SAR-Model#

## Modelos para o PT em 2014
## general controls
summary(lm(pt14~brancos14+nulos14+TFT+pop.1+pbf+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(pt14~brancos14+nulos14+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(pt14)~brancos14+nulos14+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=dat))  #Fitting a SAR-Model#

## Modelos para o PT em 2010
## general controls
summary(lm(pt10~brancos10+nulos10+TFT+pop.1+pbf+edusec+relpent,data=mapa.1))

# spatial trend
summary(lm(pt10~brancos10+nulos10+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1))

## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(pt10)~brancos10+nulos10+TFT+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],listw=listw.map, zero.policy=TRUE, data=dat))  #Fitting a SAR-Model#


