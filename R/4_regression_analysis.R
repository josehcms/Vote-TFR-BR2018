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

#2.5 merge map and vote data
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

### 3. Compute vote % #---------------------------------------------

# 3.1 Totals
datVote[ ,
         `:=`(
           total10 = brancos10 + nulos10 + uteis10,
           total14 = brancos14 + nulos14 + uteis14,
           total18 = brancos18 + nulos18 + uteis18
         )
         ]

# 3.2 Compute vote %
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
           pt18.p      = pt18 / uteis18
         )]
####################################################################

### 4. Check correlation and vif tests #---------------------------

datVote$pop.scale = datVote$pop/1000000
map.dat$pop.scale = map.dat$pop/1000000

summary(
  lm.LMtests(
    lm(
      TFR ~ pt10 + brancos10 + nulos10 + pop.scale + propW25_34 + log( meanInc ) + educSecd.fem +
        religPent + x + y,
      weights = pop.scale,
      data = map.dat
      ),
    listw.map, 
    test = c( "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA" ),
    zero.policy = TRUE
    )
  )

## Modelos para o PT em 2010
## general controls
summary(lm(
  TFR ~ pt10 + brancos10 + nulos10 + propW25_34 + log( meanInc ) + educSecd.fem +
    religPent + x + y,
  weights = pop.scale,
  data = map.dat
))

summary(lm(
  pt10.p ~ TFR + brancos10.p + nulos10.p + pop.scale + propW25_34 + log( meanInc ) + educSecd.fem +
    religPent + x + y,
  data = datVote
  )
  )

summary(lm(
  pt10.p ~ TFR + brancos10.p + nulos10.p  + propW25_34 + log( meanInc ) + educSecd.fem +
    religPent + x + y,
  weights = pop.scale,
  data = datVote
)
)


## SAR error model - autocorrelation ##
summary(errorsarlm(as.vector(pt10.p)~brancos10.p + nulos10.p +pop.scale + propW25_34 + log( meanInc ) + educSecd.fem +
                     religPent + x + y,listw=listw.map, zero.policy=TRUE, data=datVote))  #Fitting a SAR-Model#


summary(errorsarlm(as.vector(pt10.p)~brancos10.p + nulos10.p + propW25_34 + log( meanInc ) + educSecd.fem +
                     religPent + x + y,listw=listw.map, zero.policy=TRUE, weights = pop.scale, data=datVote))  #Fitting a SAR-Model#




## Teste de dependencia espacial dos modelos ###  
## Resposta TFT
summary(lm.LMtests(lm(TFT~psl18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1),listw.map, test=c("LMerr", "LMlag","RLMerr", "RLMlag", "SARMA"),zero.policy=TRUE))

summary(lm.LMtests(lm(TFT~pt18+brancos18+nulos18+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1),listw.map, test=c("LMerr", "LMlag","RLMerr", "RLMlag", "SARMA"),zero.policy=TRUE))

summary(lm.LMtests(lm(TFT~pt14+brancos14+nulos14+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1),listw.map, test=c("LMerr", "LMlag","RLMerr", "RLMlag", "SARMA"),zero.policy=TRUE))

summary(lm.LMtests(lm(TFT~pt10+brancos10+nulos10+pop.1+propW25_34+log(renda)+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1),listw.map, test=c("LMerr", "LMlag","RLMerr", "RLMlag", "SARMA"),zero.policy=TRUE))

### Nesses modelos, dependencia espacial no erro em todos os modelos.

## Resposta votos
summary(lm.LMtests(lm(psl18~TFT+brancos18+nulos18+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1),listw.map, test=c("LMerr", "LMlag","RLMerr", "RLMlag", "SARMA"),zero.policy=TRUE))

summary(lm.LMtests(lm(pt18~TFT+brancos18+nulos18+pop.1+pbf+edusec+relpent+coorden[,2]+coorden[,3],data=mapa.1),listw.map, test=c("LMerr", "LMlag","RLMerr", "RLMlag", "SARMA"),zero.policy=TRUE))

## No error em todos os casos. Apesar de significante em alguns casos para lag, optaremos por error model, dado que valores de p menores em todos os cen?rios.

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


