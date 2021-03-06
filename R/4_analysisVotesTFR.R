##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 3 - compute votes and maps
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-04
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)
require(readr)
require(geobr)
require(ggplot2)
require(car)

##################################################################

### 2. Read data and compute % votes #----------------------------

dicReg <- 
  c(
    '1'='Norte',
    '2'='Nordeste',
    '3'='Sul',
    '4'='Sudeste',
    '5'='Centro-Oeste'
    )

datVotes <- 
  fread( 'DATA/BRelections2010.csv' )


datVotes[ , 
          `:=`(
            PSL = VotesPSL.2rnd / ( VotesPSL.2rnd + VotesPT.2rnd + VotesNul.2rnd ),
            PT  = VotesPT.2rnd / ( VotesPSL.2rnd + VotesPT.2rnd + VotesNul.2rnd ),
            NUL = VotesNul.2rnd / ( VotesPSL.2rnd + VotesPT.2rnd + VotesNul.2rnd )
            )
          ]


datTFR <- 
  readRDS( "DATA/dataPopPyrTFR.rds" ) %>%
  .[,MUNICODE:=as.numeric(MUNICODE)]

datTFR[, TFR := ( 10.65 - 12.55 * pi25_34 ) * C / W ] 

dat <- 
  merge(
    datVotes[!is.na(PSL), .(MUNICODE,MUNINAME,PSL,PT,NUL)],
    datTFR,
    by = 'MUNICODE'
  )

dat[,REG := substr(MUNICODE,1,1)]
dat[,POP := cut( POP, 
                 breaks = c( 0, 20000, 50000, 100000, 500000, Inf ), 
                 labels = c( '[0;20k)', '[20k;50)', '[50k;100k)', '[100k;500k)','[500k;Inf)'),
                 right = F
                 )
    ]



require(psych)
factAnaldat <- 
  dat[complete.cases(dat),.(religPent,religCat,religNone,POP,educNone,educElemSc,educHighSc,educUniver,meanInc)]

#Create the correlation matrix from bfi_data
factAnal_cor <- cor(factAnaldat)

#Factor analysis of the data
factors_data <- fa(r = factAnal_cor, nfactors = 3)
#Getting the factor loadings and model analysis



plot(factors_data)
mod3 <- 
  lm( 
    data = dat,
    TFR ~ PSL + log( meanInc )  + POP
  ) 

vif(mod3)
mod3 %>% summary
1/(1-0.434)
# init 4 charts in 1 panel
x11()
par(mfrow=c(2,2))
plot(mod3)
dat <- 
  rbind(
    dat[,.(MUNICODE,MUNINAME,PSL,PT,NUL,REG='Brasil',TFR)],
    dat
  )

x11(height = 5, width = 8)
ggplot( data = dat, aes( x = TFR, y = PSL) ) +
  geom_point(  ) +
  geom_smooth( method = 'lm' ) +
  facet_wrap(~REG, ncol = 3 ) +
  scale_y_continuous( name = '% Votos Válidos - Bolsonaro\n(2º Turno)',
                      breaks = seq(0,1,0.2),
                      labels = seq(0,100,20),
                      limits = c(0,1)) +
  scale_x_continuous( name = 'TFT 2010',
                      breaks = seq(0,8,1),
                      labels = seq(0,8,1),
                      limits = c(1,5)) +
  theme_bw()

x11(height = 5, width = 8)
ggplot( data = dat, aes( x = TFR, y = NUL) ) +
  geom_point(  ) +
  geom_smooth( method = 'lm' ) +
  facet_wrap(~REG, ncol = 3 ) +
  scale_y_continuous( name = '% Votos Válidos - Nulo\n(2º Turno)',
                      breaks = seq(0,1,0.2),
                      labels = seq(0,100,20),
                      limits = c(0,0.25)) +
  scale_x_continuous( name = 'TFT 2010',
                      breaks = seq(0,8,1),
                      labels = seq(0,8,1),
                      limits = c(1,5)) +
  theme_bw()

x11(height = 5, width = 8)
ggplot( data = dat, aes( x = TFR, y = PT) ) +
  geom_point(  ) +
  geom_smooth( method = 'lm' ) +
  facet_wrap(~REG, ncol = 3 ) +
  scale_y_continuous( name = '% Votos Válidos - Haddad\n(2º Turno)',
                      breaks = seq(0,1,0.2),
                      labels = seq(0,100,20),
                      limits = c(0,1)) +
  scale_x_continuous( name = 'TFT 2010',
                      breaks = seq(0,8,1),
                      labels = seq(0,8,1),
                      limits = c(1,5)) +
  theme_bw()

summary(lm(data = dat[REG=='Centro-Oeste',],PSL~TFR))$r.squared
summary(lm(data = dat[REG=='Sudeste',],PSL~TFR))$r.squared
summary(lm(data = dat[REG=='Sul',],PSL~TFR))$r.squared
summary(lm(data = dat[REG=='Nordeste',],PSL~TFR))$r.squared
summary(lm(data = dat[REG=='Norte',],PSL~TFR))$r.squared
summary(lm(data = dat[REG=='Brasil',],PSL~TFR))$r.squared

cor.test(dat[REG=='Centro-Oeste',]$PSL,dat[REG=='Centro-Oeste',]$TFR)
cor.test(dat[REG=='Sudeste',]$PSL,dat[REG=='Sudeste',]$TFR)
cor.test(dat[REG=='Sul',]$PSL,dat[REG=='Sul',]$TFR)
cor.test(dat[REG=='Nordeste',]$PSL,dat[REG=='Nordeste',]$TFR)
cor.test(dat[REG=='Norte',]$PSL,dat[REG=='Norte',]$TFR)
cor.test(dat[REG=='Brasil',]$PSL,dat[REG=='Brasil',]$TFR)
