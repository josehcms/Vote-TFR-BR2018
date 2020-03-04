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

##################################################################

### 2. Read data and compute % votes #----------------------------

datVotes <- 
  fread( 'DATA/BRelections2010.csv' )

datVotes[ , 
          `:=`(
            PSL = VotesPSL.2rnd / ( VotesPSL.2rnd + VotesPT.2rnd ),
             PT = VotesPT.2rnd / ( VotesPSL.2rnd + VotesPT.2rnd )
            )
          ]
muniMapDat <- 
  read_municipality(  year = 2018 )

stateMapDat <- 
  read_state(  year = 2010 )

muniMapDat <- 
  merge(
    muniMapDat,
    datVotes[,.(MUNICODE,PSL)],
    by.x = 'code_muni',
    by.y = 'MUNICODE'
  )

require(ggplot2)
x11(width = 6, height = 8)
ggplot() + 
  geom_sf( 
    data = muniMapDat, 
    aes( fill = PSL ), 
    lwd    = 0
  ) +
  geom_sf( 
    data  = stateMapDat, 
    lwd   = 0.5,
    color = 'white',
    fill  = NA
  ) +
  scale_fill_gradient( low = 'tomato2', high = 'navyblue', limits = c(0,1) ) +
  theme_bw()

ggsave('VoteMapBR2018.png')
