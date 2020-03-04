##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 2 - compute municipalities TFR and maps
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-03
##################################################################

### 1. Housekeeping and package loading #-------------------------
  rm(list = ls())
  graphics.off()

  require(data.table)
  require(dplyr)
  require(readr)
  require(geobr)

##################################################################

### 2. Read data and compute TFR #--------------------------------

  datTFR <- 
    readRDS( "DATA/dataPopPyrTFR.rds" )
  
  datTFR[, TFR := ( 10.65 - 12.55 * pi25_34 ) * C / W ] 

  datTFR[, 
         TFRfact := cut( 
           TFR, 
           breaks = c( 0, 1.5, 2, 2.5, 3, 3.5, Inf ), 
           labels = c( '[0.0;1.5)', '[1.5;2.0)', '[2.0;2.5)', '[2.5;3.0)', '[3.0;3.5)', '[3.5;Inf)' ),
           rigth = FALSE
           )
         ]

  muniMapDat <- 
    read_municipality(  year = 2010 )
  
  stateMapDat <- 
    read_state(  year = 2010 )
  
  muniMapDat <- 
    merge(
      muniMapDat,
      datTFR[,.(MUNICODE,TFR,TFRfact)],
      by.x = 'code_muni',
      by.y = 'MUNICODE'
    )
  
  require(ggplot2)
  x11(width = 6, height = 8)
  ggplot() + 
    geom_sf( 
      data = muniMapDat, 
      aes( fill = TFRfact ), 
      lwd    = 0
      ) +
    geom_sf( 
      data  = stateMapDat, 
      lwd   = 0.5,
      color = 'white',
      fill  = NA
    ) +
    scale_fill_manual( values = c( '#c7e9b4', '#7fcdbb', '#41b6c4', '#1d91c0', '#225ea8', '#0c2c84' ) )+
    theme_bw()

  ggsave('TFRmapBR2010.png')
  