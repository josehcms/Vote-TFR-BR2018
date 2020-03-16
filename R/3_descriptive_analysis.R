##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Descriptive analysis
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-15
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
    read_micro_region(  year = 2015 )
  
  # 2.3 state shape files
  stateMapDat <- 
    read_state(  year = 2010 )
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

### 4. Create intervals for mapping #--------------------------------
  
  # 4.1 check intervals using jenks
  classIntervals( datVote$TFR, style = 'jenks', 5 )
  classIntervals( datVote$pt10.p, style = 'jenks', 6 )
  classIntervals( datVote$pt14.p, style = 'jenks', 6 )
  classIntervals( datVote$pt18.p, style = 'jenks', 6 )
  
  # 4.2 create classes
  datVote[ , 
           `:=`(
             TFR.class = cut(
               TFR,
               breaks  = c( 1.25, 1.75, 2.15, 2.65, 3.50, 5.10 ),
               labels  = c( '[1.25;1.75)', '[1.75;2.15)', '[2.15;2.65)', 
                            '[2.65;3.50)', '[3.50;5.10)' ),
               right = FALSE
               ),
             pt10.p.class = cut(
               pt10.p,
               breaks  = c( 0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.80, 0.99 ),
               labels  = c( '[0.00;0.10)', '[0.10;0.20)', '[0.20;0.30)', 
                            '[0.30;0.40)', '[0.40;0.50)', '[0.50;0.60)', 
                            '[0.60;0.80)', '[0.80;0.99)'
                            ),
               right = FALSE
             ),
             pt14.p.class = cut(
               pt14.p,
               breaks  = c( 0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.80, 0.99 ),
               labels  = c( '[0.00;0.10)', '[0.10;0.20)', '[0.20;0.30)', 
                            '[0.30;0.40)', '[0.40;0.50)', '[0.50;0.60)', 
                            '[0.60;0.80)', '[0.80;0.99)'
                            ),
               right = FALSE
             ),
             pt18.p.class = cut(
               pt18.p,
               breaks  = c( 0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.80, 0.99 ),
               labels  = c( '[0.00;0.10)', '[0.10;0.20)', '[0.20;0.30)', 
                            '[0.30;0.40)', '[0.40;0.50)', '[0.50;0.60)', 
                            '[0.60;0.80)', '[0.80;0.99)'
                            ),
               right = FALSE
             )
           )]
 
####################################################################
  
### 5. Plot descriptive maps #--------------------------------------

  # 5.1 merge vote and demographic data with map data
  map.dat <- 
    merge(
      microMapDat,
      datVote,
      by.x = 'code_micro',
      by.y = 'microcode'
    )
  
  # 5.2 TFR map
  TFR.map <- 
    ggplot( ) +
    labs(
      title   = 'Taxas de Fecundidade Total por Microrregião - Brasil, 2010',
      caption = 'IBGE, Censo Demográfico 2010'
    ) +
    geom_sf( 
      data = map.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = TFR.class
      )
    ) +
    scale_fill_brewer( 
      type    = 'seq',
      palette = 9,
      name    = ''
      ) +
    geom_sf( 
      data  = stateMapDat,
      color = 'black',
      lwd   = .50,
      fill  = NA
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 12 ),
      plot.caption = element_text( hjust = 1, size = 10 ),
    )
  
  # 5.3 PT Vote map
  pt10.map <- 
    ggplot( ) +
    labs(
      title   = '% de votos válidos no PT - Brasil, 2010',
      caption = 'IPEADATA'
    ) +
    geom_sf( 
      data = map.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = pt10.p.class
      )
    ) +
    scale_fill_manual(
      values  = c( '[0.00;0.10)'='#fee0d2', '[0.10;0.20)'='#fcbba1', '[0.20;0.30)'='#fc9272', 
                   '[0.30;0.40)'='#fb6a4a', '[0.40;0.50)'='#ef3b2c', '[0.50;0.60)'='#cb181d', 
                   '[0.60;0.80)'='#a50f15', '[0.80;0.99)'='#67000d' ),
      name    = ''
    ) +
    geom_sf( 
      data  = stateMapDat,
      color = 'black',
      lwd   = .50,
      fill  = NA
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 12 ),
      plot.caption = element_text( hjust = 1, size = 10 ),
    )
  
  pt14.map <- 
    ggplot( ) +
    labs(
      title   = '% de votos válidos no PT - Brasil, 2014',
      caption = 'IPEADATA'
    ) +
    geom_sf( 
      data = map.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = pt14.p.class
      )
    ) +
    scale_fill_manual(
      values  = c( '[0.00;0.10)'='#fee0d2', '[0.10;0.20)'='#fcbba1', '[0.20;0.30)'='#fc9272', 
                   '[0.30;0.40)'='#fb6a4a', '[0.40;0.50)'='#ef3b2c', '[0.50;0.60)'='#cb181d', 
                   '[0.60;0.80)'='#a50f15', '[0.80;0.99)'='#67000d' ),
      name    = ''
    ) +
    geom_sf( 
      data  = stateMapDat,
      color = 'black',
      lwd   = .50,
      fill  = NA
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 12 ),
      plot.caption = element_text( hjust = 1, size = 10 ),
    )
  
  pt18.map <- 
    ggplot( ) +
    labs(
      title   = '% de votos válidos no PT - Brasil, 2018',
      caption = 'IPEADATA'
    ) +
    geom_sf( 
      data = map.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = pt18.p.class
      )
    ) +
    scale_fill_manual(
      values  = c( '[0.00;0.10)'='#fee0d2', '[0.10;0.20)'='#fcbba1', '[0.20;0.30)'='#fc9272', 
                   '[0.30;0.40)'='#fb6a4a', '[0.40;0.50)'='#ef3b2c', '[0.50;0.60)'='#cb181d', 
                   '[0.60;0.80)'='#a50f15', '[0.80;0.99)'='#67000d' ),
      name    = ''
    ) +
    geom_sf( 
      data  = stateMapDat,
      color = 'black',
      lwd   = .50,
      fill  = NA
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 12 ),
      plot.caption = element_text( hjust = 1, size = 10 ),
    )
  
  # 5.3 all plots
  x11()
  grid.arrange( TFR.map, pt10.map ,pt14.map , pt18.map, ncol = 2 )
####################################################################

### 6. x-y graphs #-------------------------------------------------
  
  # 6.1 Vote PT and TFT
  x11()
  ggplot( 
    data = datVote 
    ) +
    geom_point(
      aes( 
        x = log( pt10.p ),
        y = TFR
        ),
      color = 'tomato3'
    ) +
    geom_smooth(
      aes( 
        x = log( pt10.p ),
        y = TFR
      ),
      color = 'tomato3',
      span  = 0.8,
      se    = F
    ) +
    geom_point(
      aes( 
        x = log( pt14.p ),
        y = TFR
      ),
      color = 'forestgreen'
    ) +
    geom_smooth(
      aes( 
        x = log( pt14.p ),
        y = TFR
      ),
      color = 'forestgreen',
      span  = 0.8,
      se    = F
    ) +
    geom_point(
      aes( 
        x = log( pt18.p ),
        y = TFR
      ),
      color = 'skyblue'
    ) +
    geom_smooth(
      aes( 
        x = log( pt18.p ),
        y = TFR
      ),
      color = 'skyblue',
      span  = 0.8,
      se    = F
    ) +
    theme_bw()
####################################################################
    microMap.centroids <-
    st_centroid( microMapDat )
  