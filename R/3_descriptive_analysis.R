##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Descriptive analysis
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
 
  # 4.3 set data for map plot of votes using facet_wrap
  datVote.map <- 
    datVote[ , 
             .( microcode,
                pt10.p, pt14.p, pt18.p, 
                nulos10.p, nulos14.p, nulos18.p, 
                brancos10.p, brancos14.p, brancos18.p 
                ) 
             ] %>%
    melt( 
      id.vars = c( 'microcode' ),
      measure.vars = c( 'pt10.p', 'pt14.p', 'pt18.p', 
                        'nulos10.p', 'nulos14.p', 'nulos18.p', 
                        'brancos10.p', 'brancos14.p', 'brancos18.p' 
                        ),
      variable.name = 'vote.year',
      value.name    = 'prop'
      ) %>%
    .[ ,
       list(
         microcode,
         year = ifelse( grepl( '10', vote.year ),
                        2010,
                        ifelse( grepl( '14', vote.year ),
                                2014,
                                2018 
                                )
                        ),
         vote = ifelse( grepl( 'pt', vote.year ),
                        'PT',
                        ifelse( grepl( 'brancos', vote.year ),
                                'Brancos',
                                'Nulos' 
                                )
                        ),
         prop
         ) 
       ]
  


  classIntervals(  datVote.map[vote=='Brancos' & year==2018]$prop, style = 'jenks', 5 )
  classIntervals(  datVote.map[vote=='Nulos']$prop, style = 'jenks', 6 )
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
  
  ptmap.dat <- 
    merge(
      microMapDat,
      datVote.map[ 
        vote == 'PT',
        list( 
          microcode, 
          year, 
          prop.class = cut(
            prop,
            breaks  = c( 0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.80, 0.99 ),
            labels  = c( '[0.00;0.10)', '[0.10;0.20)', '[0.20;0.30)', 
                         '[0.30;0.40)', '[0.40;0.50)', '[0.50;0.60)', 
                         '[0.60;0.80)', '[0.80;0.99)'
            ),
            right = FALSE
            )
          )
        ],
      by.x = 'code_micro',
      by.y = 'microcode'
    )
  
  brancosmap.dat <- 
    merge(
      microMapDat,
      datVote.map[ 
        vote == 'Brancos',
        list( 
          microcode, 
          year, 
          prop.class = cut(
            prop,
            breaks  = c( 0, 0.015, 0.025, 0.035, 0.045, 0.065 ),
            labels  = c( '[0.000;0.015)', '[0.015;0.025)', '[0.025;0.035)', 
                         '[0.035;0.045)', '[0.045;0.065)'
                         ),
            right = FALSE
          )
        )
        ],
      by.x = 'code_micro',
      by.y = 'microcode'
    )
  
  nulosmap.dat <- 
    merge(
      microMapDat,
      datVote.map[ 
        vote == 'Nulos',
        list( 
          microcode, 
          year, 
          prop.class = cut(
            prop,
            breaks  = c( 0.020, 0.040, 0.050, 0.065, 0.080, 0.100, 0.140 ),
            labels  = c( '[0.020;0.040)', '[0.040;0.050)', '[0.050;0.065)', 
                         '[0.065;0.080)', '[0.080;0.100)', '[0.100;0.140)'
            ),
            right = FALSE
          )
        )
        ],
      by.x = 'code_micro',
      by.y = 'microcode'
    )
  
  # 5.2 TFR map
  TFR.map <- 
    ggplot( ) +
    labs(
      title   = 'Taxas de Fecundidade Total por Microrregião - Brasil, 2010',
      caption = 'Fonte: IBGE, Censo Demográfico 2010'
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
      plot.caption = element_text( hjust = 1, size = 10 )
    )
  ggsave( 'OUTPUTS/tft_2010.png', width = 6, height = 6 )
  
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
      legend.position  = 'top',
      legend.direction = 'horizontal' 
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
      legend.position = 'none'
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
      legend.position = 'none'
    )
  
  ptall.map <- 
   ggplot( ) +
    labs(
      title    = 'Proporção de votos válidos no PT',
      subtitle = '1º Turno - Eleições Presidenciais Brasil - 2010, 2014, 2018',
      caption  = 'Fonte: IPEADATA'
    ) +
    geom_sf( 
      data = ptmap.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = prop.class
      )
    ) +
    facet_wrap( 
      ~ year,
      nrow = 1
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
      plot.title   = element_text( hjust = 0, size = 14 ),
      plot.subtitle = element_text( hjust = 0, size = 13 ),
      plot.caption = element_text( hjust = 1, size = 12 ),
      legend.text  = element_text( size = 12 ),
      strip.text   = element_text( size = 13 ), 
      legend.position = c(1.09, 0.60),
      plot.margin = margin( 0.25, 3.75, 0.25, 0.25, "cm")
    )
  
  ggsave( 'OUTPUTS/votos_pt.png', width = 10, height = 6 )
  
  # 5.4 Brancos votes
  brancosall.map <- 
    ggplot( ) +
    labs(
      title    = 'Proporção de votos brancos dentre votos totais',
      subtitle = '1º Turno - Eleições Presidenciais Brasil - 2010, 2014, 2018',
      caption  = 'Fonte: IPEADATA'
    ) +
    geom_sf( 
      data = brancosmap.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = prop.class
      )
    ) +
    facet_wrap( 
      ~ year,
      nrow = 1
    ) +
    scale_fill_manual(
      values  = c( '[0.000;0.015)'='#bcbddc', '[0.015;0.025)'='#9e9ac8', '[0.025;0.035)'='#807dba', 
                   '[0.035;0.045)'='#6a51a3', '[0.045;0.065)'='#4a1486' ),
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
      plot.title   = element_text( hjust = 0, size = 14 ),
      plot.subtitle = element_text( hjust = 0, size = 13 ),
      plot.caption = element_text( hjust = 1, size = 12 ),
      legend.text  = element_text( size = 12 ),
      strip.text   = element_text( size = 13 ), 
      legend.position = c(1.09, 0.60),
      plot.margin = margin( 0.25, 3.75, 0.25, 0.25, "cm")
    )
  
  ggsave( 'OUTPUTS/votos_brancos.png', width = 10, height = 6 )
  
  # 5.4 Brancos votes
  nulosall.map <- 
    ggplot( ) +
    labs(
      title    = 'Proporção de votos nulos dentre votos totais',
      subtitle = '1º Turno - Eleições Presidenciais Brasil - 2010, 2014, 2018',
      caption  = 'Fonte: IPEADATA'
    ) +
    geom_sf( 
      data = nulosmap.dat,
      color = 'white',
      lwd = .01,
      aes(
        fill = prop.class
      )
    ) +
    facet_wrap( 
      ~ year,
      nrow = 1
    ) +
    scale_fill_manual(
      values  = c( '[0.020;0.040)'='#c7e9c0', '[0.040;0.050)'='#a1d99b', '[0.050;0.065)'='#74c476', 
                   '[0.065;0.080)'='#41ab5d', '[0.080;0.100)'='#238b45', '[0.100;0.140)'='#005a32' ),
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
      plot.title   = element_text( hjust = 0, size = 14 ),
      plot.subtitle = element_text( hjust = 0, size = 13 ),
      plot.caption = element_text( hjust = 1, size = 12 ),
      legend.text  = element_text( size = 12 ),
      strip.text   = element_text( size = 13 ), 
      legend.position = c(1.09, 0.60),
      plot.margin = margin( 0.25, 3.75, 0.25, 0.25, "cm")
    )
  
  ggsave( 'OUTPUTS/votos_nulos.png', width = 10, height = 6 )
  # x11()
  # grid.arrange( TFR.map, pt10.map ,pt14.map , pt18.map, ncol = 2 )
####################################################################

### 6. x-y graphs #-------------------------------------------------
  
  datVote.plot <- 
    merge(
      datVote.map,
      datVote[ , .( microcode, TFR, educNone.fem, educNone.mal, educSecd.fem, educSecd.mal ) ],
      by = 'microcode'
    )

  # 6.1 Vote PT and TFT
  pt.tfr <- 
    ggplot( 
      data = datVote.plot[ vote == 'PT' ]
    ) +
    labs(
      title    = 'Votos no PT em Primeiro Turno vs Taxa de Fecundidade Total (TFT)',
      subtitle = 'Microrregiões - Brasil, 2010 e Eleições 2010, 2014 e 2018',
      caption  = 'Fonte: IBGE, Censo Demográfico 2010 e IPEADATA' 
    ) +
    geom_point(
      aes( 
        y = prop ,
        x = TFR
      ),
      color = 'black',
      size  = 0.75
    ) +
    geom_smooth(
      aes( 
        y = prop,
        x = TFR
      ),
      color = 'steelblue3',
      span  = 0.85,
      se    = F
    ) +
    scale_x_continuous( 
      breaks = seq( 0.5, 5.5, 0.5 ),
      name   = 'TFT 2010'
      ) +
    scale_y_continuous( 
      breaks = seq( 0, 1, 0.1 ),
      labels = paste0( seq( 0, 100, 10 ) ),
      name   = '% votos PT - Primeiro Turno'
    ) +
    facet_wrap( 
      ~ year,
      nrow = 1
      ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 14 ),
      plot.subtitle = element_text( hjust = 0, size = 13 ),
      plot.caption = element_text( hjust = 1, size = 12 ),
      legend.text  = element_text( size = 12 ),
      strip.text   = element_text( size = 13 )
    )
  
  ggsave( 'OUTPUTS/pt_vs_tft.png', width = 8, height = 4 )
  
  # 6.2 Vote Brancos and TFT
  brancos.tfr <- 
    ggplot( 
      data = datVote.plot[ vote == 'Brancos' ]
    ) +
    labs(
      title    = 'Votos Brancos em Primeiro Turno vs Taxa de Fecundidade Total (TFT)',
      subtitle = 'Microrregiões - Brasil, 2010 e Eleições 2010, 2014 e 2018',
      caption  = 'Fonte: IBGE, Censo Demográfico 2010 e IPEADATA' 
    ) +
    geom_point(
      aes( 
        y = prop ,
        x = TFR
      ),
      color = 'black',
      size  = 0.75
    ) +
    geom_smooth(
      aes( 
        y = prop,
        x = TFR
      ),
      color = 'steelblue3',
      span  = 0.85,
      se    = F
    ) +
    scale_x_continuous( 
      breaks = seq( 0.5, 5.5, 0.5 ),
      name   = 'TFT 2010'
    ) +
    scale_y_continuous( 
      breaks = seq( 0, 0.065, 0.005 ),
      labels = paste0( seq( 0, 6.5, 0.5 ) ),
      name   = '% votos Brancos - Primeiro Turno'
    ) +
    facet_wrap( 
      ~ year,
      nrow = 1
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 14 ),
      plot.subtitle = element_text( hjust = 0, size = 13 ),
      plot.caption = element_text( hjust = 1, size = 12 ),
      legend.text  = element_text( size = 12 ),
      strip.text   = element_text( size = 13 )
    )
  
  ggsave( 'OUTPUTS/brancos_vs_tft.png', width = 8, height = 4 )
  
  # 6.3 Vote Nulos and TFT
  nulos.tfr <- 
    ggplot( 
      data = datVote.plot[ vote == 'Nulos' ]
    ) +
    labs(
      title    = 'Votos Nulos em Primeiro Turno vs Taxa de Fecundidade Total (TFT)',
      subtitle = 'Microrregiões - Brasil, 2010 e Eleições 2010, 2014 e 2018',
      caption  = 'Fonte: IBGE, Censo Demográfico 2010 e IPEADATA' 
    ) +
    geom_point(
      aes( 
        y = prop ,
        x = TFR
      ),
      color = 'black',
      size  = 0.75
    ) +
    geom_smooth(
      aes( 
        y = prop,
        x = TFR
      ),
      color = 'steelblue3',
      span  = 0.85,
      se    = F
    ) +
    scale_x_continuous( 
      breaks = seq( 0.5, 5.5, 0.5 ),
      name   = 'TFT 2010'
    ) +
    scale_y_continuous( 
      breaks = seq( 0, 0.15, 0.025 ),
      labels = paste0( seq( 0, 15, 2.5 ) ),
      name   = '% votos Nulos - Primeiro Turno'
    ) +
    facet_wrap( 
      ~ year,
      nrow = 1
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text( hjust = 0, size = 14 ),
      plot.subtitle = element_text( hjust = 0, size = 13 ),
      plot.caption = element_text( hjust = 1, size = 12 ),
      legend.text  = element_text( size = 12 ),
      strip.text   = element_text( size = 13 )
    )
  
  ggsave( 'OUTPUTS/nulos_vs_tft.png', width = 8, height = 4 )
####################################################################
    microMap.centroids <-
    st_centroid( microMapDat )
  