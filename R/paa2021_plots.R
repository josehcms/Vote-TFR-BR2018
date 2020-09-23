##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Descriptive analysis - PAA 2021
### Author: Jose H C Monteiro da Silva
### Last update: 2020-09-19
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
require(cowplot)
require(ggsn)

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

# 2.4 region shape files
regMapDat <- 
  read_region(  year = 2010 )
####################################################################

### 3. Compute vote share #-----------------------------------------

# 3.1 Totals
datVote[ ,
         `:=`(
           total10 = brancos10 + nulos10 + uteis10,
           total14 = brancos14 + nulos14 + uteis14,
           total18 = brancos18 + nulos18 + uteis18
         )
]

# 3.2 Compute vote share
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
           psl18.p      = psl18 / uteis18
         )]
####################################################################

### 4. Plot 1: vote pt vs TFT #--------------------------------------

tfr_vote_plot <- 
  datVote %>%
  melt( 
    measure.vars  = c( 'psl18.p', 'pt18.p' ),
    id.vars       = c( 'microcode', 'regname', 'TFR' ),
    value.name    = 'vote_share',
    variable.name = 'party' ) %>%
  .[, party := ifelse( party == 'psl18.p', 'PSL', 'PT' ) ]

ggfig1 <- 
  ggplot( tfr_vote_plot ) +
  geom_hline( yintercept = 2.1, color = 'red', size = 0.25, 
              linetype = 'longdash' ) +
  geom_point( aes( x = vote_share, y = TFR, 
                   color = regname, shape = regname ), 
              size = 2.1 ) +
  geom_smooth( aes( x = vote_share, y = TFR ), 
               color = 'tomato3',
               method = 'lm' ) +

  scale_color_manual( values = c( 'Centro-Oeste' = 'black',
                                  'Nordeste'     = 'black',
                                  'Norte'        = 'steelblue4',
                                  'Sudeste'      = 'black',
                                  'Sul'          = 'forestgreen' ),
                      labels = c( 'Centro-Oeste' = 'Midwest',
                                  'Nordeste'     = 'Northeast',
                                  'Norte'        = 'North',
                                  'Sudeste'      = 'Southeast',
                                  'Sul'          = 'South' ),
                      name = '') +
  scale_shape_manual( values = c( 'Centro-Oeste' = 15,
                                  'Nordeste'     = 2,
                                  'Norte'        = 19,
                                  'Sudeste'      = 19,
                                  'Sul'          = 15 ),
                      labels = c( 'Centro-Oeste' = 'Midwest',
                                  'Nordeste'     = 'Northeast',
                                  'Norte'        = 'North',
                                  'Sudeste'      = 'Southeast',
                                  'Sul'          = 'South' ),
                      name = '') +
  scale_x_continuous( limits = c( 0.0, 0.80 ),
                      breaks = seq( 0, 1, 0.10 ),
                      name = 'Vote Share') +
  scale_y_continuous( limits = c( 1, 5.25 ),
                      breaks = seq( 0, 6, 0.50 ),
                      name = 'Total Fertility Rates (2010)' ) + 
  labs( 
    caption = 'Sources:\nBrazilian National Census 2010, IBGE (2010)\nBrazilian 2018 General Election Results, Superior Electoral Court (2018)' ) +
  facet_wrap( ~ party, nrow = 1 ) +
  theme_classic( ) +
  theme(
    panel.grid.major = element_line( color = 'gray75', size = 0.25, 
                                     linetype = 'dashed' ),
    axis.text = element_text( color = 'black', size = 11 ),
    axis.title = element_text( color = 'black', size = 15 ),
    plot.caption = element_text( size = 11, color = 'black', hjust = 0 ),
    legend.position = 'top',
    legend.text = element_text( color = 'black', size = 12 ),
    legend.title = element_text( color = 'black', size = 13 ),
    strip.background = element_rect( fill = 'gray90', color = 'black' ),
    strip.text = element_text( color = 'black', size = 15 ) 
    )

ggsave( filename = 'OUTPUTS/paa2021_fig1.png', height = 6, width = 9,
        plot = ggfig1 )
####################################################################

### 5. Prepare data for mapping #--------------------------------

# 5.1 check intervals using jenks
classIntervals( datVote$TFR, style = 'jenks', 5 )
classIntervals( datVote$pt10.p, style = 'jenks', 6 )
classIntervals( datVote$pt14.p, style = 'jenks', 6 )
classIntervals( datVote$pt18.p, style = 'jenks', 6 )
classIntervals( datVote$psl18.p, style = 'jenks', 6 )

# 5.2 create classes
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
           ),
           psl18.p.class = cut(
             psl18.p,
             breaks  = c( 0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.80, 0.99 ),
             labels  = c( '[0.00;0.10)', '[0.10;0.20)', '[0.20;0.30)', 
                          '[0.30;0.40)', '[0.40;0.50)', '[0.50;0.60)', 
                          '[0.60;0.80)', '[0.80;0.99)'
             ),
             right = FALSE
           )
         )]

# 5.3 set data for map plot of votes using facet_wrap
datVote.map <- 
  datVote[ , 
           .( microcode,
              pt10.p, pt14.p, pt18.p, 
              nulos10.p, nulos14.p, nulos18.p, 
              brancos10.p, brancos14.p, brancos18.p,
              psl18.p
           ) 
  ] %>%
  melt( 
    id.vars = c( 'microcode' ),
    measure.vars = c( 'pt10.p', 'pt14.p', 'pt18.p', 
                      'nulos10.p', 'nulos14.p', 'nulos18.p', 
                      'brancos10.p', 'brancos14.p', 'brancos18.p',
                      'psl18.p'
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
                              ifelse( grepl( 'psl', vote.year ),
                                      'PSL',
                                      'Nulos'
                                      )
                              )
                      ),
       prop
     ) 
  ]

# 5.4 merge vote with map sf file
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
      vote == 'PT' & year == 2018,
      list( 
        microcode, 
        year, 
        prop.class = cut(
          prop,
          breaks  = c( 0, 0.15, 0.30, 0.45, 0.60, 0.80, 0.99 ),
          labels  = c( '[0.00;0.15)', '[0.15;0.30)', '[0.30;0.45)', 
                       '[0.45;0.60)','[0.60;0.80)', '[0.80;0.99)'
          ),
          right = FALSE
        )
      )
    ],
    by.x = 'code_micro',
    by.y = 'microcode'
  )

pslmap.dat <- 
  merge(
    microMapDat,
    datVote.map[ 
      vote == 'PSL' & year == 2018,
      list( 
        microcode, 
        year, 
        prop.class = cut(
          prop,
          breaks  = c( 0, 0.15, 0.30, 0.45, 0.60, 0.80, 0.99 ),
          labels  = c( '[0.00;0.15)', '[0.15;0.30)', '[0.30;0.45)', 
                       '[0.45;0.60)','[0.60;0.80)', '[0.80;0.99)'
          ),
          right = FALSE
        )
      )
    ],
    by.x = 'code_micro',
    by.y = 'microcode'
  )

ptpslmap.dat <- 
  merge(
    microMapDat,
    datVote.map[ 
      vote %in% c( 'PSL', 'PT' )  & year == 2018,
      list( 
        microcode,
        vote,
        year, 
        prop.class = cut(
          prop,
          breaks  = c( 0, 0.15, 0.30, 0.45, 0.60, 0.80, 0.99 ),
          labels  = c( '[0.00;0.15)', '[0.15;0.30)', '[0.30;0.45)', 
                       '[0.45;0.60)','[0.60;0.80)', '[0.80;0.99)'
          ),
          right = FALSE
        )
      )
    ],
    by.x = 'code_micro',
    by.y = 'microcode'
  )

####################################################################

### 6. Plot 2: maps #-----------------------------------------------

# 6.1 plot 2. a) and b) votes PT and PSL 2018 map

ggfig2a <- 
  ggplot( ) +
  labs(
    caption = 'Source:\nBrazilian 2018 General Election Results, Superior Electoral Court (2018)'
  ) +
  geom_sf( 
    data = ptpslmap.dat,
    color = 'white',
    lwd = .01,
    aes(
      fill = prop.class
    )
  ) +
  scale_fill_manual(
    values  = c( '[0.00;0.15)'='#2166ac', '[0.15;0.30)'='#67a9cf', 
                 '[0.30;0.45)'='#d1e5f0', '[0.45;0.60)'='#fddbc7', 
                 '[0.60;0.80)'='#ef8a62', '[0.80;0.99)'='#b2182b' ),
    name    = 'Vote Share\n2018 General Elections\n(1st Round)'
  ) +
  facet_wrap( ~ vote, nrow =  1 ) +
  scale_y_continuous( breaks = seq( 0, -30, -10 ),
                      labels = paste0( seq( 0, -30, -10 ) ) ) +
  scale_x_continuous( breaks = seq( -30, -70, -10 ),
                      labels = paste0( seq( -30, -70, -10 ) ) ) +
  geom_sf( 
    data  = microMapDat,
    color = 'gray65',
    lwd   = .05,
    fill  = NA
  ) +
  geom_sf( 
    data  = stateMapDat,
    color = 'black',
    lwd   = .50,
    fill  = NA
  )  +
  theme_bw() +
  theme(
    axis.text = element_text( color = 'black', size = 11 ),
    plot.caption = element_text( hjust = 0, size = 12, color = 'black' ),
    legend.text  = element_text( size = 10, color = 'black' ),
    legend.title = element_text( color = 'black', size = 11 ),
    strip.background = element_rect( fill = 'gray90', color = 'black' ),
    strip.text = element_text( size = 15, color = 'black' )
  ) +
  north( map.dat, symbol = 15, location = 'bottomright' )


ggsave(filename = 'OUTPUTS/paa2021_fig2.png', width = 9, height = 6,
       plot = ggfig2a )


# 6.2 plot 2. c) fertility 2018 map
ggfig2c <- 
  ggplot( ) +
  labs( 
    caption = 'Source:\nBrazilian National Census 2010, IBGE (2010)' ) +
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
    name    = 'Total Fertility Rates\n(2010)'
  ) +
  geom_sf( 
    data  = microMapDat,
    color = 'gray65',
    lwd   = .05,
    fill  = NA
  ) +
  geom_sf( 
    data  = stateMapDat,
    color = 'black',
    lwd   = .50,
    fill  = NA
  )  +
  scale_y_continuous( breaks = seq( 0, -30, -10 ),
                      labels = paste0( seq( 0, -30, -10 ) ) ) +
  scale_x_continuous( breaks = seq( -30, -70, -10 ),
                      labels = paste0( seq( -30, -70, -10 ) ) ) +
  theme_bw() +
  theme(
    axis.text = element_text( color = 'black', size = 11 ),
    plot.caption = element_text( hjust = 0, size = 12, color = 'black' ),
    legend.text  = element_text( size = 10, color = 'black' ),
    legend.title = element_text( color = 'black', size = 11 )
  ) +
  north( map.dat, symbol = 15, location = 'bottomright' )

ggsave( filename = 'OUTPUTS/paa2021_fig3.png', width = 6, height = 6,
        plot = ggfig2c )


####################################################################

### 7. Plot 3: Bivariate maps #-------------------------------------

require(biscale)
map.datbisc <- bi_class( map.dat, y = TFR, x = psl18.p,
                         style = 'quantile', dim = 3 )

regLabs <- 
  data.table(
    reglab = c( 'North', 'Northeast', 'Midwest', 'Southeast', 'South' ),
    xval   = c( -57, -41, -53, -45, -51.5 ),
    yval   = c( -4, -9, -16, -20, -27 )
  )

bicolormap <- 
  ggplot( ) +
  labs( 
    caption = 'Sources:\nBrazilian National Census 2010, IBGE (2010)\nBrazilian 2018 General Election Results, Superior Electoral Court (2018)' ) +
  geom_sf( 
    data = map.datbisc,
    color = 'white',
    lwd = .01,
    aes(
      fill = bi_class
    )
  ) +
  bi_scale_fill( pal = "GrPink", dim = 3 ) +
  geom_sf( 
    data  = microMapDat,
    color = 'gray65',
    lwd   = .05,
    fill  = NA
  ) +
  geom_sf( 
    data  = regMapDat,
    color = 'black',
    lwd   = .50,
    fill  = NA
  )  +
  scale_y_continuous( breaks = seq( 0, -30, -10 ),
                      labels = paste0( seq( 0, -30, -10 ) ),
                      name = '') +
  scale_x_continuous( breaks = seq( -30, -70, -10 ),
                      labels = paste0( seq( -30, -70, -10 ) ),
                      name = '') +
  geom_text( data = regLabs,
             aes( x = xval, y = yval, label = reglab ),
             size = 4) +
  theme_bw() +
  theme(
    axis.text = element_text( color = 'black', size = 11 ),
    plot.caption = element_text( hjust = 0, size = 12, color = 'black' ),
    legend.position = 'none'
  ) +
  north( map.dat, symbol = 15, location = 'bottomright' )

legend <- 
  bi_legend( pal = "GrPink",
             dim = 3,
             ylab = "TFR ",
             xlab = "PSL Vote Share ",
             size = 12 ) +
  theme(
    panel.background = element_rect( fill = "transparent" ),
    plot.background = element_rect( fill = "transparent"  )
  )
          
ggfig3 <- 
  ggdraw() +
  draw_plot( bicolormap, 0, 0, 1, 1) +
  draw_plot( legend, 0.10, .18, 0.30, 0.30)

ggsave( filename = 'OUTPUTS/paa2021_fig4.png', width = 6, height = 6,
        plot = ggfig3 )
####################################################################
