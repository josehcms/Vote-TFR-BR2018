##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 2 - merge vote and census data
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

##################################################################

### 2. Read data bases #------------------------------------------

datCensus <- 
  readRDS( "DATA/dataBRCensusMicro.rds" )

datTSE <- 
  fread( "DATA/votes_micro.csv" )

dicReg <- 
  c(
    '1' = 'Norte',
    '2' = 'Nordeste',
    '3' = 'Sudeste',
    '4' = 'Sul',
    '5' = 'Centro-Oeste'
  )

##################################################################

### 3. Merge data #-----------------------------------------------

datComplete <-
  merge(
    datCensus,
    datTSE,
    by = 'microcode'
  ) %>%
  .[ ,
     list(
       regcode = reg,
       regname = dicReg[ as.character( reg ) ],
       ufcode  = uf,
       ufname,
       microcode,
       microname,
       pop,
       women15_49,
       propW25_34,
       child0_4,
       TFR,
       depRatio.youth,
       depRatio.elder,
       sexRatio,
       religPent,
       religCat,
       religNone,
       educNone.fem, educNone.mal,
       educPrim.fem, educPrim.mal,
       educSecd.fem, educSecd.mal,
       educTerc.fem, educTerc.mal,
       meanInc,
       pbf,
       qInfant.fem = q.infan_f,
       qInfant.mal = q.infan_m,
       qAdult.fem  = q.adult_f,
       qAdult.mal  = q.adult_m,
       q0_5.fem    = q0_5_f,
       q0_5.mal    = q0_5_m,
       e0.mal      = e0_m,
       e0.fem      = e0_f,
       e20.mal     = e20_m,
       e20.fem     = e20_f,
       e40.mal     = e40_m,
       e40.fem     = e40_f,
       e60.mal     = e60_m,
       e60.fem     = e60_f,
       brancos02, nulos02, pt02, psdb02, uteis02,
       brancos06, nulos06, pt06, psdb06, uteis06,
       brancos10, nulos10, pt10, uteis10,
       brancos14, nulos14, pt14, uteis14,
       brancos18, nulos18, pt18, novo18, mdb18, psl18, uteis18
       )
     ]
  
##################################################################

### 4. Save data #------------------------------------------------

write_csv2(
  datComplete,
  'DATA/demographic_elections_02_06_10_14_18_data.csv'
)

##################################################################
