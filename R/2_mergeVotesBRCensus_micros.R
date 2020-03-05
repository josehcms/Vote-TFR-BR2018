##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 2 - merge vote and census data
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-05
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

datFertVote <-
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
       religPent,
       religCat,
       religNone,
       educNone,
       educPrim,
       educSecd,
       educTerc,
       meanInc,
       voteUteis,
       voteBrancos,
       voteNulos,
       voteNOVO,
       voteMDB,
       votePT,
       votePSL
       )
     ]
  
##################################################################

### 4. Save data #------------------------------------------------

saveRDS(
  datFertVote,
  file = 'DATA/mergedDataVoteCensus_micro.rds'
)

##################################################################
