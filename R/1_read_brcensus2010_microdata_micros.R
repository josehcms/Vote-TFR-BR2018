##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 1 - read BR 2010 census data for micros
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-15
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)
require(readr)

# dir <-
#   "/home/josehcms/Documents/DATA/CENSO/IBGE/2010"
# 
dir <-
  '/media/jose/DATA/CENSO 2010'
##################################################################

### 2. BR census dictionary #-------------------------------------
dirPerson <- 
  file.path(
    dir,
    "PESSOAS"
  )

dicPerson2010 <- 
  read.csv2(
    file.path(
      dir,
      "Leitura em R/dic_var2010_pes.csv"
    ),
    colClasses = c( "character", "numeric", "numeric", "numeric", "numeric" ),
    sep = ","
  )

dirDwelling <- 
  file.path(
    dir,
    "DOMICILIOS"
  )

dicDwelling2010 <- 
  read.csv2(
    file.path(
      dir,
      "Leitura em R/dic_var2010_dom.csv"
    ),
    colClasses = c( "character", "numeric", "numeric", "numeric", "numeric" ),
    sep = ","
  )

# 2.3 Data dictionary person
## Weight       - V0010
## Sex          - V0601
## Head of hhwd - V0502
## Age          - V6036
## UF (State)   - V0001
## Municipality - V0002
## Educ Attainm - V6400
## Religion     - V6121
## Microrregion - V1003
## Bolsa Fam    - V0657
## Control      - V0300

filterPerson <- 
  c( 'V0300', 'V0001', 'V0002', 'V1003', 'V0010','V0601','V6036', 'V6400', 'V6121', 'V0657', 'V0502' )

var2010Person <-
  filter( 
    dicPerson2010, 
    VAR %in% filterPerson 
  )

# 2.4 Data dictionary dwelling
## Weight       - V0010
## UF (State)   - V0001
## Municipality - V0002
## Urban/rural  - V1006    
## PerCapitaInc - V6531
## Microrregion - V1003

filterDwelling <- 
  c( 'V0001', 'V0002', 'V1003', 'V0010', 'V1006', 'V6531' )

var2010Dwelling <-
  filter( 
    dicDwelling2010, 
    VAR %in% filterDwelling 
  )

##################################################################

### 3. Read data #------------------------------------------------

# 3.1 Person data
gc(reset=T)
personFiles <- 
  list.files( dirPerson )

var2010Person$VAR <- 
  as.character( var2010Person$VAR )

colPos <- 
  fwf_positions( 
    var2010Person$inicio, 
    var2010Person$fim, 
    col_names = var2010Person$VAR
  )

datPerson <-  
  list()

count = 1

for( i in 1 : length( personFiles ) ){
  
  datPerson[count] <- 
    list(
      read_fwf(
        file = file.path( dirPerson,
                          personFiles[i]),
        col_positions = colPos 
      )
    )
  
  count <-  
    count + 1
}

dtPerson <- 
  rbindlist( datPerson )

rm(datPerson)

# 3.2 Dwelling data
gc(reset=T)
dwellingFiles <- 
  list.files( dirDwelling )

var2010Dwelling$VAR <- 
  as.character( var2010Dwelling$VAR )

colPos <- 
  fwf_positions( 
    var2010Dwelling$inicio, 
    var2010Dwelling$fim, 
    col_names = var2010Dwelling$VAR
  )

datDwelling <-  
  list()

count = 1

for( i in 1 : length( dwellingFiles ) ){
  
  datDwelling[count] <- 
    list(
      read_fwf(
        file = file.path( dirDwelling,
                          dwellingFiles[i]),
        col_positions = colPos 
      )
    )
  
  count <-  
    count + 1
}

dtDwelling <- 
  rbindlist( datDwelling )

rm(datDwelling)
##################################################################

### 4. Organize and clean person data #---------------------------

# 4.1 Set variable types and 5-year age group in person data
dtPerson[, 
         `:=` (
           weight   = as.numeric( V0010 ) / ( 10 ^ 13 ),
           sex      = as.numeric( V0601 ),
           age      = as.numeric( V6036 ),
           relig    = as.numeric( substr( V6121, 1, 2 ) ),
           educ     = as.numeric( V6400 ),
           MICROCODE = paste0( V0001, V1003 ),
           uf        = V0001,
           pbf       = ifelse( V0657 == 1, 1, 0 ),
           head      = ifelse( as.numeric(V0502) == 1, 1, 0 )
           )
         ]

# 4.2 Total women 15-49 and 25-34 proportion
dtWomen <- 
  dtPerson[ 
    age %in% 15:49 & sex == 2, 
    list(
      W       = sum( weight ),
      pi25_34 = sum( weight[ age %in% 25:34 ] ) / sum( weight )
    ),
    .( MICROCODE )
    ]

# 4.3 Children 0-4 population total
dtChildren <- 
  dtPerson[ 
    age %in% 0:4, 
    list(
      C       = sum( weight )
    ),
    .( MICROCODE )
    ]

# 4.4 Religious proportions
dtRelig <- 
  dtPerson[ 
    age >= 18,
    list(
      religPent = sum( weight[ relig %in% c( 31:39, 42:48 ) ] ) / sum( weight ),
      religNone = sum( weight[ relig == 0 ] ) / sum( weight ),
      religCat  = sum( weight[ relig %in% c( 11, 12, 13, 19 ) ] ) / sum( weight )
      ),
    .( MICROCODE )
    ]

# 4.5 Education proportions
dtEduc <- 
  dtPerson[ 
    age >= 25,
    list(
      educNone.fem    = sum( weight[ educ == 1 & sex == 2 ] ) / sum( weight[ sex == 2 ] ),
      educElemSc.fem  = sum( weight[ educ == 2 & sex == 2 ] ) / sum( weight[ sex == 2 ] ),
      educHighSc.fem  = sum( weight[ educ == 3 & sex == 2 ] ) / sum( weight[ sex == 2 ] ),
      educUniver.fem  = sum( weight[ educ == 4 & sex == 2 ] ) / sum( weight[ sex == 2 ] ),
      educNone.mal    = sum( weight[ educ == 1 & sex == 1 ] ) / sum( weight[ sex == 1 ] ),
      educElemSc.mal  = sum( weight[ educ == 2 & sex == 1 ] ) / sum( weight[ sex == 1 ] ),
      educHighSc.mal  = sum( weight[ educ == 3 & sex == 1 ] ) / sum( weight[ sex == 1 ] ),
      educUniver.mal  = sum( weight[ educ == 4 & sex == 1 ] ) / sum( weight[ sex == 1 ] )
      ),
    .( MICROCODE )
    ]

# 4.6 Population totals, age-dependecy ratio and sex ratio
dtPop <- 
  dtPerson[ , 
    list(
      POP            = sum( weight ),
      depRatio.youth = sum( weight[ age < 15 ] ) / sum( weight[ age > 14 & age < 65 ] ),
      depRatio.elder = sum( weight[ age > 64 ] ) / sum( weight[ age > 14 & age < 65 ] ),
      sexRatio       = sum( weight[ sex == 1 & age > 17 & age < 70 ] ) / sum( weight[ sex == 2 & age > 17 & age < 70 ] )
    ),
    .( MICROCODE )
    ]

# 4.7 Bolsa Familia
dtPBF <- 
  dtPerson[ , 
            pbf.dwelling := sum( pbf, na.rm = T ), 
            .( V0300 ) 
            ] %>%
  .[ ,
     list(
       pbf     = ifelse( pbf.dwelling > 0, 1, 0 ),
       weight,
       MICROCODE
       ),
     .( V0300 )
     ] %>%
  unique %>%
  .[ , 
     list(
       pbf = sum( pbf * weight ) / sum( weight )
       ),
     .( MICROCODE )
     ]
  
# 4.8 Women head of household
dtWomenHead <- 
  dtPerson[ head == 1, 
            list(
              Whead = sum( weight[ sex == 2 ] ) / sum( weight )
            ),
            .( MICROCODE )
            ]
# 4.9 Merge data 
dtPopPyrTFR <- 
  merge(
    dtWomen,
    dtChildren,
    by = 'MICROCODE'
  )

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtRelig,
    by = 'MICROCODE'
  )

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtEduc,
    by = 'MICROCODE'
  )

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtPop,
    by = 'MICROCODE'
  )

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtPBF,
    by = 'MICROCODE'
  )

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtWomenHead,
    by = 'MICROCODE'
  )

##################################################################


### 5. Organize and clean dwelling data #---------------------------

# 5.1 Set variable types 
dtDwelling[,
           `:=` (
             weight    = as.numeric( V0010 ) / ( 10 ^ 13 ),
             MICROCODE  = paste0( V0001, V1003 ),
             urb       = as.numeric( V1006 ),
             IncPercap = as.numeric( V6531 )/ 100
             )
           ]


# 5.2 Mean per capita income by municipality 
dtInc <- 
  dtDwelling[ 
    !is.na( IncPercap ),
    list(
      meanInc = sum( weight * IncPercap ) / sum( weight ) 
    ),
    .( MICROCODE )
    ]

# 5.3 % rural dwellings
dtRur <- 
  dtDwelling[ ,
              list(
                propRur = sum( weight[ urb == 2 ] ) / sum( weight ) 
              ),
              .( MICROCODE )
              ]

# 5.4 Merge data 

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtInc,
    by = 'MICROCODE'
  )

dtPopPyrTFR <- 
  merge(
    dtPopPyrTFR,
    dtRur,
    by = 'MICROCODE'
  )

# 5.5 Compute TFR
dtPopPyrTFR[, TFR := ( 10.65 - 12.55 * pi25_34 ) * C / W ] 

# 5.6 Change order
datMicro <- 
  dtPopPyrTFR[, 
              list(
                reg        = substr( MICROCODE, 1, 1), 
                uf         = substr( MICROCODE, 1, 2),
                microcode  = as.numeric( MICROCODE ),
                pop        = POP,
                women15_49 = W,
                propW25_34 = pi25_34,
                child0_4   = C,
                TFR,
                depRatio.youth,
                depRatio.elder,
                sexRatio,
                womenHead = Whead,
                religPent,
                religCat,
                religNone,
                educNone.fem,
                educNone.mal,
                educPrim.fem = educElemSc.fem,
                educPrim.mal = educElemSc.mal,
                educSecd.fem = educHighSc.fem,
                educSecd.mal = educHighSc.mal,
                educTerc.fem = educUniver.fem,
                educTerc.mal = educUniver.mal,
                meanInc,
                pbf
                )
              ]

# 5.7 Add mortality data

lt.micros <- 
  fread(
    '/media/jose/DATA/Dropbox (IPC-IG)/BRASIL 3 TEMPOS RPPS/RELATORIOS DOS PESQUISADORES/Jose Henrique/TABUAS DE VIDA/DATA/lt.micro.2010.csv'
    )

datMort <- 
  lt.micros[ ,
    list(
      q.adult = sum( ndx[ age %in% 15:59 ] ) / lx [ age == 15 ],
      q.infan = nqx[ age == 0 ],
      q0_5    = sum( ndx[ age %in% 0:4 ] ) / lx [ age == 0 ],
      e0      = ex[ age == 0 ],
      e20     = ex[ age == 20 ],
      e40     = ex[ age == 40 ],
      e60     = ex[ age == 60 ]
    ),
    .( microcode, sex )
  ] %>%
  dcast( 
    microcode ~ sex , 
    value.var = c( 'q.adult', 'q.infan', 'q0_5', 'e0', 'e20', 'e40', 'e60' ) 
    )

datMicro <- 
  merge(
    datMicro,
    datMort,
    by = 'microcode'
  )

# 5.8 Save data
saveRDS(
  datMicro,
  file = "DATA/dataBRCensusMicro.rds"
)
##################################################################

### THE END