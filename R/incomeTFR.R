##################################################################
### Project: TFR and Income in Brazil
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-10
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)
require(readr)

# dir <-  
#   "/home/josehcms/Documents/DATA/CENSO/IBGE/2010"

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
## Age          - V6036
## Control      - V0300

filterPerson <- 
  c( 'V0010','V0601','V6036', 'V0300' )

var2010Person <-
  filter( 
    dicPerson2010, 
    VAR %in% filterPerson 
  )

# 2.4 Data dictionary dwelling
## Weight       - V0010
## PerCapitaInc - V6531
## Control      - V0300

filterDwelling <- 
  c( 'V0300', 'V0010', 'V6531' )

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

var2010PDwelling$VAR <- 
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

### 4. Organize andm erge data #----------------------------------

# 4.1 Set variable types and 5-year age group in person data
dtPerson[, 
         `:=` (
           weight   = as.numeric( V0010 ) / ( 10 ^ 13 ),
           sex      = as.numeric( V0601 ),
           age      = as.numeric( 
             paste0( 
               cut( as.numeric( V6036 ), 
                    breaks = c( seq( 0, 50, 5 ), Inf ) , 
                    labels = seq( 0, 50, 5 ), 
                    right = F 
                    )
               )
             )
           )
         ]

# 4.2 Set Income quantiles in household data
dtDwelling[,
           `:=` (
             weight    = as.numeric( V0010 ) / ( 10 ^ 13 ),
             IncPercap = as.numeric( V6531 ) / 100,
             IncQuant  = 
               cut( 
                 as.numeric( V6531 ) / 100, 
                 breaks = c( quantile( x = as.numeric( dtDwelling$V6531 ) / 100, probs = seq( 0, 1, by = 0.25 ), na.rm = TRUE ) ), 
                 #labels = c( "0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"), 
                 #labels = c( "0-20", "20-40", "40-60", "60-80", "80-100"), 
                 labels = c( "0-25", "25-50", "50-75", "75-100"), 
                 
                 include.lowest = TRUE
                 )
             )
           ]

# 4.3 Merge person and household data

dtmerge <- 
  merge(
    dtPerson,
    dtDwelling[ , .( V0300, IncPercap, IncQuant )],
    by = 'V0300',
    all.x = TRUE
  )

# 4.4 Compute total women 15-49 and 25-34 proportion
dtWomen <- 
  dtmerge[ 
    age %in% 15:49 & sex == 2, 
    list(
      W       = sum( weight ),
      pi25_34 = sum( weight[ age %in% 25:34 ] ) / sum( weight )
    ),
    .( IncQuant )
    ]

# 4.5 Children 0-4 population total
dtChildren <- 
  dtmerge[ 
    age %in% 0:4, 
    list(
      C       = sum( weight )
    ),
    .( IncQuant )
    ]

# 4.6 Merge Women and Child data by income and compute TFR
dtTFR <- 
  merge(
    dtChildren[ !is.na(IncQuant), ],
    dtWomen[ !is.na(IncQuant), ],
    by = 'IncQuant'
  ) %>%
  setorder( IncQuant )

# 4.6 Compute TFR by income group
dtTFR[, TFR := ( 10.65 - 12.55 * pi25_34 ) * C / W ] 



##################################################################

### THE END
