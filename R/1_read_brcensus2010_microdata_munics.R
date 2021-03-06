##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 1 - read BR 2010 census data
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-04
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
  ## UF (State)   - V0001
  ## Municipality - V0002
  ## Educ Attainm - V6400
  ## Religion     - V6121
  
  filterPerson <- 
    c( 'V0001', 'V0002', 'V1003', 'V0010','V0601','V6036', 'V6400', 'V6121' )
  
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

### 4. Organize and clean person data #---------------------------

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
             ),
           relig    = as.numeric( substr( V6121, 1, 2 ) ),
           educ     = as.numeric( V6400 ),
           MUNICODE = paste0( V0001, V0002 )
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
        .( MUNICODE )
        ]

    # 4.3 Children 0-4 population total
    dtChildren <- 
      dtPerson[ 
        age %in% 0:4, 
        list(
          C       = sum( weight )
        ),
        .( MUNICODE )
        ]
    
    # 4.4 Religious proportions
    dtRelig <- 
      dtPerson[ ,
                list(
                  religPent = sum( weight[ relig %in% c( 31:39, 42:48 ) ] ) / sum( weight ),
                  religNone = sum( weight[ relig == 0 ] ) / sum( weight ),
                  religCat  = sum( weight[ relig %in% c( 11, 12, 13, 19 ) ] ) / sum( weight ),
                  POP       = sum( weight )
                ),
                .( MUNICODE )
               ]
    
    # 4.5 Education proportions
    dtEduc <- 
      dtPerson[ ,
                list(
                  educNone    = sum( weight[ educ == 1 ] ) / sum( weight ),
                  educElemSc  = sum( weight[ educ == 2 ] ) / sum( weight ),
                  educHighSc  = sum( weight[ educ == 3 ] ) / sum( weight ),
                  educUniver  = sum( weight[ educ == 4 ] ) / sum( weight )
                ),
                .( MUNICODE )
                ]
    
    # 4.6 Merge data 
    dtPopPyrTFR <- 
      merge(
        dtWomen,
        dtChildren,
        by = 'MUNICODE'
      )
    
    dtPopPyrTFR <- 
      merge(
        dtPopPyrTFR,
        dtRelig,
        by = 'MUNICODE'
      )
    
    dtPopPyrTFR <- 
      merge(
        dtPopPyrTFR,
        dtEduc,
        by = 'MUNICODE'
      )
    
##################################################################

    
### 5. Organize and clean dwelling data #---------------------------
    
    # 5.1 Set variable types 
    dtDwelling[,
               `:=` (
                 weight    = as.numeric( V0010 ) / ( 10 ^ 13 ),
                 MUNICODE  = paste0( V0001, V0002 ),
                 urb       = as.numeric( V1006 ),
                 IncPercap = as.numeric( V6531 )
                 )
               ]
               
    
    # 5.2 Mean per capita income by municipality 
    dtInc <- 
      dtDwelling[ 
        !is.na( IncPercap ),
        list(
          meanInc = sum( weight * IncPercap ) / sum( weight ) 
          ),
        .( MUNICODE )
        ]
    
    # 5.3 % rural dwellings
    dtRur <- 
      dtDwelling[ ,
                  list(
                    propRur = sum( weight[ urb == 2 ] ) / sum( weight ) 
                    ),
                  .( MUNICODE )
                  ]
    
    # 5.4 Merge data 

    dtPopPyrTFR <- 
      merge(
        dtPopPyrTFR,
        dtInc,
        by = 'MUNICODE'
      )
    
    dtPopPyrTFR <- 
      merge(
        dtPopPyrTFR,
        dtRur,
        by = 'MUNICODE'
      )
    
    # 5.5 Compute TFR
    dtPopPyrTFR[, TFR := ( 10.65 - 12.55 * pi25_34 ) * C / W ] 
    
    # 5.6 Change order
    datMuni <- 
      dtPopPyrTFR[, 
                  list(
                    reg       = substr( MUNICODE, 1, 1 ),
                    uf        = substr( MUNICODE, 1, 2 ),
                    municode  = MUNICODE,
                    pop        = POP,
                    women15_49 = W,
                    propW25_34 = pi25_34,
                    child0_4   = C,
                    TFR,
                    religPent,
                    religCat,
                    religNone,
                    educNone,
                    educPrim = educElemSc,
                    educSecd = educHighSc,
                    educTerc = educUniver,
                    meanInc
                    )
                  ]
    
    # 5.7 Save data
    saveRDS(
      datMuni,
      file = "DATA/dataBRCensusMunic.rds"
    )
##################################################################
    
### THE END