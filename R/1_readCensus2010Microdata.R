##################################################################
### Project: TFR and Vote in Brazilian 2018 elections
### Supplementary material 1 - read BR 2010 census data
### Author: Jose H C Monteiro da Silva
### Last update: 2020-03-03
##################################################################

### 1. Housekeeping and package loading #-------------------------
  rm(list = ls())
  graphics.off()

  require(data.table)
  require(dplyr)
  require(readr)

  dir <-  
    "/home/josehcms/Documents/DATA/CENSO/IBGE/2010"
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
  
  # 2.3 Data dictionary
  ## Weight       - V0010
  ## Sex          - V0601
  ## Age          - V6036
  ## UF (State)   - V0001
  ## Municipality - V0002
  
  filterPerson <- 
    c( 'V0001', 'V0002', 'V0010','V0601','V6036' )
  
  var2010Person <-
    filter( 
      dicPerson2010, 
      VAR %in% filterPerson 
      )
  
##################################################################
  
### 3. Read data #------------------------------------------------
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
##################################################################

### 4. Organize and clean data #----------------------------------

    # 4.1 Set variable types and 5-year age group
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
    
    # 4.4 Merge data 
    dtPopPyrTFR <- 
      merge(
        dtWomen,
        dtChildren,
        by = 'MUNICODE'
      )
    
    # 4.5 Save data
    saveRDS(
      dtPopPyrTFR,
      file = "DATA/dataPopPyrTFR.rds"
    )

##########
