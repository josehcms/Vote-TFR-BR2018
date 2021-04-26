require(geobr)
require(tidyverse)
require(data.table)

dat <- fread('DATA/eleicao_vereador_sexo.csv',
             encoding = 'Latin-1')

munis <- 
  fread('DATA/names_munis_2013.csv') %>%
  .[ , name_muni2 := as.character( name_muni2 ) ] %>%
  .[ , muniname_tse := ifelse( name_muni2 == '', 
                               name_muni %>% toupper,
                               name_muni2 %>% toupper ) ] %>%
  .[ , muniname_tse := gsub( '\n', '', muniname_tse ) ]

dat2 <- 
  merge(
    dat[ , .( muniname_tse = Abrangência, Cargo, Sexo, Eleito ) ],
    munis[ , .( muniname_tse, municode = code_muni ) ],
    by = 'muniname_tse',
    all = TRUE
    ) %>%
  .[ , municode := as.numeric( municode ) ]


#
ibge_micros <- 
  fread( 'DATA/ibge_muni_codes.csv' ) %>%
  .[ , municode := paste0( ufcode, sprintf( '%05d', municode ) )  %>% 
       as.numeric ] %>%
  .[ , .( microcode, municode ) ] %>%
  unique

# insert females DF
dat_tse <- 
  rbind(
    dat2[ municode != '5300108' ],
    data.table(
      muniname_tse = c( 'BRASÍLIA', 'BRASÍLIA' ),
      Cargo = c( 'Deputado Distrital', 'Deputado Distrital' ),
      Sexo  = c( 'Masculino', 'Feminino' ),
      Eleito = c( 20, 4 ),
      municode = c( 5300108, 5300108 )
    )
  ) %>%
  .[ !is.na( Eleito ) ] %>%
  dcast( municode ~ Sexo, value.var = 'Eleito' ) %>%
  .[ , municode := paste0( municode ) %>% as.numeric ] %>%
  merge( 
    ibge_micros,
    by = 'municode'
    ) %>%
  .[ , .(
    fem = sum( Feminino ),
    tot = sum( Feminino + Masculino )
  ),
  microcode ] %>%
  .[ , share_fem_politics := fem / tot ] 

write.csv2( dat_tse,
            file = 'DATA/elected_representatives_sex_micros.csv',
            row.names = F
            )


