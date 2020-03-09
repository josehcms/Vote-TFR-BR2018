# Voto e Fecundidade: Uma análise a partir das eleições de 2018

O presente trabalho se propõe à investigar se as áreas de voto conservador (a partir dos resultados das eleições de 2018) estariam associadas a taxas de fecundidade total mais elevadas, controlando por covariáveis como renda, educação e filiação religiosa.

Para isso, constrõem-se bases com dados agregados em municípios e microrregiões, tomando as proporções de votos para determinados candidatos à presidência, com destaque para os candidatos do PT e do PSL. Os dados eleitorais de 2018 foram extraídos do [IPEADATA](http://www.ipeadata.gov.br/) As TFTs são computadas a partir da metodologia de [Hauer & Schmertmann (2020)](https://link.springer.com/article/10.1007/s13524-019-00842-x), que permitiu obter estimativas de fecundidade de período para pequenas áreas a partir de informações de contagem populacionais de municípios e microrregiões do Censo de 2010.

## Leitura da base

O arquivo ```mergedDataVoteCensus_micro.rds``` apresenta variáveis socioeconômicas do censo de 2010 e de votos no primeiro turno das eleições de 2018. Sua leitura em R é dada por:

```r
dat <- 
  readRDS( "DATA/mergedDataVoteCensus_micro.rds" )
```

## Descrição das variáveis

* ```regcode```: código da Grande Região
* ```regname```: nome da Grande Região
* ```ufcode```: código da Unidade da Federação
* ```ufname```: nome da Unidade da Federação
* ```microcode```: código da Microrregião
* ```microname```: nome da Microrregião
* ```pop```: volume populacional da Microrregião (censo 2010)
* ```women15_49```: total de mulheres entre 15 e 49 anos (censo 2010)
* ```propW25_34```: proporção de mulheres entre 25 a 34 anos dentre as mulheres entre 15 a 49 anos (censo 2010)
* ```child0_4```: total de crianças entre 0 e 4 anos (censo 2010)
* ```TFR```: taxa de fecundidade total computada a partir da fórmula ``` ( 10.65 - 12.55 * propW25_34 ) * child0_4 / women15_49 ``` proposta por [Hauer & Schmertmann (2020)](https://link.springer.com/article/10.1007/s13524-019-00842-x) para estimação de *extended total fertility rates*
* ```religPent```: proporção de pessoas (18 anos ou mais) de religião de origem Pentecostal (censo 2010)
* ```religCat```: proporção de pessoas (18 anos ou mais) de religião de origem Católica (censo 2010)
* ```religNone```: proporção de pessoas (18 anos ou mais) sem religião (censo 2010)
* ```educNone```: proporção de pessoas (25 anos ou mais) sem instrução ou com ensino fundamental incompleto (censo 2010)
* ```educPrim```: proporção de pessoas (25 anos ou mais) com ensino fundamental completo ou médio incompleto (censo 2010)
* ```educSecd```: proporção de pessoas (25 anos ou mais) com ensino médio completo ou superior incompleto (censo 2010)
* ```educterc```: proporção de pessoas (25 anos ou mais) com ensino superior completo (censo 2010)
* ```meanInc```: renda per capita domiciliar média da Microrregião em reais (censo 2010)
* ```pbf```: proporção de domicílios que declararam receber algum benefício do programa bolsa família (censo 2010)
* ```voteUteis```: total de votos úteis para candidatos à presidência (eleições 2018)
* ```voteBrancos```: total de votos brancos para candidatos à presidência (eleições 2018)
* ```voteNulos```: total de votos nulos para candidatos à presidência (eleições 2018)
* ```voteNOVO```: total de votos para o candidato do NOVO (eleições 2018)
* ```voteMDB```: total de votos para o candidato do MDB (eleições 2018)
* ```votePT```: total de votos para o candidato do PT (eleições 2018)
* ```votePSL```: total de votos para o candidato do PSL (eleições 2018)


