# Voto e Fecundidade: Uma análise a partir das eleições de 2018

O presente trabalho se propõe à investigar se as áreas de voto conservador (a partir dos resultados das eleições de 2018) estariam associadas a taxas de fecundidade total mais elevadas, controlando por covariáveis como renda, educação e filiação religiosa.

Para isso, constrõem-se bases com dados agregados em municípios e microrregiões, tomando as proporções de votos para determinados candidatos à presidência, com destaque para os candidatos do PT e do PSL. Os dados eleitorais de 2018 foram extraídos do [IPEADATA](http://www.ipeadata.gov.br/) As TFTs são computadas a partir da metodologia de [Hauer & Schmertmann (2020)](https://link.springer.com/article/10.1007/s13524-019-00842-x), que permitiu obter estimativas de fecundidade de período para pequenas áreas a partir de informações de contagem populacionais de municípios e microrregiões do Censo de 2010.

## Leitura da base

O arquivo ```demographic_elections_10_14_18_data.csv``` apresenta variáveis socioeconômicas do censo de 2010 e de votos no primeiro turno das eleições de 2010, 2014 e 2018. Sua leitura em R é dada por:

```r
require( data.table )

dat <- 
  fread( "DATA/demographic_elections_10_14_18_data.csv" )
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
* ```depRatio.youth```: razão de dependência jovem (``` pop0_14 / pop15_64 ```)
* ```depRatio.elder```: razão de dependência idosa (``` pop65+ / pop15_64 ```)
* ```sexRatio```: razão de sexo (``` pop.masculina / pop.feminina```)
* ```religPent```: proporção de pessoas (18 anos ou mais) de religião de origem Pentecostal (censo 2010)
* ```religCat```: proporção de pessoas (18 anos ou mais) de religião de origem Católica (censo 2010)
* ```religNone```: proporção de pessoas (18 anos ou mais) sem religião (censo 2010)
* ```educNone```: proporção de pessoas (25 anos ou mais) sem instrução ou com ensino fundamental incompleto (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ```educPrim```: proporção de pessoas (25 anos ou mais) com ensino fundamental completo ou médio incompleto (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ```educSecd```: proporção de pessoas (25 anos ou mais) com ensino médio completo ou superior incompleto (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ```educTerc```: proporção de pessoas (25 anos ou mais) com ensino superior completo (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ```meanInc```: renda per capita domiciliar média da Microrregião em reais (censo 2010)
* ```pbf```: proporção de domicílios que declararam receber algum benefício do programa bolsa família (censo 2010)
* ``` qInfant ```: probabilidade de óbito no primeiro ano de vida (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ``` qAdult ```: probabilidade de óbito adulto, 15 a 59 anos (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ``` q0_5 ```: probabilidade de óbito entre 0 e 4 anos (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ``` ex ```: expectativa de vida à idade x - para x = 0, 20, 40 e 60 (censo 2010) - com sufixo ``` .fem ``` para mulheres e  ``` .mal ``` para homens
* ```brancos```: total de votos brancos para candidatos à presidência no primeiro turno (com sufixo ``` 10 ``` para eleições 2010, ``` 14 ``` para eleições 2014  e  ``` 18 ``` para eleições 2018)
* ```nulos```: total de votos nulos para candidatos à presidência no primeiro turno (com sufixo ``` 10 ``` para eleições 2010, ``` 14 ``` para eleições 2014  e  ``` 18 ``` para eleições 2018)
* ```pt```: total de votos para o candidato do PT (com sufixo ``` 10 ``` para eleições 2010, ``` 14 ``` para eleições 2014  e  ``` 18 ``` para eleições 2018)
* ```novo18```: total de votos para o candidato do NOVO no primeiro turno (eleições 2018)
* ```mdb18```: total de votos para o candidato do MDB no primeiro turno (eleições 2018)
* ```psl18```: total de votos para o candidato do PSL no primeiro turno (eleições 2018)
* ```uteis```: total de votos úteis para candidatos à presidência no primeiro turno (com sufixo ``` 10 ``` para eleições 2010, ``` 14 ``` para eleições 2014  e  ``` 18 ``` para eleições 2018)




