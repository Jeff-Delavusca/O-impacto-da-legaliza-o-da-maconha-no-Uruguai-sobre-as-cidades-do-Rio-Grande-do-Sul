### MONOGRAFIA 2


#PACOTES
library(tidyverse)
library(magrittr)



#CRIANDO VARIAVEL DE ANO PARA CADA UMA DAS BASES DE 2008 A 2021
X2021 <- X2021 %>% 
  mutate(ano=2021)



#UNINDO TODAS AS BASES EM APENAS UMA - CRIMES
#crimes <- list(X2008, X2009, X2010, X2011, X2012, X2013,
              # X2014, X2015, X2016, X2017, X2018, X2019,
               #X2020, X2021) %>% 
  #bind_rows()



#FILTRAR O ID DOS MUNICIPIOS DO RIO GRANDE DO SUL
id_municipios <- municipios %>% 
  filter(codigo_uf == 43)



#RENOMEANDO A COLUNA COM O CODIGO DO MUNICIPIO
id_municipios <- id_municipios %>% 
  rename(id_municipio = codigo_ibge)



#RENOMEANDO A COLUNA COM O NOME DO MUNICIPIO
crimes <- crimes %>% 
  rename(nome = Município)



#COLOCANDO TODA A BASE COM OS CODIGOS DO MUNICIPIOS
#EM LETRA MAIUSCULA
id_municipios %<>%
  mutate_if(is.character, toupper)



#SELECIONADO APENAS AS COLUNAS DE INTERESSE DA BASE DE CODIGO
#DOS MUNICIPIOS
codigos <- id_municipios %>% 
  select(id_municipio, nome, latitude, longitude)



#AQUI EU COLEI A COLUNA NOME DO DF CODIGO EM CADA UM DOS ANOS
#REORDENEI AS COLUNAS E POR FIM EXCLUI A TERCEIRA COLUNA COM
#O NOME DE MUNICIPIO. FIZ ESSE PROCESSO PORQUE NAO SABIA COMO LIMPAR
#OS CARACTERES ESPECIAIS DA COLUNA NOME DA BASE CODIGO
X2021 <- X2021 %>% 
  mutate(nome = codigos$nome) %>% 
  select(nome, `Homicídio  Doloso`, everything())

X2021 <- X2021[, -3]



#UNINDO NOVAMENTE TODAS AS BASES DE ANOS DOS CRIMES
#EM APENAS UMA GERAL CHAMADA DE: CRIMES
crimes <- list(X2008, X2009, X2010, X2011, X2012, X2013,
               X2014, X2015, X2016, X2017, X2018, X2019,
               X2020, X2021) %>% 
  bind_rows()



#SELECIONANDO AS COLUNAS DE INTERESSE DO DF CRIMES
bd_crimes <- crimes %>% 
  select(nome, ano, Furtos, Roubos, 
         `Delitos Relacionados à Armas e Munições`, 
         `Entorpecentes - Tráfico`)



#RENOMEANDO COLUNAS DA BD_CRIMES
bd_crimes <- bd_crimes %>% 
  rename(furtos = Furtos,
         roubos = Roubos,
         delitos_armas = `Delitos Relacionados à Armas e Munições`,
         trafico = `Entorpecentes - Tráfico`)



#CRUZANDO A BD_CRIMES COM O CODIGOS
total_crimes <- bd_crimes %>% 
  left_join(codigos)



#REORDENANDO COLUNAS DO DF TOTAL_CRIMES
total_crimes <- total_crimes %>% 
  select(id_municipio ,nome, everything())



#EXPORTANDO A BASE TOTAL_CRIMES EM CSV
write.csv(total_crimes, 'total_crimes.csv')