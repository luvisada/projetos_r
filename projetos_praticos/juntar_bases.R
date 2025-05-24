## ===============================================
##
##      Aula 14 - Juntar Bases de Dados
##
## ===============================================

## ===============================================================
##                  MONTANDO BASES DE DADOS
## ===============================================================

library(tidyverse)
library(rio)
library(janitor)
library(electionsBR)

# dicas electionsBR# dicas ---------------------------------------
# 1) juntar por baixo: bind_rows.
# observacoes adicionais (linhas)
# mesmo nome, mesmas colunas, mesmo tipo.
# 2) juntar pelo lado: left_join
# variaveis adicionais (colunas).
# mesmo nome, mesmo tipo da variavel comum.

# baixar dados 2014 e 2018 em MG ---------------------------------

dados14 <- elections_tse(year = 2014, type = "party_mun_zone", 
                         uf = "MG")

dados18 <- elections_tse(year = 2018, type = "party_mun_zone", 
                         uf = "MG")

# selecionar somente algumas variaveis --------------------------

dados14 <- dados14 %>% 
  select(partido = SG_PARTIDO, votos_nom = QT_VOTOS_NOMINAIS, 
         cargo = DS_CARGO, uf = SG_UF, 
         cod_mun = CD_MUNICIPIO, nome_mun = NM_MUNICIPIO) 

### como criar uma variavel pro ano?


### como filtrar somente governador?


# fazer o mesmo com 2018 ----------------------------------------

dados18 <- dados18 %>% 
  select(partido = SG_PARTIDO, votos_nom = QT_VOTOS_NOMINAIS_VALIDOS, 
         cargo = DS_CARGO, uf = SG_UF, 
         cod_mun = CD_MUNICIPIO, nome_mun = NM_MUNICIPIO) %>% 
  mutate(ano = 2018)  %>% 
  filter(cargo == "Governador")

### bases tem mesmo nome e mesmas colunas?

names(dados14)

names(dados18)

### bases tem mesmo tipo?

glimpse(dados14)

glimpse(dados18)

## ok, entao juntar

dadostotais <- bind_rows(dados14, dados18)

## como saber se esta certo?

#### 1) contagem das obs
#### dados14 = 5737
#### dados18 = 8670
#### total = 5737 + 8670 = 14407

#### 2) verificacao base
#### organizar por municipio e ano


#### como trazer cod_mun e ano pro inicio?


# e se os tipos das variaveis fossem diferentes? -------------


#### como faco para converter variavel ano para character?


#### como faco para juntar as bases?


## ------------------------------------------
## Juntar pelo Lado
## Usar left-join

# incluir PIB 

pib_2018 <- import("pib_2018.xlsx")


# 1o problema: cod_mun tse != cod_mun ibge
# pacote codesbr pode nos ajudar

devtools::install_github("meirelesff/codesBR")

library(codesBR)

dados14 <- dados14 %>% 
  ibge_from_tse(cod_mun)

# logo, transformar em character

dados14 <- dados14 %>% 
  mutate(cod_mun = as.character(cod_mun)) %>% 
  ibge_from_tse(cod_mun)

# temos os mesmos nomes?

names(dadostotais)

names(pibs)

## como modificar nome?

pib_2018 <- pib_2018 %>% 
  rename(cod_ibge = cod_mun)


dados14 %>% 
  left_join(pib_2018, by = c("cod_ibge"))


## ------------------------------------------
## importar base de dados geograficos

library(geobr)

mapa_mg2014 <- read_municipality(year = 2014, 
                                 code_muni = "MG") %>% 
  mutate(ano = 2014)

mapa_mp2018 <- read_municipality(year = 2018, 
                                 code_muni = "MG") %>% 
  mutate(ano = 2018)


# juntar por baixo ------------

mapa_mg <- mapa_mg2014 %>% 
  bind_rows(mapa_mp2018)  

mapa_mg <- mapa_mg %>% 
  select(-c(code_state, abbrev_state, name_muni))


# juntar pelo lado ------------

mapa_mg <- mapa_mg %>% 
  rename(cod_ibge = code_muni)

vot_2014_2018 <- vot_2014_2018 %>% 
  left_join(mapa_mg, by = c("cod_ibge", "ano"))

# como resolver?

glimpse(vot_2014_2018)
glimpse(mapa_mg)

vot_2014_2018 <- vot_2014_2018 %>% 
  mutate(cod_ibge = as.double(cod_ibge)) %>% 
  left_join(mapa_mg, by = c("cod_ibge", "ano"))



## ===========================
## Exercicio do dia
## 1) pegar dados 2018 de mg, 
## 2) criar proporcao de votos PSL
## 3) juntar com dados de desigualdade
## 4) criar um scatterplot com desigualdade x votos PSL



