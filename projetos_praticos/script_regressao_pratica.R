## ========================================
##
##      REGRESSAO AULA PRATICA 
##
## ========================================

pacman::p_load(rio, tidyverse, modelsummary, modelr, stargazer, marginaleffects)

## 1) abra a base dos salarios, 
salarios <- import('income.csv')



## transforme a variavel educacao em ensino fundamental, 
## ensino medio, pos-graduacao. 

salarios_edit <- salarios %>% 
  mutate(educacao = case_when(educ %in% c(0,9)~ 'fundamental',
                              educ %in% c(10,12) ~ 'medio',
                              educ %in% c(13, 16) ~ 'graduacao',
                              TRUE ~ 'pos-grad'))


## rode uma regressao com a variavel modificada +
## afqt (exame de habilidade/inteligencia)

m1 <- lm(income ~ afqt + educacao, data = salarios_edit)

stargazer(m1, type = 'text')

salarios_edit %>% 
  ggplot(aes(x = reorder(educacao, income), income)) +
  geom_boxplot() 

## interprete os coeficientes com o pacote 
## "modelsummary".

modelsummary(m1, output = "flextable")

# intercept > desconsiderando as variaveis educacao, as pessoas tendem a ganahr
# 25.978

# afqt > uma unidade de inteligencia impacta 546 no salario

# educacao graduacao aumenta em media 11.294 no salario 

# educacao medio nao tem significancia estatistica pois a margem de erro é o dobro do valor do coeficiente

# educacao pos grad impacta em media 11.603 na renda anual








## 2) abra a base do eseb 2022, 

eseb22 <- import('eseb_2022.sav')


## selecione as variaveis q04a, sexo, raca, 
## idade, renda e escolaridade. 

eseb_edit <- eseb22 %>% 
  select(democracia = Q04a, sexo = D02, raca = D12a, idade = D01A_IDADE)
glimpse(eseb_edit)


eseb_edit <- eseb_edit %>% 
  mutate(raca2 = case_when(raca == 3 ~ 'branco',
                           raca %in% c(97, 98) ~ NA,
                           TRUE ~ 'nao branco'))

eseb_edit <- eseb_edit %>% 
  filter(!is.na(raca2))

## renomear a variavel q04a para "democracia". 
## rode uma regressao com a variavel democracia
## como variavel dependente e todo o restante
## como variavel independente.

m2 <- lm(democracia ~ factor(sexo) + factor(raca2) + idade, data = eseb_edit)

stargazer(m2, type = 'text')

glimpse(eseb_edit)
# ser ou nao branco até entao NAO possui significancia estatistica



## corrija a variavel democracia para que 
## coeficientes altos representem NAs.
library(janitor)
tabyl(eseb_edit$democracia)

eseb_edit <- eseb_edit %>% 
  filter(!(democracia %in% c(97, 98)))




## interprete os 
## coeficientes das variaveis com significancia
## estatistica.


m3 <- lm(democracia ~ factor(sexo) + factor(raca2) + idade, data = eseb_edit)

stargazer(m3, type = 'text')

# ser mulher (2) aumenta em media 0.258 a pessoa ser a favor

modelsummary(m3, output = 'flextable')


