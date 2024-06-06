#install.packages(c('rio', 'tidyverse', 'janitor', 'scales', 'kableExtra',
#                  'knitr'))

### BIBLIOTECAS
library(rio)
library(janitor)
library(tidyverse)
library(scales)
library(dplyr)





#### BASE DE DADOS

titanic <- rio::import('titanic3 (1).xls')


### TESTE DE HIPÓTESES
# hipotese nula > quando um fenomeno nao altera outro fenomeno
# independentes

#hipotese alternada > um fenomeno influencia outro fenomeno
# dependentes


##########################
### TESTE DE p - padrao###
##########################

# visualizar se possui correlação
# mais proximo de 1 > possui correlação

mean(titanic$age, na.rm = T)
t.test(titanic$age ~ titanic$sex)


#######################################
### TESTE T para diferenca de medias###
#######################################

# primeira variavel em relacao a segunda
t.test(titanic$age ~ titanic$sex)

# true difference in means between group female and group male is not equal to 0
# existe uma diferenca significante


#########################################
### TESTE QUI - QUADRADO (Chi-Square) ###
#########################################

# verificar a diferença do esperado e do observado

titanic %>% 
  group_by(sex, survived) %>% 
  summarise(contagem = n())

# criando o teste

chisq_sex_surv <- chisq.test(titanic$survived, titanic$sex)


# verificar as diferencas do observado e do esperado
chisq_sex_surv$observed
chisq_sex_surv$expected

# analisando >> as mulheres sobreviveram mais que
# os homens 
# ou seja, nao é uma hipotese nula: existe sim uma...
# correlacao entre sobreviventes e sexo


#############
### ANOVA ###
#############

# extensão do test T, para quando se tem mais do que tres grupos

#visualizando a distribuicao
titanic <- titanic %>% 
  mutate(pclass = factor(pclass))

titanic %>% 
  ggplot(mapping = aes(x = pclass,
                       y = age,
                       colour = pclass)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Dark2')

# teste de hipote anova

anova_test <- aov(titanic$age ~ titanic$pclass)


# *** > alta correlacao entre idade e pclass
# 2e-16 > 16 zeros 
summary(anova_test)



##################
### CORRELACAO ###
##################
library(ggpubr)

data("USArrests")

cor.test(USArrests$Murder, USArrests$Assault)

# resultado> p-value = 2.596e-12
# ou seja, extremamente relacionado
# reta será diretamente proporcional

USArrests %>% 
  ggplot(mapping = aes(x = Murder,
                       y = Assault)) +
  geom_point() +
  stat_cor() +
  geom_smooth(method = 'lm', se = F)



################################
### REGRESSAO LINEAR SIMPLES ###
################################

murder_assault_lm <- lm(Murder ~ Assault,
                        data = USArrests)
summary(murder_assault_lm)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.631683     
#Assault     0.041909 

# EXPLICANDO::  
# a ocorrencia é de 0.63 de assassinato
# cada nova ocorrencia aumentará 0.041 a taxa 
# de assassinato



#################################
### REGRESSAO LINEAR MULTIPLA ###
#################################

murder_assault_lm_pop <- lm(Murder ~ Assault + UrbanPop,
                        data = USArrests)

summary(murder_assault_lm_pop)






