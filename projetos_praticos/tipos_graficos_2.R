# gráficos no R 

library(tidyverse)
library(rio)
library(ggplot2)

mpg <- mpg

mpg %>% 
  ggplot(mapping = aes(x = cty,
                       y = hwy,
                       colour = manufacturer,
                       shape = drv,
                       alpha = 1.5)) +
  geom_point() +
  labs(title = 'Modelos - relação km/l',
       caption = 'Fonte: Pesquisa Interna',
       x = "km/l na cidade",
       y = 'km/l na rodovia',
       colour = "Fabricante",
       alpha = "Escala tonalidade",
       shape = 'Modelos') +
  facet_wrap(~ class) +
  theme_gray() 



##################
#TIPOS DE GRÁFICO
##################

base_titanic <- import('titanic3 (1).xls')



###GRÁFICO DE BARRA###

# é um grafico que conta as categorias
# eixo y já é autodefinido contagem

base_titanic %>% 
  ggplot(mapping = aes(x = sex)) +
  geom_bar(colour = 'black', fill = 'pink') +
  labs(title = "Contagem Gênero",
       caption = "Fonte: Pesquisa Interna",
       x = "",
       y = '') +
  scale_x_discrete(labels = c('Mulheres', 'Homens'))

###GRÁFICO DE BARRA BIVARIADO###
# necessario falar para o R
# que pclass nao é numerico, é factor
base_titanic <- base_titanic %>% 
  mutate(sex = factor(sex),
         pclass = factor(pclass))

#plotando
base_titanic %>% 
  ggplot(mapping = aes(x = sex,
                       fill = pclass)) +
  geom_bar()


glimpse(base_titanic)

###GRÁFICO DE COLUNA###

# igual ao de barra, mas permite direcionar eixo y
# nesse caso, o eixo y será em porcentagem


base_titanic %>% 
  group_by(sex) %>% 
  summarise(contagem = n()) %>% 
  mutate(percentual = contagem/sum(contagem)) %>% 
  
  ggplot(mapping = aes(x = sex,
                       y = percentual)) +
  geom_col() +
  scale_y_continuous(label = percent)



###GRÁFICO BARRAS, FREQ. ABSOLUTA, DUAS VARIÁVEIS###

base_titanic %>% 
  group_by(sex, pclass) %>% 
  summarise(contagem = n()) %>% 
  mutate(percentual = contagem/sum(contagem)) %>% 
  
  ggplot(mapping = aes(x = sex,
                       y = percentual,
                       fill = pclass)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(label = percent) +
  scale_fill_brewer(palette = 'Dark2')










