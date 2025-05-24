# proxy > variaveis aproximadas de um determinado fenomeno

# colar de lado (left_join)

# colar por baixo  (bind_rows)


#exercicio
# pegar dados eleitorais de 2018 em mg
# criar prop votos do novo
# juntar com dados de desigualdade (gini)
# criar scatterplot desigualdade x NOVO 


# ABRINDO BIBLIOTECAS
pacman::p_load(rio, tidyverse, janitor, scales, geobr, electionsBR, codesBR)


# puxando os dados eleitorais mg 2018

mg2018 <- electionsBR::elections_tse(year = 2018, type = 'party_mun_zone', uf = 'MG')

# selecionando variaveis

mg2018_edit <- mg2018 %>% 
  select(municipio = NM_MUNICIPIO, codigo_mun = CD_MUNICIPIO, partido = SG_PARTIDO,
         estado = SG_UF, cargo = DS_CARGO, partido = SG_PARTIDO, votos_legenda = QT_VOTOS_LEGENDA_VALIDOS,
         votos_nominais = QT_VOTOS_NOMINAIS_VALIDOS, turno = NR_TURNO)


mg2018_edit <- mg2018_edit %>% 
  arrange(municipio)

# selecionando o cargo governador

mg2018_edit <- mg2018_edit %>% 
  filter(cargo == 'Governador', turno == 1)

# criando variavel total de votos 

mg2018_edit <-  mg2018_edit %>% 
  mutate(votos_partido = votos_legenda + votos_nominais)



# total de votos por municipio

mg2018_edit <- mg2018_edit %>% 
  group_by(municipio) %>% 
  mutate(voto_municipio = sum(votos_partido))

# porcentagem de votos do partido no municipio
mg2018_edit <- mg2018_edit %>% 
  mutate(percentual = votos_partido/voto_municipio)


# filtrando o NOVO
tabyl(mg2018_edit$partido)

novo_mg2018 <- mg2018_edit %>% 
  filter(partido == 'NOVO') %>% 
  arrange(municipio)

# removendo duplicatas
# alguns municipios se repetem por conta de ter varios cartorios eleitorais
# somei o percentual por municipio e retirei as duplicatas da variavel total percentual
novo_mg2018_edit2 <- novo_mg2018 %>% 
  group_by(municipio) %>% 
  mutate(percentual_total = sum(percentual)) %>% 
  filter(!(duplicated(percentual_total)))



# PUXANDO A BASE INDICE GINI
gini <- import('gini_2010.csv')


# convertendo a variavel em character
novo_mg2018_edit2 <- novo_mg2018_edit2 %>% 
  mutate(codigo_mun = as.character(codigo_mun))

# passando do tse para ibge a base de votação
novo_mg2018_edit2 <-  novo_mg2018_edit2 %>% 
  ibge_from_tse(codigo_mun)



### ERRO: NA BASE GINI > o codigo é do IBGE, mas está faltando o ultimo digito

novo_mg2018_edit2 %>% 
  filter(municipio %in% c('BELO HORIZONTE', 'CONTAGEM')) %>% 
  select(cod_ibge)

# BH = 310620
# CTG = 311860

gini %>% 
  filter(municipio %in% c('belo horizonte', 'contagem')) %>% 
  select(codigo)

# BH = 310620
# CTG = 311860

# retirando o ultimo numero da base de votação
# dessa forma as observações de ambas as colunas ficam iguais
novo_mg2018_edit2 <- novo_mg2018_edit2 %>% 
  mutate(cod_ibge = str_sub(cod_ibge, end = 6))


# deixando a coluna de referencia com o mesmo nome em ambas

gini <- gini %>% 
  rename(cod_ibge = codigo) 

gini_2 <- gini %>% 
  select(cod_ibge, gini)

# deixando as duas variaveis na mesma forma de dado
gini_2 <- gini_2 %>% 
  mutate(cod_ibge = as.character(cod_ibge))


novo_mg2018_edit2 <- novo_mg2018_edit2 %>% 
  left_join(gini_2, by = 'cod_ibge')

# até que enfim o scatterplot pqp


novo_mg2018_edit2 %>% 
  ggplot(aes(x = gini, y = percentual_total)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  labs(title = 'Relação desigualdade e votos no Zema por município',
       caption = 'Fonte: Pesquisa Interna - Elections-TSE',
       x = '',
       y = 'Percentual de votos') +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5)) +
  scale_y_continuous(labels = percent) +
  ggpubr::stat_cor() 


library(stargazer)

m1 <- lm(percentual_total ~ gini, data = novo_mg2018_edit2)

stargazer(m1, type = 'text')



