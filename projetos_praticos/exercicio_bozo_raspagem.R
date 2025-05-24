pacman::p_load(tidyverse, janitor, rvest, stringr, abjutils, tm, tidytext, rio, rslp,
               patchwork, ggwordcloud)


# abrindo a base de dados

base_bozo <- import('discursos_bolsonaro.csv')

# selecionando a variavel discurso
discursos_bozo <- base_bozo %>% 
  select(discurso = discurso_1) %>% 
  slice(1:500)


# passando as palavras para caixa baixa
discursos_bozo <- discursos_bozo %>% 
  mutate(discurso = str_to_lower(discurso))

# removendo acentos e pontuação
discursos_bozo <- discursos_bozo %>% 
  mutate(discurso = removePunctuation(discurso),
         discurso = rm_accent(discurso))

# removendo numeros
discursos_bozo <- discursos_bozo %>% 
  mutate(discurso = removeNumbers(discurso))

# separando as observações por palavras
discursos_bozo_1 <- discursos_bozo %>% 
  unnest_tokens(word, discurso)

# primeiro gráfico
grafico_1 <- discursos_bozo_1 %>% 
  count(word) %>%
  arrange(-n) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Discurso com stop words',
       x = '', 
       y = '') +
  coord_flip()


# fazendo o gráfico removendo as STOP WORDS

discursos_bozo_2 <- discursos_bozo_1 %>%
  count(word) 

discursos_bozo_2 <- discursos_bozo_2 %>% 
  filter(!(word %in% stopwords(kind = 'portuguese'))) %>% 
  arrange(-n)

# removendo mais STOP WORDS

# abrindo o pacote ptstem

discursos_bozo_2 <- discursos_bozo_2 %>% 
  filter(!(word %in% c('nao', 'sr', 'presidente', 'aqui', 'bolsonaro', 'deputado',
                       'jair', 'porque', 'agora', 'brasil', 'orador', 'revisao', 'ha',
                       'obrigado', 'quero', 'sao', 'ser', 'vai', 'anos', 'entao',
                       'la', 'tambem', 'vexa', 'ja', 'estao', 'ate', 'so', 'pode',
                       'falar', 'ter', 'hoje', 'vamos', 'dia', 'casa', 'contra',
                       'projeto', 'bem', 'sobre', 'ordem', 'questao',
                       'vou',
                       "de", "a", "o", "que", "e", "é", "do", "da",
                       "em", "um", "para", "com", "não", "uma", "os", "no",
                       "se", "na", "por", "mais", "as", "dos", "como", "mas",
                       "ao", "ele", "das", "à", "seu", "sua", "ou", "quando",
                       "muito", "nos", "já", "eu", "também", "só", "pelo", "pela",
                       "até", "isso", "ela", "entre", "depois", "sem", "mesmo", "aos",
                       "seus", "quem", "nas", "me", "esse", "eles", "você", "essa",
                       "num", "nem", "suas", "meu", "às", "minha", "numa", "pelos",
                       "elas", "qual", "nós", "lhe", "deles", "essas", "esses", "pelas",
                       "este", "dele", "tu", "te", "vocês", "vos", "lhes", "meus",
                       "minhas", "teu", "tua", "teus", "tuas", "nosso", "nossa", "nossos",
                       "nossas", "dela", "delas", "esta", "estes", "estas", "aquele", "aquela",
                       "aqueles", "aquelas", "isto", "aquilo", "estou", "está", "estamos", "estão",
                       "estive", "esteve", "estivemos", "estiveram", "estava", "estávamos", "estavam", "estivera",
                       "estivéramos", "esteja", "estejamos", "estejam", "estivesse", "estivéssemos", "estivessem", "estiver",
                       "estivermos", "estiverem", "hei", "há", "havemos", "hão", "houve", "houvemos",
                       "houveram", "houvera", "houvéramos", "haja", "hajamos", "hajam", "houvesse", "houvéssemos",
                       "houvessem", "houver", "houvermos", "houverem", "houverei", "houverá", "houveremos", "houverão",
                       "houveria", "houveríamos", "houveriam", "sou", "somos", "são", "era", "éramos",
                       "eram", "fui", "foi", "fomos", "foram", "fora", "fôramos", "seja",
                       "sejamos", "sejam", "fosse", "fôssemos", "fossem", "for", "formos", "forem",
                       "serei", "será", "seremos", "serão", "seria", "seríamos", "seriam", "tenho",
                       "tem", "temos", "tém", "tinha", "tínhamos", "tinham", "tive", "teve",
                       "tivemos", "tiveram", "tivera", "tivéramos", "tenha", "tenhamos", "tenham", "tivesse",
                       "tivéssemos", "tivessem", "tiver", "tivermos", "tiverem", "terei", "terá", "teremos",
                       "terão", "teria", "teríamos", "teriam")))

discursos_bozo_2 <- discursos_bozo_2 %>% 
  filter(!(word %in% c('sendo', 'fazer', 'caso', 'nada', 'voces', 'inclusive',
                       'todos')))




# pacote rslp para remover radicais iguais
#library(rslp)
#library(SnowballC)

#discursos_bozo_sem_radicais <- discursos_bozo_2 %>% 
#  select(word) %>% 
#  mutate(word2 = rslp(steprules = 'pt', word))

#discursos_bozo_sem_radicais <- discursos_bozo_2 %>%
#  mutate(word2 = wordStem(word, language = "pt"))

  
  
# criando o grafico 2
grafico_2 <- discursos_bozo_2 %>%
  filter(!is.na(word)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = 'identity', colour = 'gray10', fill = 'gray70') +
  labs(title = 'Discurso sem stop words',
       x = '',
       y = '') +
  coord_flip()


### comparando as duas formas (com e sem stop words)

library(patchwork)

grafico_1+grafico_2


#########################
#
# criando nuvem de palavras
#
#######################
library(ggwordcloud)

discursos_bozo_2 %>% 
  slice(1:100) %>% 
  arrange(-n) %>% 
  ggplot(aes(label = word, size = 2*n, color = word)) +
  geom_text_wordcloud(rm_outside = T, shape = 'square') +
  theme_minimal() +
  scale_size_area(max_size = 8) 


################################################################
#
# EFETUANDO A REMOCAO DAS STOP WORDS USANDO O CHAT GPT E A FUNCAO ANTI JOIN
#
################################################################


# criando um objeto que contenha as stop words geradas no ChatGPT
stopwords_pt <- c("vamos", "todos", "nós", "eles", "elas", "e", "ou", "mas",
                  "para", "por", "com", "de", "em", "no", "na", "se",
                  "que", "como", "quando", "porque", "porquê", "qual", "quem", "onde",
                  "quando", "ainda", "mais", "menos", "muito", "pouco", "bem", "mal",
                  "assim", "mesmo", "apenas", "só", "também", "ainda", "agora", "antes",
                  "depois", "enquanto", "logo", "primeiro", "segundo", "último", "sempre", "nunca",
                  "jamais", "talvez", "certamente", "provavelmente", "possivelmente", "claro", "óbvio", "absolutamente",
                  "realmente", "verdadeiramente", "efetivamente", "eficazmente", "basicamente", "principalmente", "especialmente", "particularmente",
                  "deveríamos", "precisamos", "teremos", "vamos", "devemos", "queremos", "podemos", "iremos",
                  "será", "seremos", "serão", "estamos", "estaremos", "estão", "estavam", "estiveram",
                  "havíamos", "haveremos", "haverão", "havia", "haviam", "há", "haverá", "haveriam",
                  "tínhamos", "teríamos", "teriam", "tinham", "temos", "teremos", "tiveram", "têm",
                  "tenhamos", "tenham", "teve", "tinha", "tivesse", "tenha", "tenham", "teria",
                  "tivesse", "teriam", "seja", "sejam", "sejamos", "sejamos", "fosse", "fossem",
                  "foram", "sejam", "sejamos", "fosse", "fossem", "foram", "sendo", "sido",
                  "poderia", "poderiam", "poderá", "poderão", "pode", "podem", "pôde", "puderam",
                  "puder", "pudermos", "puderem", "deve", "devem", "deveria", "deveriam", "deverá",
                  "deverão", "deveríamos", "devesse", "devessem", "dever", "deveríamos", "faz", "faça",
                  "façamos", "fazemos", "fizeram", "fazia", "fizeram", "fizerem", "fará", "farão",
                  "fazer", "fiz", "fizer", "fizeram", "fizera", "fizermos", "fizerem", "fizeria",
                  "façamos", "haja", "hajam", "hajamos", "hajam", "houve", "houveram", "havemos",
                  "houvermos", "houvesse", "houvessem", "houvesse", "haver", "haverá", "haveremos", "haverão",
                  "haveria", "haveriam", "outra", "outras", "outro", "outros", "alguma", "algumas",
                  "algum", "alguns", "muita", "muitas", "muito", "muitos", "pouca", "poucas",
                  "pouco", "poucos", "todo", "todos", "toda", "todas", "nenhum", "nenhuma",
                  "nenhuns", "nenhumas", "cada", "cada um", "cada uma", "várias", "vários",
                  "várias", "vários", "mesmo", "mesma", "mesmos", "mesmas", "próprio", "própria",
                  "próprios", "próprias", "tão", "tanta", "tantos", "tantas", "tudo", "nada",
                  "coisa", "coisas", "qualquer", "quaisquer", "todo mundo", "todos nós", "todos vocês", "cada um de nós",
                  "cada um de vocês", "ambos", "ambas", "todo o mundo", "nenhuma pessoa", "nenhuma delas", "nenhum de nós",
                  "quem quer que", "qualquer um", "alguém", "ninguém", 'nao',
                  'sr', 'presidente', 'aqui', 'orador', 'revisao', 'sao', 'ser',
                  'quero', 'bolsonaro', 'deputado', 'vai', 'ha', 'obrigado')

# criando o data frame
stopwords_pt <- as.data.frame(stopwords_pt)

# aqui renomeei a coluna para word para ficar igual nos dois objetos
stopwords_pt <- stopwords_pt %>%
  rename(word = stopwords_pt)

# usar a funcao antijoin para remover as stop words do discurso
discursos_bozo_3 <- discursos_bozo_1

discursos_bozo_3 <- discursos_bozo_3 %>% 
  anti_join(stopwords_pt, by = 'word')

discursos_bozo_3 <- discursos_bozo_3 %>% 
  arrange(-n)


# aqui gerei um data frame das stop words do pacote tm, repetirei o processo anterior
stopwords_tm <- data_frame(stopwords(kind = 'pt'))  

stopwords_tm <- stopwords_tm %>% 
  rename(word = `stopwords(kind = "pt")`)

discursos_bozo_3 <- discursos_bozo_3 %>% 
  anti_join(stopwords_tm, by = 'word') %>% 
  arrange(-n)



#plotandoooo

discursos_bozo_3 %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = 'identity')





library(tm)
library(abjutils)
library(tidytext)



#################################
#
# RASPAGEM DE DADOS
#
#################################

# selectorgadget > extensao do chrome

# obsidian > revisao literatura / análise em redes

# texto em latex




