####################################################################
####################################################################
###
### Raspagem INSTAGRAM influencers politicos - intervalo 01/08/2024 até 13/11/2024
### 
####################################################################
####################################################################


# tirar da notação cientifica
options(scipen = 999)

#install.packages(pacman)
#rodando os pacotes
#install.packages('pacman')
pacman::p_load(tidyverse, DataExplorer, scales, readxl, kableExtra, skimr,
               jsonlite, rio)


#Criar objeto para a base de dados
# dados_zc contem todas as informações coletadas pelo zc
dados_zc <- stream_in(file("nikolas_ferreira_insta.ndjson"))



#Criar objeto para a base com as colunas desejadas
insta_dados <- data.frame(i=1:nrow(dados_zc))


insta_dados$influencer_politico <- dados_zc$data$user$full_name
insta_dados$nome_de_usuario <- dados_zc$data$user$username

insta_dados$data_da_publicacao <- dados_zc$data$caption$created_at


insta_dados$texto_da_publicacao <- dados_zc$data$caption$text

insta_dados$id_do_video <- dados_zc$data$code

insta_dados <- insta_dados %>% 
  mutate(link_da_publicacao = paste("https://www.instagram.com/p/",id_do_video, sep = '', '/'))




insta_dados$top_likers <- dados_zc$data$top_likers

insta_dados$contagem_comentarios <- dados_zc$data$comment_count

insta_dados$contagem_curtidas <- dados_zc$data$like_count

summary(insta_dados$top_likers)













