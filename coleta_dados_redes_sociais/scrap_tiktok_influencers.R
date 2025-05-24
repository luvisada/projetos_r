
####################################################################
####################################################################
###
### Raspagem tiktok influencers politicos - intervalo 01/08/2024 até 13/11/2024
### 
####################################################################
####################################################################


# tirar da notação cientifica
options(scipen = 999)

#install.packages(pacman)
#rodando os pacotes
install.packages('pacman')
pacman::p_load(tidyverse, DataExplorer, scales, readxl, kableExtra, skimr,
               jsonlite, rio)


#Criar objeto para a base de dados
# dados_zc contem todas as informações coletadas pelo zc
dados_zc <- stream_in(file("joao_carvalho.ndjson"))

#Criar objeto para a base com as colunas desejadas
tk_dados <- data.frame(i=1:nrow(dados_zc))


###########################################
#
#  Selecionar as colunas desejadas
#
###########################################



# nome do influencier / biografia no perfil
tk_dados$influencer_politico <- dados_zc$data$author$nickname
tk_dados$biografia <- dados_zc$data$author$signature



# criando o link para direcionar direto para o video no tiktok
# infelizmente o link para o vídeo nao funciona do modo que o zc coleta os dados
# portanto foi preciso pegar a o link da url junto com o id do video e juntar usando o paste
tk_dados$legenda_video <- dados_zc$data$desc
tk_dados$link_perfil <- dados_zc$source_platform_url
tk_dados$video_id <- dados_zc$item_id

tk_dados$link_video <- paste(tk_dados$link_perfil, "/video/", tk_dados$video_id, sep="")


#######################################
#
# Data da coleta - Manipulacoes necessarias
#
######################################

# puxando a data de publicacao
# estava inicialmente no formato segundos a partir de 1970 > precisa convertar para o formato d/m/a
tk_dados$data_publi <- dados_zc$data$createTime
tk_dados$data_publi_convertido <- format(as.POSIXct(tk_dados$data_publi, origin="1970-01-01", tz="UTC"), "%d/%m/%Y")
# por fim foi preciso converveter para o formato Data no R. Essa linha vai ser importante depois 
# para filtrar o intervalo das postagens
tk_dados$data_publi_convertido_2 <- as.Date(tk_dados$data_publi_convertido, format = "%d/%m/%Y")


#######################################
#
# Informações do post em específico
#
######################################
tk_dados$duracao_video <- dados_zc$data$video$duration
tk_dados$curtidas_no_post <- dados_zc$data$statsV2$diggCount
tk_dados$comentarios_no_post <- dados_zc$data$statsV2$commentCount
tk_dados$visualizacoes_post <- dados_zc$data$statsV2$playCount
tk_dados$compartilhamentos_post <- dados_zc$data$statsV2$shareCount

#######################################
#
# Informações gerais do perfil
#
######################################
tk_dados$seguidores_conta <- dados_zc$data$authorStats$followerCount
tk_dados$seguindo <- dados_zc$data$authorStats$followingCount
tk_dados$numero_curtidas_total <- dados_zc$data$authorStats$heart
tk_dados$total_publicacoes <- dados_zc$data$authorStats$videoCount



# repassando as informacoes para um data frame em especifico do influencer
# salvando a raspagem completa
joao_carvalho_completo <- tk_dados
write_xlsx(joao_carvalho_completo, 'joao_carvalho_completo.xlsx')



###########################################################
#
# FILTRANDO PARA O INTERVALO DE 01/08/2024 ATÉ 13/11/2024
#
###########################################################


# filtrando para as datas entre 01/08/2024 até 13/11/2024
# Converter a coluna 'data_publi_convertido' para o formato Date
joao_carvalho_recorte_tempo <- joao_carvalho_completo %>%
  filter(data_publi_convertido_2 >= as.Date("2024-08-01") & data_publi_convertido_2 <= as.Date("2024-11-13"))


write_xlsx(joao_carvalho_recorte_tempo, 'joao_carvalho_recorte_tempo.xlsx')



# QUANDO TIVER VÁRIOS DATA FRAMES PRONTOS, JUNTA TODOS EM UM DATA FRAME SÓ PARA DEPOIS BAIXAR EM XLSX

# apos repetir o script para cada influencer e tiver o objeto pronto, junto todos com esse comando abaixo


### juntando as bases de dados



##############################################
#
#
# JUTANDO DATAFRAMES NO R
#
#
##############################################

#esquerda_tiktok <- rbind(chavoso, debora_baldin, felipe_neto, gustavo_gaiofato, ian_neves,
#                         jones_manoel, laura_sabino, nath_financas, rita_von_hunty)







