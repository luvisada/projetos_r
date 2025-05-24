####################################################################
####################################################################
###
### Raspagem TWITTER/X influencers politicos - intervalo 01/08/2024 até 13/11/2024
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
dados_zc <- stream_in(file("lula_twitter.ndjson"))



#Criar objeto para a base com as colunas desejadas
tk_dados <- data.frame(i=1:nrow(dados_zc))



