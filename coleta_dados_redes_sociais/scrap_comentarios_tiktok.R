# Raspagem tiktok influencers cientificos

#install.packages(pacman)
pacman::p_load(tidyverse, DataExplorer, scales, readxl, kableExtra, skimr,
               jsonlite)

#Criar objeto para a base de dados
dados_zeeschuimer <- stream_in(file("ian_neves_c1.ndjson"))

#Criar objeto para a base com as colunas desejadas
tk_data <- data.frame(i=1:nrow(dados_zeeschuimer))

#Selecionar as colunas desejadas

tk_data$source_url <- dados_zeeschuimer$source_url

tk_data$usuario <- dados_zeeschuimer$data$share_info$desc

tk_data$comentario <- dados_zeeschuimer$data$text



#Salvar o arquivo em excel
library(writexl)
write_xlsx(tk_data, path = "ianneves_tk.xlsx")



