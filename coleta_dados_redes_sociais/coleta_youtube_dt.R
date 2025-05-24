# abrindo as bibliotecas
pacman::p_load(rio, tidyverse, janitor, lubridate, writexl)

# abrindo o dataframe gerado no youtube data tools
youtube <- import('nath_financas.csv')

glimpse(youtube)

summary(youtube$publishedAt)


# delimitando o intervalo de tempo 

youtube_recorte_tempo <- youtube
youtube_recorte_tempo$publishedAt <- ymd_hms(youtube_recorte_tempo$publishedAt)

# Filtrando os dados dentro do intervalo
youtube_recorte_tempo <- youtube_recorte_tempo %>% 
  filter(publishedAt >= "2024-08-01 00:00:00" & publishedAt <= "2024-11-13 00:00:00")

# eliminando colunas indiferentes
youtube_recorte_tempo <- youtube_recorte_tempo %>% 
  select(-latitude, -longitude, -favoriteCount, -dislikeCount, -locationDescription,
         -defaultLanguage, -caption, -definition, -dimension, -publishedAtSQL, -defaultLAudioLanguage)


# salve em um objeto novo com o nome do influencer ex. ian_neves_youtube_ytb <- youtube_recorte_tempo

write_xlsx(youtube_recorte_tempo, "nath_financas_ytb.xlsx")


