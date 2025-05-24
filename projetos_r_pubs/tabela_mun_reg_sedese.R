


# bibliotecas
pacman::p_load(rio, tidyverse, janitor, kableExtra, knitr, kable)


# importando a base
regionais <- import('municipios_diretoria_regional_sedese.xlsx')


glimpse(regionais)
view(regionais)

# visualizar municipios por regionais
regionais %>% 
  group_by(`DiretoriaRegional SEDESE `) %>% 
  summarise(contagem = n())


# mais detalhado
regionais %>% 
  group_by(`DiretoriaRegional SEDESE `) %>% 
  summarise(contagem = n()) %>% 
  kable(caption = "<center>Municípios por Diretoria Regional</center>", 
        align = "l",
        col.names = c('Diretoria Regional', 'Número de municípios')) %>% 
  kable_classic(html_font = 'Times New Roman', lightable_options = 'striped') %>% 
  kable_styling(full_width = F, font_size = 15) 


instlibrary(knitr)
library(kableExtra)





