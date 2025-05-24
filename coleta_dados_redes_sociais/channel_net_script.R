pacman::p_load(rio, tidyverse, igraph)

channel_list <- import('channel_list_donald_trump.csv')
view(channel_list)

# leitura_arquivo
channel_network <- "channel_net_donald_trump.gdf"
file.exists(channel_network)


# Read the .gdf file into an igraph object
grafo <- read_graph(channel_network, format = 'gml')

