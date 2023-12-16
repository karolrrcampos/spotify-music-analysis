################################################################################
# Fonte dataset: https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023/data
################################################################################
# Carregar pacotes
library(stringr)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)

# Gradiente de cores para os gráficos
color <- c("#d1f2eb", "#c7eae1", "#bde2d8", "#b3dace", "#a9d2c4", "#9fcaba", "#95c2b1", "#8bbaa7", "#81b29d", "#77aa93", "#6ea28a", "#649a80", "#5a9276", "#508a6c", "#468263", "#3c7a59", "#32724f", "#286a45", "#1e623c", "#145a32")

# Carregar dataset
df <- read.csv("music_streaming.csv", 
               sep = ",",
               dec = ".")
names(df)

# Eliminar linha vazia da variável 'music'
df <- df[-54, ]

# Valores ausentes e NAs
sum(is.na(df))
sum(is.null(df))

# Remoção de pontos e virgula dos valores da variável deezer_music
df <- df %>%
  mutate(deezer_playlists = str_remove_all(deezer_playlists, "[,\\.]"))

df$deezer_playlists <- as.integer(df$deezer_playlists)

# Novo df com a variável artists separada em linhas
singer <- df
singer <- singer %>%
  separate_rows(artists, sep = ",|&")

singer <- data.frame(singer)

# Remover espaços extras
singer$artists <- trimws(singer$artists)

unique(singer$artists)

# Correção nomes de artistas
singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Amitabha Bhattacharya", "Amitabh Bhattacharya"))

singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Anuel Aa", "Anuel AA"))

singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Peso P", "Peso Pluma"))

singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Selena G", "Selena Gomez"))

################################################################################
# Lançamentos por ano
count_year <- df %>%
  group_by(released_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplotly(
  ggplot(count_year, aes(x = released_year, y = count, color = count)) +
    geom_point(stat = "identity",
               aes(text = paste("Em", released_year, "houveram", count, "lançamentos")), size = 3) +
    scale_color_gradient(low = "#98FB98", high = "#145A32") +
    labs(title = "Distribuição Anual de Lançamentos Musicais",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(1930, 2030, 10)), 
  tooltip = "text"
)

# Lançamentos por dia da semana
count_week <- df %>%
  group_by(day_week_released) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplotly(
  ggplot(count_week, aes(x = reorder(day_week_released, count), y = count, fill = count)) +
    geom_bar(stat = "identity",
             aes(text = paste(count, "lançamentos")), size = 3, width = 0.7) +
    labs(title = "Distribuição dos Lançamentos por Dias da Semana",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

# Número de lançamentos por artistas
count_artists <- singer %>%
  group_by(artists) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

ggplotly(
  ggplot(count_artists, aes(x = count, y = reorder(artists, count), fill = count)) +
    geom_bar(stat = "identity",
             aes(text = paste(count, "lançamentos")), size = 3) +
    labs(title = "Distribuição de Lançamentos por Artistas",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

# Numero de playlists em plataformas streamings
count_spotfy <- df[ ,c(2,10)] %>%
  group_by(music) %>%
  arrange(desc(spotify_playlists)) %>%
  head(20)

ggplotly(
  ggplot(count_spotfy, aes(x = spotify_playlists, y = reorder(music, spotify_playlists), fill = spotify_playlists)) +
    geom_bar(stat = "identity",
             aes(text = paste("Presente em", spotify_playlists,"playlists")), size = 3) +
    labs(title = "Distribuição de Músicas em Playlists no Spotify em 2023",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

count_apple <- df[ ,c(2,13)] %>%
  group_by(music) %>%
  arrange(desc(apple_playlists)) %>%
  head(20)

ggplotly(
  ggplot(count_apple, aes(x = apple_playlists, y = reorder(music, apple_playlists), fill = apple_playlists)) +
    geom_bar(stat = "identity",
             aes(text = paste("Presente em", apple_playlists,"playlists")), size = 3) +
    labs(title = "Distribuição de Músicas em Playlists no Apple Music em 2023",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

count_deezer <- df[ ,c(2,15)] %>%
  group_by(music) %>%
  arrange(desc(deezer_playlists)) %>%
  head(20)

ggplotly(
  ggplot(count_deezer, aes(x = deezer_playlists, y = reorder(music, deezer_playlists), fill = deezer_playlists)) +
    geom_bar(stat = "identity",
             aes(text = paste("Presente em", deezer_playlists, "playlists")), size = 3) +
    labs(title = "Distribuição de Músicas em Playlists no Deezer Music em 2023",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

# Quantas vezes as musicas foram tocadas no spotify
count_streams <- df[ ,c(2,12)] %>%
  group_by(music) %>%
  arrange(desc(streams)) %>%
  head(20)

ggplotly(
  ggplot(count_streams, aes(x = streams, y = reorder(music, streams), fill = streams)) +
    geom_bar(stat = "identity",
             aes(text = paste(music, "tocou", streams, "vezes na plataforma")), size = 3) +
    labs(title = "As Músicas Mais Tocadas no Spotify em 2023",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)
################################################################################