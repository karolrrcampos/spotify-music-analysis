################################################################################
# Fonte dataset: https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023/data
################################################################################
# Carregar pacotes


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

# Alguns valores da variável deezer music estão com virgulas e pontos, será feita a removação
library(stringr)
df <- df %>%
  mutate(deezer_playlists = str_remove_all(deezer_playlists, "[,\\.]"))

df$deezer_playlists <- as.integer(df$deezer_playlists)

# Separar os artista presente em 'artists' em colunas
#library(tidyr)
#streaming <- separate(df, col=artists, into = c("artist_1", "artist_2", "artist_3", "artist_4", "artist_5", "artist_6", "artist_7", "artist_8"), sep = ",")

# Novo df somente com os artistas
library(dplyr)
singer <- df
singer <- singer %>%
  separate_rows(artists, sep = ",|&")

singer <- data.frame(singer)

# Eliminar linha vazia da variável 'artists'
# singer <- singer[-1154, ]

# Remover espaços extras
singer$artists <- trimws(singer$artists)

unique(singer$artists)

# Corrigindo alguns nomes de artistas
singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Amitabha Bhattacharya", "Amitabh Bhattacharya"))

singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Anuel Aa", "Anuel AA"))

singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Peso P", "Peso Pluma"))

singer <- mutate(singer, artists = replace(singer$artists, singer$artists == "Selena G", "Selena Gomez"))

################################################################################
# Lançamentos por ano
library(plotly)
library(ggplot2)
count_year <- df %>%
  group_by(released_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplotly(
  ggplot(count_year, aes(x = released_year, y = count, color = count)) +
    geom_point(stat = "identity",
               aes(text = paste("Ano de Lançamento:", released_year,
                                "<br>",
                                "Lançamentos:", count)), size = 3) +
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

color <- c("#98fb98", "#77d37f", "#56ab65", "#35824c", "#145a32")

ggplotly(
  ggplot(count_week, aes(x = reorder(day_week_released, count), y = count, fill = count)) +
    geom_bar(stat = "identity",
             aes(text = paste("Dia de Lançamento:", day_week_released,
                              "<br>",
                              "Lançamentos:", count)), size = 3) +
    labs(title = "Distribuição dos Lançamentos Musicais por Dias da Semana",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

# 20 artistas com maior numero de lançamentos
count_artists <- singer %>%
  group_by(artists) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

color <- c("#d1f2eb", "#c7eae1", "#bde2d8", "#b3dace", "#a9d2c4", "#9fcaba", "#95c2b1", "#8bbaa7", "#81b29d", "#77aa93", "#6ea28a", "#649a80", "#5a9276", "#508a6c", "#468263", "#3c7a59", "#32724f", "#286a45", "#1e623c", "#145a32")

ggplotly(
  ggplot(count_artists, aes(x = count, y = reorder(artists, count), fill = count)) +
    geom_bar(stat = "identity",
             aes(text = paste("Artista:", artists,
                              "<br>",
                              "Lançamentos:", count)), size = 3) +
    labs(title = "Artistas Com Maior Número de Lançamentos",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

# Numero de playlists em plataformas streamings
count_spotfy <- df %>%
  group_by(music) %>%
  arrange(desc(spotify_playlists)) %>%
  head(20)

ggplotly(
  ggplot(count_spotfy, aes(x = spotify_playlists, y = reorder(music, spotify_playlists), fill = spotify_playlists)) +
    geom_bar(stat = "identity",
             aes(text = paste("Música:", music,
                              "<br>",
                              "Artistas:", artists,
                              "<br>",
                              "Presente em", spotify_playlists,"playlists")), size = 3) +
    labs(title = "Distribuição de Músicas em Playlists no Spotify",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

count_apple <- df %>%
  group_by(music) %>%
  arrange(desc(apple_playlists)) %>%
  head(20)

ggplotly(
  ggplot(count_apple, aes(x = apple_playlists, y = reorder(music, apple_playlists), fill = apple_playlists)) +
    geom_bar(stat = "identity",
             aes(text = paste("Música:", music,
                              "<br>",
                              "Artistas:", artists,
                              "<br>",
                              "Presente em", apple_playlists, "playlists")), size = 3) +
    labs(title = "Distribuição de Músicas em Playlists no Apple Music",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

count_deezer <- df %>%
  group_by(music) %>%
  arrange(desc(deezer_playlists)) %>%
  head(20)

ggplotly(
  ggplot(count_deezer, aes(x = deezer_playlists, y = reorder(music, deezer_playlists), fill = deezer_playlists)) +
    geom_bar(stat = "identity",
             aes(text = paste("Música:", music,
                              "<br>",
                              "Artistas:", artists,
                              "<br>",
                              "Presente em", deezer_playlists, "playlists")), size = 3) +
    labs(title = "Distribuição de Músicas em Playlists no Deezer Music",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)

# Quantas vezes as musicas foram tocadas no spotify
count_streams <- df %>%
  group_by(music) %>%
  arrange(desc(streams)) %>%
  head(20)

ggplotly(
  ggplot(count_streams, aes(x = streams, y = reorder(music, streams), fill = streams)) +
    geom_bar(stat = "identity",
             aes(text = paste("Música:", music,
                              "<br>",
                              "Artistas:", artists,
                              "<br>",
                              "Tocada", streams, "vezes no Spotify")), size = 3) +
    labs(title = "Distribuição de Músicas Mais Tocadas no Spotify",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = color),
  tooltip = "text"
)
################################################################################
