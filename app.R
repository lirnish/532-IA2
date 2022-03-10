library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

genres <- c("pop","rap","rock","latin","r&b","edm")

app$layout(
    dbcContainer(
        list(
            htmlH1('Dashr heroku deployment'),
            dccGraph(id='plot_2'),
            htmlBr(),
            htmlLabel('Popularity Slider'),
            dccRangeSlider(
                id = 'pop-slider',
                min = 0,
                max = 100,
                marks = list("0"="0", "25"="25","50"="50","75"="75","100"="100"),
                value = list(5,100)
            ),
            htmlBr(),
            htmlLabel('Music Genre Checklist'),
            # dccChecklist(
            #     id = 'genre_checklist',
            #     options = list(
            #         list("label" = "rock", "value" = "rock"),
            #         list("label" = "pop", "value" = "pop"),
            #         list("label" = "rap", "value" = "rap"),
            #         list("label" = "latin", "value" = "latin"),
            #         list("label" = "r&b", "value" = "r&b"),
            #         list("label" = "edm", "value" = "edm")
            #     ),
            #     value = list("rock","pop"),
            #     labelStyle = list("display":"block",
            #                       "margin-left": "10px")
            # ),
            dccDropdown(
                id='genre-select',
                options = genres %>% purrr::map(function(genre, pop) list(label = genre, value = genre)),
                value=list("rock","pop"),
                multi=TRUE),
            dccDropdown(
              id='artist_names',
              # options = genres %>% purrr::map(function(genre, pop) list(label = genre, value = genre)),
              value=list("Queen","The Cranberries", "Calvin Harris", "David Guetta", "The Chainsmokers"),
              multi=TRUE)
            
        )
    )
)

app$callback(
  output("artist_names", "options"),
  list(input('genre-select', 'value'),
       input('pop-slider','value')),
function(genre, pop){
  pop_min <- pop[1]
  pop_max <- pop[2]
  suggested_list  <-  df %>% drop_na() %>%
    filter(playlist_genre %in% genre) %>%
    filter(track_popularity >= pop_min & track_popularity <= pop_max)%>%
    group_by(track_artist) %>%
    summarize(count=n())%>%
    arrange(desc(count))%>%
    pull(track_artist)
  
  result <- suggested_list %>% purrr::map(function(track_artist) list(label = track_artist, value = track_artist))
  result
  }
)
app$callback(
    output('plot_2', 'figure'),
    list(input('genre-select', 'value'),
    input('pop-slider','value'),
    input("artist_names", "value")
    ),
    function(genre, pop, artist) {
      pop_min <- pop[1]
      pop_max <- pop[2]
      filtered_df <- df %>% drop_na() %>%
        filter(playlist_genre %in% genre) %>%
        filter(track_popularity >= pop_min & track_popularity <= pop_max)
      if (is.null(artist)|length(artist) == 0){
        artist <- filtered_df %>%
          group_by(track_artist) %>%
          summarize(count=n())%>%
          arrange(desc(count))%>%
          pull(track_artist)
        artist <- artist[1:5]
      }
      filtered_df <- filtered_df%>%
        filter(track_artist %in% artist)
      filtered_df$year <- as.numeric(substr(filtered_df$track_album_release_date, 1,4))
      # if using direct labeling
      order <- filtered_df %>%
        group_by(track_artist, year) %>%
        summarize(popularity = mean(track_popularity))%>%
        arrange(desc(year)) %>%
        distinct(track_artist,.keep_all = TRUE)

      p <- filtered_df %>%
        ggplot(aes(x = year,
                   y = track_popularity,
                   color = track_artist,
                   )) +
        geom_point(alpha = 0.6) +
        ggthemes::scale_fill_tableau()+
        geom_line(stat = 'summary', fun = 'mean')+
        geom_text(
          data = order,
          ggplot2::aes_string(x = "year", y = "popularity", color = "track_artist", label = "track_artist"),
          size = 3.5,
          vjust = -3,
          hjust = 0.7
        )+
        theme_bw()+
        theme(legend.position = 'none') +
        labs(x="Year",
             y="Popularity",
             )
        
        # p <- df %>% drop_na() %>%
        #         filter(playlist_genre %in% genre) %>%
        #         filter(track_popularity >= pop[1] & track_popularity <= pop[2]) %>%
        #      ggplot(aes(x = playlist_genre,
        #          fill = playlist_genre)) +
        #     geom_bar(alpha = 0.6) +
        #     ggthemes::scale_fill_tableau() +
        #     theme_bw()
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0' )
