library(rvest)
library(tidyverse)

parse_table_ranking = function(url){
  table_data = url %>%
    read_html() %>%
    html_nodes('table tbody') %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_text()
  
  
  
  table_data = matrix(table_data, ncol = 10, byrow = TRUE)
  
  table_df = as.data.frame(table_data) %>%
    rename(rank = V1,
          bib = V2,
          h2h=    V3, 
          specialty =   V4, 
           rider =  V5, 
           age = V6,
          team = V7, 
          uci = V8,
          pnt = V9, 
          time = V10) %>%
    as_tibble() %>%
    mutate(race = url, 
           race = str_remove(race, 'https://www.procyclingstats.com/race/nc-australia/' ),
           race = str_remove(race, '/result'), 
           race = str_trim(race, 'both'),
           race = as.numeric(race),
           age = as.numeric(age), 
           uci = as.numeric(uci)
    )
  
  
 return(table_df)
  
}


theme_cycling <- function() { 
  font_text <- "Futura"
  font_title <- 'Roboto Slab'
  font_subtitle <- 'Roboto Slab'
  font_caption <- 'Andale Mono'
  
  theme_minimal() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#f0f0ed", colour = NA),
      
      plot.title = element_text(
        family = font_title,
        size = 20,
        face = 'bold',
        color = "#253f4b",
        hjust = 0,
        vjust = 1
      ),
      
      plot.subtitle = element_text(
        family = font_subtitle,
        color = "#253f4b",
        size = 10
      ),
      
      plot.caption = element_text(
        family = font_caption,
        size = 7,
        hjust = 1
      ),
      
      axis.title = element_text(
        family = font_text,
        size = 10
      ),
      
      axis.text = element_text(
        family = font_text,
        face = 'bold',
        size = 10
      ),
      
      axis.text.x = element_text(
        color = "#949494"
      ),
      
      strip.text.x = element_text(
        family = font_title,
        face = "bold", 
        size = 8,
        color = "#253f4b" ),
      
      axis.text.y = element_text(
        color = "#949494"
      )
    )
}

lst_url = c('https://www.procyclingstats.com/race/nc-australia/2024/result',
        'https://www.procyclingstats.com/race/nc-australia/2023/result',
        'https://www.procyclingstats.com/race/nc-australia/2022/result',
        'https://www.procyclingstats.com/race/nc-australia/2021/result',
        'https://www.procyclingstats.com/race/nc-australia/2020/result',
        'https://www.procyclingstats.com/race/nc-australia/2019/result',
        'https://www.procyclingstats.com/race/nc-australia/2018/result')
        

        
australia = map_df(lst_url, parse_table_ranking)



total_points_rider = australia %>%
  mutate(rider_clean = ifelse(team == '', rider, 
                              str_trim(str_replace(rider, team, ""))))  %>%
  group_by(race, rider_clean, specialty) %>%
  filter(rank %in% as.character(1:10) )


total_points_ = total_points_rider %>%
  ungroup() %>%
  select(rider = rider_clean, specialty, rank,  uci, year = race, team) %>%
  mutate(rank = as.numeric(rank),
         year = as.factor(year)) 



plot_df = total_points_ %>%
  count(specialty, rank) %>%
  rename(Position = rank)
  

# Pie-Donut chart
PieDonut(plot_df, 
         aes(Position, specialty, count=n), 
         r0 = 0.35,
         r1 = 0.8, 
         explode=c(1,2,3,4,5), 
         showRatioThreshold = F,
         title = 'Top 10 in #RoadNats by Rider Specialty since 2018', 
         titlesize = 8,
         donutLabelSize = 3,
         color = 'snow',
         family = 'Futura'
         
         ) 
 
# Analytics
 
total_points_ %>%
  group_by(rider) %>%
  summarise(total_points_uci = sum(uci, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = total_points_uci /sum(total_points_uci)) %>%
  arrange(-prop)

total_points_ %>%
  group_by(rider) 
  count(rank) %>%
  filter(rank == 2) %>%
  arrange(-n)

total_points_ %>%
  group_by(rider) %>%
  mutate(podium = ifelse(rank %in% c(1,2,3), 'yes', 'no')) %>%
  count(podium) %>%
  filter(podium == 'yes') %>%
  arrange(-n)

total_points_ %>%
  group_by(rider) %>%
count(rank) %>%
  filter(str_detect(rider, 'MEYER'))

total_points_ %>%
  group_by(rider) %>%
  count(rank) %>%
  filter(str_detect(rider, 'HARPER'))

total_points_ %>%
  count(rider, sort = TRUE)



