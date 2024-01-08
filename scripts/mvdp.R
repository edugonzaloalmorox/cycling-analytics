library(tidyverse)
library(rvest)
library(gghighlight)

source('R/utils.R')


# Get the races

get_rider_table = function(url){
  
  sum_races = url %>%
    read_html() %>%
    html_node('.rdrResultsSum') %>%
    html_text()
  
  matches <- regmatches(sum_races, gregexpr("\\b\\d+\\b", sum_races))  
  
  
  # Extracting numeric values
  race_days <- as.numeric(matches[[1]][1])
  
  
  
  # 2024
  table_rider = url %>%
    read_html() %>%
    html_nodes("table tbody") %>%
    html_nodes("tr") %>%
    html_nodes("td") %>% 
    html_text()
  
  last_row = race_days * 9 
  data_matrix = table_rider[1:last_row]
  data_matrix  = matrix(data_matrix, ncol = 9, byrow = TRUE)
  
  df = as.data.frame(data_matrix)
  
  df = df %>% 
    mutate(season = url, 
           season = as.numeric(str_extract_all(url, "\\d+"))) 
  
  
  return(df)
  
  
  
  
}


# Get the data
start_year <- 2018
end_year <- 2024

# Generate the sequence of years
years <- seq(start_year, end_year, by = 1)

# Create the character strings
url_lst <- paste0("https://cx.procyclingstats.com/rider/mathieu-van-der-poel/", years)





df_2024 = map_df(url_lst, get_rider_table)


df_2024 = df_2024 %>%
  as_tibble() %>%
  select(date = V1, 
          position =V2, 
          race = V5, 
          points = V7,
          season) %>%
 group_by(season) %>%
 mutate(rank_race =  row_number()) %>%
 arrange(season, -rank_race) %>%
 mutate(order_race = row_number()) %>%
 select(-rank_race)

df_2024 = df_2024 %>%
   mutate(points= as.numeric(points),
          points = ifelse(is.na(points), 0, points), 
          cum_sum_points = cumsum(points), 
           points_per_race = (cum_sum_points/order_race) )


df_2024 %>%
  filter(season > 2018) %>%
  ungroup() %>%
  mutate(points_per_race = ifelse(is.na(points_per_race), 0, points_per_race)) %>%
  filter(order_race <=10) %>%
  ggplot(aes(order_race, points_per_race, colour = '#ee7600', group = season)) +
  geom_line(lineend = "round") +
  geom_point() +
  gghighlight(use_direct_label = FALSE) +
  theme_minimal() +
  facet_wrap(~ season) +
  theme_cycling() +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = 1:10) +
  labs(title = 'MVPD performance in Cyclocross | Seasons: 2019 - 2024',
       subtitle = 'MVDP UCI points per race during his first 10 CX races', 
       x = '', 
       y = '',
       caption = 'Source: ProCycling Stats | @EdudinGonzalo')
  
  


 





