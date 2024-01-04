library(rvest)
library(tidyverse)

source('R/utils.R')


parse_table_ranking = function(url){
  table_data = url %>%
    read_html() %>%
    html_nodes('table tbody') %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_text()
  
  
  
  table_data = matrix(table_data, ncol = 6, byrow = TRUE)
  
  table_df = as.data.frame(table_data) %>%
    rename(rank = V1,
            previous_rank = V2,
            dif =    V3, 
            rider=   V4, 
            team =  V5, 
            points = V6) %>%
    as_tibble()
  
  
  links = url %>%
    read_html() %>%
    html_nodes("table tbody") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  
  links_df = matrix(links, ncol = 3, byrow = TRUE)
  links_df = as.data.frame(links_df) %>%
    rename(link_rider = V1,
           link_team =    V2, 
           link_points=   V3 )
  
  
  final_df = table_df %>%
    bind_cols(as_tibble(links_df)) %>%
    mutate(link_rider = paste0('https://cx.procyclingstats.com/', link_rider), 
           link_team = paste0('https://cx.procyclingstats.com/', link_team),
           link_points = paste0('https://cx.procyclingstats.com/', link_points))
  
  
  return(final_df)
  
}


results_cx = map_df(links_rider, get_races_info)


url_men = 'https://cx.procyclingstats.com/rankings/me/individual'
url_women = 'https://cx.procyclingstats.com/rankings/we'

 
cx_df_men = parse_table_ranking(url_men)
cx_df_women = parse_table_ranking(url_women)

links_rider_men = cx_df_men %>%
  pull(link_rider)


links_rider_women = cx_df_women %>%
  pull(link_rider)


results_cx_men = map_df(links_rider_men, get_races_info)
results_cx_women = map_df(links_rider_women, get_races_info)

df_final = cx_df_men %>%
  left_join(results_cx_men, by = c('link_rider' = 'url')) %>%
  mutate(category = 'Men Elite') %>%
  bind_rows(
    cx_df_women %>%
      left_join(results_cx_women, by = c('link_rider' = 'url')) %>%
      mutate(category = 'Women Elite') 
  )





