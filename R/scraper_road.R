library(rvest)
library(tidyverse)
library(ggrepel)


url_road_men = 'https://www.procyclingstats.com/rankings/me/uci-individual'

source('R/utils.R')




df = url_road_men %>%
  read_html() %>%
  html_nodes("table tbody") %>%
  html_nodes("tr") %>%
  html_nodes("td") %>% 
  html_text()


data_matrix  = matrix(df, ncol = 6, byrow = TRUE)


df = as.data.frame(data_matrix) %>%
  rename(number = V1,
         prev = V2, 
         diff = V3, 
         rider = V4,
         team = V5,
         points = V6) %>%
  mutate(number = as.integer(number),
         prev = as.integer(prev),
         diff = as.character(diff),
         rider = as.character(rider), 
         team = as.character(team),
         points = as.numeric(points),
         )


links_road_rider = url_road_men %>%
  read_html() %>%
  html_nodes("table tbody") %>%
  html_nodes("a") %>%
  html_attr("href")


matrix_links_road = matrix(links_road_rider, ncol = 3, byrow = TRUE)

df_links = as.data.frame(matrix_links_road ) %>%
  rename(link_rider = V1,
         link_team = V2, 
         link_points = V3) %>%
  mutate(link_rider = as.character(link_rider),
         link_team = as.character( link_team),
         link_points = as.character(link_points)
         ) 


df_complete = df %>%
  bind_cols(df_links) %>%
  as_tibble() %>%
  mutate(category = 'Men Elite')



link_rider_lst = get_rider_url(df_complete, 'link_rider')


results = map_df(link_rider_lst, get_races_info)
results = map_df(link_rider_lst, test_get_races_info)



df_final = df_complete %>%
   mutate(url = paste0('https://www.procyclingstats.com/', link_rider)) %>%
  left_join(results, by = 'url')


#### Women #####



women_url = 'https://www.procyclingstats.com/rankings/we/world-ranking'

df_women = women_url %>%
  read_html() %>%
  html_nodes("table tbody") %>%
  html_nodes("tr") %>%
  html_nodes("td") %>% 
  html_text()



data_matrix_w  = matrix(df_women, ncol = 6, byrow = TRUE)

df_w= as.data.frame(data_matrix_w) %>%
  rename(number = V1,
         prev = V2, 
         diff = V3, 
         rider = V4,
         team = V5,
         points = V6) %>%
  mutate(number = as.integer(number),
         prev = as.integer(prev),
         diff = as.character(diff),
         rider = as.character(rider), 
         team = as.character(team),
         points = as.numeric(points),
  )



links_road_women = women_url %>%
  read_html() %>%
  html_nodes("table tbody") %>%
  html_nodes("a") %>%
  html_attr("href")


matrix_links_women = matrix(links_road_women, ncol = 3, byrow = TRUE)

df_links_w = as.data.frame(matrix_links_women ) %>%
  rename(link_rider = V1,
         link_team = V2, 
         link_points = V3) %>%
  mutate(link_rider = as.character(link_rider),
         link_team = as.character( link_team),
         link_points = as.character(link_points)
  ) 


df_complete_w = df_w %>%
  bind_cols(df_links_w) %>%
  as_tibble() %>%
  mutate(category = 'Women Elite')


link_w_lst = get_rider_url(df_complete_w, 'link_rider')


results_w = map_df(link_w_lst, test_get_races_info)

df_final_w = df_complete_w %>%
  mutate(url = paste0('https://www.procyclingstats.com/', link_rider)) %>%
  left_join(results_w, by = 'url')


df_plot = df_final %>% bind_rows(df_final_w)










