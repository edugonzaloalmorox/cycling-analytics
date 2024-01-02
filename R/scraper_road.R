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
  mutate(link_rider = as.character(paste0('https://www.procyclingstats.com/',link_rider)),
         link_team = as.character(paste0('https://www.procyclingstats.com/', link_team)),
         link_points = as.character(paste0('https://www.procyclingstats.com/', link_points))
         ) 


df_complete = df %>%
  bind_cols(df_links) %>%
  as_tibble()




link_rider_lst = get_rider_url(df_complete, 'link_rider')



"https://cx.procyclingstats.com/https://www.procyclingstats.com/rider/tadej-pogacar" %>%
  read_html() %>%
  html_node('.rdrResultsSum') %>%
  html_text()



test_df = map_df(link_rider_lst, get_races_info)
  

df %>%
  group_by(team) %>%
  summarise(total_riders = n(),
    mean_pts_team = mean(points),
            total_pts_team = sum(points)) %>%
  arrange(-total_pts_team) %>%
  ungroup() %>%
  ggplot(aes(mean_pts_team, total_pts_team)) +
  geom_point(aes(size = total_riders), alpha=0.4) +
  geom_text_repel(
    aes(label = team),
    family = "Roboto",
    size =4,
    min.segment.length = 0, 
    seed = 42, 
    box.padding = 0.5,
    max.overlaps = Inf,
    arrow = arrow(length = unit(0.010, "npc")),
    nudge_x = .15,
    nudge_y = .5,
    color = "grey50"
  ) +
  geom_smooth(se = FALSE, alpha = 0.2) +
  theme_cycling() +
  labs(title = 'Total vs Average UCI points per team')
  
  
  safe_scrape <- function(url) {
    Sys.sleep(runif(1, 2, 5))  # Random delay between 2 and 5 seconds
    tryCatch({
      # Attempt to read the HTML content of the page
      page <- read_html(url, handle = handle(url))
      # Your parsing code goes here
      # Example: content <- page %>% html_nodes("yourSelector") %>% html_text()
      return(content)
    }, error = function(e) {
      # Return NA or a custom message in case of error
      return(NA)
    })
  }

test_urls = link_rider_lst[1:2]

  
results <- map(test_urls, safe_scrape)








