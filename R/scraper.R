library(tidyverse)
library(rvest)


source('R/utils.R')


# ETL  --- ------
url_men = 'https://cx.procyclingstats.com/race/gp-sven-nys/2024/startlist'
url_women = 'https://cx.procyclingstats.com/race/gp-sven-nys-we/2024/startlist'


# Get info from the startlist
df_men  = parse_startlist_info(url_men, 'Men Elite')
df_women = parse_startlist_info(url_women, 'Women Elite')

# Parse riders links
links_men = get_rider_url(df_men)
links_women = get_rider_url(df_women)

# Get info for 
results <- map_df(links_men, get_races_info)

results_women <- map_df(links_women, get_races_info)


# Final dataset

df_complete_men = df_men %>%
  mutate(rider_url = paste0('https://cx.procyclingstats.com/', link)) %>%
  left_join(results, 
            by = c('rider_url' = 'url'))


df_complete_women = df_women %>%
  mutate(rider_url = paste0('https://cx.procyclingstats.com/', link)) %>%
  left_join(results_women, 
            by = c('rider_url' = 'url'))



df_final = bind_rows(df_complete_men, df_complete_women)

Sys.Date()
write_csv(df_final, paste0('data/processed/cyclocross_',Sys.Date(),'.csv' ))

