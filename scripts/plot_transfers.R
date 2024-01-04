library(rvest)
library(tidyverse)
library(tidytext)
library(ggchicklet)

source('R/utils.R')


url = 'https://www.procyclingstats.com/teams/transfer-value-won-lost'

tables_url = url %>%
  read_html() %>%
  html_nodes('table tbody') %>%
  html_nodes('a') %>%
  html_attr("href") 


transfers = matrix(tables_url, ncol = 2, byrow = TRUE) 


transfer_value = as.data.frame(transfers)%>%
  rename(
    link_team = V1,
    link_table = V2
  ) %>%
  mutate(link_team = paste0('https://www.procyclingstats.com/', link_team),
         link_table = paste0('https://www.procyclingstats.com/', link_table))


links_transfers = transfer_value %>% pull(link_table)



get_table = function(url, number_cols){
  
  
  
  
  
  tables_url = url %>%
    read_html() %>%
    html_nodes('table tbody') %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_text()
  
  
  
  transfers = matrix(tables_url, ncol = 4, byrow = TRUE) 
  
  
  transfer_value = as.data.frame(transfers)
  
  transfer_value = transfer_value %>%
    rename(transfer = V1, 
           rider = V2, 
           type_transfer = V3, 
           points = V4) %>%
    mutate(team = str_remove(url, 'https://www.procyclingstats.com/team/'))
  
  return(transfer_value)
  
  
}

result = map_df(links_transfers, get_table)

total_transfers_df  = result %>%
  mutate(team = str_remove(team,'-2024/season/transfer-surplus'),
         team = str_replace_all(team, '-', ' '), 
         team = str_to_title(team)) %>%
  as_tibble() %>%
  filter(rider != "")




plot_df = total_transfers_df %>%
  mutate(points = as.numeric(points)) %>%
  ungroup() %>%
  group_by(team) %>%
  mutate(total_pts_team = sum(points)) %>%
  mutate(team_clean = case_when(
         team == 'Astana Qazaqstan Team' ~ 'Astana',
         team == 'Decathlon Ag2r La Mondiale' ~ 'Ag2r La Mondiale',
         team == 'Team Jayco Alula' ~ 'Jayco Alula', 
         team == 'Alpecin Deceuninck' ~ 'Alpecin', 
         team == 'Ef Education Easypost' ~ 'EF Education', 
         team == 'Uae Team Emirates' ~ 'UAE',
         team == 'Team Dsm Firmenich Postnl' ~ 'DSM',
         team == 'Team Visma Lease A Bike' ~ 'Visma',
         team == 'Soudal Quick Step' ~ 'Soudal',
         team == 'Intermarche Circus Want' ~ 'Intermarche',
         team == 'Bahrain Victorious' ~ 'Bahrain',
         teamm == 'Ineos Grenadiers' ~ 'INEOS', 
         TRUE ~ team) , 
         team = as.factor(team),
         rider = reorder_within(rider, points, team),
         # Create a facet label with team and total points
         facet_label = paste0(team_clean, ': ', total_pts_team)) %>%
  ungroup()

ordered_teams = plot_df %>%
  group_by(facet_label) %>%
  summarise(mean_pts= mean(total_pts_team)) %>%
  arrange(-mean_pts) %>%
  pull(facet_label)


plot_df$facet_label <- factor(plot_df$facet_label, levels = ordered_teams, ordered = TRUE)

plot_df %>%
  ggplot(aes(rider, points, fill = type_transfer)) +
  geom_col(show.legend = FALSE, position="stack", alpha = 0.35) +
  # Use the facet_label directly without parsing as expressions
  facet_wrap(~ facet_label, scales = "free", ncol = 6) +
  coord_flip() +
  scale_fill_manual(values = c('in' = 'blue', 'out' = 'red')) + 
  scale_x_reordered() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(size = )) +
  theme_cycling() +
  labs(title = 'UCI points with 2024 transfers',
       subtitle = 'Teams ordered by total transfer surplus', x = '', y='',
        caption = 'Source: Pro Cycling Stats| @EdudinGonzalo',)

