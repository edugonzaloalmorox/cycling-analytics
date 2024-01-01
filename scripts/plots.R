library(tidyverse)
library(tidytext)

source('R/utils.R')

path = 'data/processed/'
discipline = 'cyclocross_'


df_final = read_csv(paste0(path, discipline, Sys.Date(), '.csv'))

df_plot = df_final %>%
  mutate(pcs_day = pcs_points / race_days) %>%
  arrange(-pcs_day) %>%
  group_by(category) %>%
  top_n(25, pcs_day) %>%
  ungroup() %>%
  mutate(category = as.factor(category),
         rider = reorder_within(rider, pcs_day, category))


df_plot %>%
  ggplot(aes(rider, pcs_day, fill=category)) +
  geom_col(alpha = 0.35, show.legend = FALSE) +
  facet_wrap(~category, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c('Men Elite' = "red", 'Women Elite' = "blue"))+ 
  geom_text(aes(label = paste0(round(pcs_day, 1), ' pts'),
                hjust = 1),
            color = 'snow',
            size = 2.5 , 
            fontface = "bold" )+
  theme_cycling() +
  labs(title = 'Who is the most efficient rider in CX?',
       subtitle = 'Rider PCS points per CX race competed in 2023/24 season - Top 25',
       caption = 'Source: Pro Cycling Stats 
       \n @EdudinGonzalo',
       x = '', y = '')
