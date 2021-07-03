library(dplyr)
library(RColorBrewer)
library(extrafont)
library(tidytuesdayR)
library(tidyr)
library(ggplot2)

most_recent_tuesday <- tidytuesdayR::last_tuesday()

tt_data <- tidytuesdayR::tt_load(most_recent_tuesday)

data <- tt_data$animal_rescues

font_import()

data %>% 
  mutate(animal_group_parent = if_else(animal_group_parent == 'cat', 'Cat', animal_group_parent)) %>% 
  mutate(animal_group_parent = if_else(animal_group_parent == 'Budgie', 'Bird', animal_group_parent)) %>%
  mutate(animal_group_parent = if_else(animal_group_parent == 'Pigeon', 'Bird', animal_group_parent)) %>%
  mutate(animal_group_parent = if_else(animal_group_parent == 'Lamb', 'Sheep', animal_group_parent)) %>% 
  mutate(animal_group_parent = if_else(stringr::str_detect(animal_group_parent, 'Unknown'), 'Other', animal_group_parent)) %>% 
  dplyr::filter(incident_notional_cost != 'NULL') %>% 
  dplyr::mutate(incident_notional_cost = as.numeric(incident_notional_cost)) %>% 
  dplyr::select(animal_group_parent,incident_notional_cost) %>% 
  dplyr::group_by(animal_group_parent) %>% 
  dplyr::summarize(avg_cost = mean(incident_notional_cost),
                   count = n()) %>% 
  dplyr::arrange(desc(avg_cost)) %>% 
  ggplot(aes(reorder(animal_group_parent, avg_cost), avg_cost, fill = count)) + geom_bar(stat = 'identity') + coord_flip() +
  theme(panel.background = element_rect('#DAFFED'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#DAFFED"),
        axis.text = element_text(color = '#000000', family = 'Roboto Condensed'),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(color = '#000000'),
        legend.background = element_rect(fill = '#DAFFED'),
        plot.title = element_text(family = 'Roboto Condensed', size = 20),
        plot.subtitle = element_text(family = 'Roboto Condensed')) +
  xlab('') + ylab('') + ggtitle('Average Cost of Rescue by Animal',
                                subtitle = 'By London Fire Brigade (2009-2021)') + labs(caption = "@noahtolsen\ | Data:London.gov",
                                                                                        fill = 'Number of\nRescues')

ggsave('/Users/noah/Coding/R/TidyTuesday/2021-06-29/rescueCost.png', dpi = 700)
