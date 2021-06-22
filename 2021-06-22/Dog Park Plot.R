library(dplyr)
library(tidytuesdayR)
library(tidyr)
library(ggplot2)
library(grid)
most_recent_tuesday <- tidytuesdayR::last_tuesday()

tt_data <- tidytuesdayR::tt_load(most_recent_tuesday)
data <- tt_data$parks

img <- png::readPNG("2021-06-22/cartoonDogsWithBone.png")

twentyTwenty <- data %>% 
  filter(year == 2020)
  
test <- twentyTwenty %>% 
  arrange(-dogpark_data) %>% 
  head(5) %>% 
  ggplot(aes(reorder(city, dogpark_data), dogpark_data))+
  geom_bar(stat = 'identity', fill = '#3C6E71') + 
  xlab('') +
  ylab('') +
  geom_text(aes(y = 1,
                label = paste0(dogpark_data, " dog parks per 100,000 residents")),
            color = '#ffffff',
            size = 2.5,
            hjust = .35, position = position_dodge(.9),
            fontface = "bold") +
  scale_y_continuous(expand=c(0,0), breaks = NULL) +
  theme(
        text = element_text(color = "#8be9fd"), 
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks=element_blank(),
        axis.title.x= element_blank(),
        panel.border=element_blank(), 
        panel.spacing = unit(0, "cm"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff"),
        plot.margin = margin(0, 0, -.825, 0, "cm")) + coord_flip()

test2 <- twentyTwenty %>% 
  arrange(-dogpark_data) %>% 
  head(10) %>% 
  ggplot(aes(reorder(city, -dogpark_data), dogpark_data))+
  background_image(img) + 
  xlab('') +
  ylab('') +
  theme(plot.margin = unit(c(1, 0, 0, 0), "cm"),
        axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = '#f2d0b0'))






g <- ggplotGrob(test)
 full <- test2 +
  annotation_custom(
    grob = g,
    xmin = .12,
    xmax = .843,
    ymin = .369,
    ymax = .535
  ) +
   annotation_custom(grob = textGrob("Best Cities for Dogs", gp=gpar(fontsize=25, col="#3C6E71", fontface="bold"),   y = .95))
  
ggsave('dogPlot.png', full, dpi = 5000)
