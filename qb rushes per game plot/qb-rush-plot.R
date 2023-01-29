library(tidyverse)
library(ggimage)
library(ggtext)
library(glue)
# library(sysfonts)
# library(showtext)
source('Final ETR dataviz themes.R')

# 
# #set up fonts
# sysfonts::font_add_google("Chivo","Chivo")
# sysfonts::font_add("Gotham", regular = "fonts/Gotham-Light.otf", bold="fonts/Gotham-Bold.otf")
# sysfonts::font_add('Font Awesome 6 Brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
# showtext_auto()
# showtext_opts(dpi=300)

# Create positional arguments
qb_rush_data <- read_csv("data/qb_rushes_data_with_centered_and_cropped_images.csv")


qb_rush_data <- qb_rush_data %>% 
  filter(player != "Feleipe Franks") %>% 
  mutate(path = paste0(tolower(str_replace_all(player," ","_")),".png")) %>% 
  mutate(rushes_per_game = round(rushes_per_game, 0)) %>% 
  group_by(rushes_per_game) %>% 
  mutate(group_count = n(),
         row = row_number() - 1,
         type = case_when(group_count %% 2 == 0 ~ "even",
                          TRUE ~ "odd"),
         spacer = case_when(
           rushes_per_game > 10 ~ 3,
           rushes_per_game >= 7 ~ 3,
           TRUE ~ 1),
         max = 0 - ((group_count/2)-0.5)*spacer,
         pos = max + spacer * row) %>% 
  mutate(image_with_label =glue("images/circle-labels/{path}")) %>% 
  mutate(image_no_label =glue("images/circles/{path}")) 


test_plot <- ggplot() + 
  geom_rect(aes(xmin = 0.5,
                xmax = 1.5,
                ymin = -6,
                ymax = 6),
            fill = "#eeeeee") +
  geom_rect(aes(xmin = 2.5,
                xmax = 3.5,
                ymin = -6,
                ymax = 6),
            fill = "#eeeeee") +
  geom_rect(aes(xmin = 4.5,
                xmax = 5.5,
                ymin = -6,
                ymax = 6),
            fill = "#eeeeee") +
  geom_rect(aes(xmin = 6.5,
                xmax = 7.5,
                ymin = -6,
                ymax = 6),
            fill = "#eeeeee") +
  geom_rect(aes(xmin = 8.5,
                xmax = 9.5,
                ymin = -6,
                ymax = 6),
            fill = "#eeeeee") +
  geom_rect(aes(xmin = 10.5,
                xmax = 11.5,
                ymin = -6,
                ymax = 6),
            fill = "#eeeeee") +
  theme_etr() +
  theme(panel.grid.major.x = element_blank()) +
  #change up size for images in different popularity ranges
  #size cannot be mapped to aes, set manually outside of aes
  geom_image(data=qb_rush_data %>% filter(rushes_per_game < 7),
             aes(x = rushes_per_game,
                 y = pos,
                 image=image_no_label),
             size = 0.075,
             position = position_jitter(width=0.2, height=0),
             by = "height",
             asp = 2) +
  geom_image(data=qb_rush_data %>% filter(rushes_per_game > 6),
             aes(x = rushes_per_game,
                 y = pos,
                 image=image_with_label),
             size = 0.2,
             position = position_jitter(width=0.2, height=0),
             by = "height",
             asp = 2) +
  scale_y_continuous(limits = c(-6,6),
                     expand = c(0,0),
                                breaks = NULL) +
  scale_x_continuous(position = "top",
                     limits = c(0.5,12.5),
                     expand = c(0,0),
                     breaks = c(1:12))+
  labs(title = "INSERT TITLE",
       subtitle = "INSERT SUBTITLE",
       caption = "caption",
       x = "RUSHES PER GAME",
       y = NULL)
  
factor = 9.5 / 14

ggsave(test_plot,
       filename = "testplot.png",
       units = "in",
       height = 7,
       width = 7/factor,
       dpi = "retina")

#import custom social caption function
source("social-caption.R")

#plot title, subtitle, and caption for ggtext (HTML/CSS)
title = "<span style='font-family:Gotham;color:white;font-size:35pt;'><span style='font-family:\"Font Awesome 6 Brands\";color:#1DB954;font-size:35pt;'>&#xf1bc;</span> **Most <span style='color:#F7D22F;'>Pop</span>ular Artists on Spotify**</span>"
subtitle = "<p>Top 70 pop artists by popularity. Populairty determined by Spotify, scaled from 0, least popular, to 100, most popular.</p>"
caption = paste0("<span>**Source: {spotifyr}**<span><br>",
                 social_caption(icon_color ="#1DB954", bg_color="black", font_color = "#D7DDDD",
                                font_family="sans",
                                mastodon="fosstodon/@tanya_shapiro", linkedin="shapirotanya"))

#create positional arguments (similar to beeswarm)
df_plot = df_sub%>%
  select(name, popularity, path)%>%
  group_by(popularity)%>%
  mutate(group_count=n(),
         row = row_number()-1,
         type = case_when(group_count %% 2 ==0 ~ "even", TRUE ~ "odd"),
         spacer = case_when(popularity>95 ~ 1.25, 
                            popularity>=90 ~ 1.25, 
                            group_count<13 ~ 0.9, 
                            TRUE ~ 0.5),
         max =0-((group_count/2)-0.5)*spacer,
         pos = max + spacer*row)%>%
  mutate(image =glue("../images/circle-labels/{path}"))


#plot - use geom image to plot images
ggplot()+
  #change up size for images in different popularity ranges
  #size cannot be mapped to aes, set manually outside of aes
  geom_image(data=df_plot|>filter(popularity==99),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0, height=0.1),
             size = 0.1,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(popularity>=95 & popularity<99),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0, height=0.1),
             size = 0.08,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(popularity>=90 & popularity<95),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0, height=0.12),
             size = 0.065,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(popularity<90),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0, height=0.12),
             size = 0.05,
             asp=9.5/6)+
  coord_flip()+
  scale_x_continuous(limits=c(-5.8,5.8), expand=c(0,0))+
  labs(title = title,
       subtitle = subtitle,
       caption = caption)+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="black", color="black"),
        plot.title = element_textbox_simple(),
        plot.subtitle  = element_textbox_simple(color="#D7DDDD", size=15, margin=margin(t=8, b=10)),
        plot.caption = element_textbox_simple(color="#D7DDDD", margin=margin(t=15), size=12),
        plot.margin = margin(t=20, r=20, l=20, b=10),
        text = element_text(color="white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="#178F41",linewidth=0.25),
        axis.text.y=element_blank(),
        axis.text.x=element_text(family="Chivo", color="white", size=12),
        axis.ticks = element_blank(),
        axis.title = element_blank())

factor = 19/14
ggsave("../plots/popular-artists.png", height=14/factor, width=14, unit="in")