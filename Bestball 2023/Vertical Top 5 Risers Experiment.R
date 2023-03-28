library(tidyverse)
library(janitor)
library(zoo)
library(googlesheets4)
library(googledrive)
library(fontawesome)
library(htmltools)
library(ggimage)
library(geomtextpath)
library(glue)
library(gganimate)
library(showtext)
library(shadowtext)
font_add_google('Chivo', 'Chivo')
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)
source('essential-files/ETR Final 2023 Themes.R')



# Downloading data 
ud_top_100_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11QNIJCY59MERr_q2qlvodenLITWP63bY_Q84mRCoQ1o/edit#gid=1990779930",
                             sheet = "UDTop100Risers-Fallers") %>% 
  clean_names() %>% 
  # correcting player teams
  mutate(team = case_when(
    player == "Miles Sanders" ~ "CAR",
    player == "David Montgomery" ~ "DET",
    player == "Brandin Cooks" ~ "DAL",
    player == "Jakobi Meyers" ~ "LV",
    player == "Darren Waller" ~ "NYG",
    player == "Mecole Hardman" ~ "NYJ",
    player == "Jerick McKinnon" ~ "FREE AGENT",
    TRUE ~ team
  )) %>% 
  mutate(team = case_when(
    team == "FA" ~ "FREE AGENT",
    TRUE ~ team
  ))


# UD Top 100 Top 5 Risers ====

ud_top_100_top_5 <- ud_top_100_raw %>% 
  slice_max(change_adp, n =5)


# Loading NFL player names and headshots ====
player_images <- nflreadr::load_players() %>% 
  select(display_name, headshot) %>% 
  rename(player = display_name) %>% 
  # Fixing Jeff Wilson 
  mutate(player = case_when(player == "Jeffery Wilson" ~ "Jeff Wilson", TRUE ~ player)) %>% 
  # Removing duplicate named players
  filter(headshot != "https://static.www.nfl.com/image/private/f_auto,q_auto/league/yt5pwzmwiqh824tjj96x") %>% # Lamar Jackson dupe
  filter(headshot != "https://static.www.nfl.com/image/private/f_auto,q_auto/league/wkugml4qpzkcj1tcyezx") %>% # Michael Thomas dupe
  # Adding rookie headshots
  add_row(player = "Jaxon Smith-Njigba", headshot = "https://static.www.nfl.com/image/private/f_auto,q_85/league/god-combine-headshots/2023/92a5ae48-30f7-4f74-baaf-d6cb3a99ba28") %>% 
  add_row(player = "Jordan Addison", headshot = "https://static.www.nfl.com/image/private/f_auto,q_85/league/god-combine-headshots/2023/2784ba17-49ae-489f-bbcf-2fcbea4ae243") %>% 
  add_row(player = "Sean Tucker", headshot = "https://static.www.nfl.com/image/private/f_auto,q_85/league/god-combine-headshots/2023/58c0e4f0-a26f-4da6-b767-d8affe628f2c") %>% 
  add_row(player = "Josh Downs", headshot = "https://static.www.nfl.com/image/private/f_auto,q_85/league/god-combine-headshots/2023/7e13e543-ac84-4325-94c8-3e03c3748de7") %>% 
  add_row(player = "Kayshon Boutte", headshot = "https://static.www.nfl.com/image/private/f_auto,q_85/league/god-combine-headshots/2023/fa315812-61ef-4529-8fa3-9a5310d1a25c") %>% 
  rename(full_name = player,
         headshot_url = headshot)

# Cropping the headshots ====


# Setting up to add the circle cropped player headshots
sample_of_players <- player_images %>% 
  filter(full_name %in% ud_top_100_top_5$player)

all_players <- sample_of_players$full_name

source('essential-files/Circle Cropping Headshots Background Script.R')

walk(all_players, crop_and_save_image)

player_images <- sample_of_players %>% 
  mutate(headshot_centered = str_glue("cropped images/{to_snake_case(full_name)}.png")) %>% 
  mutate(headshot_cropped = circle_crop(headshot_centered)) %>% 
  select(full_name,
         headshot_cropped) %>% 
  rename(player = full_name)


#create new file path for images
player_images = player_images %>% mutate(new_image_path = paste0(tolower(str_replace_all(player," ","_")),".png"))



#create loop to generate and save all images with labels
for(i in 1:nrow(player_images)){
  pos = "bottom"
  hjust = 0.5
  path = player_images$new_image_path[i]
  plot = plot_image_label(image=player_images$headshot_cropped[i],
                          label= player_images$player[i],
                          font_color="#333333",
                          top_bottom = pos, 
                          hjust=hjust)
  ggsave(filename=glue("images/circle-labels/{path}"), plot=plot, height=3.95, width=4.5)
}


# Fixing up image pathway ====

player_images <- player_images %>% 
  mutate(path = paste0(tolower(str_replace_all(player," ","_")),".png")) %>% 
  mutate(image_with_label =glue("images/circle-labels/{path}")) %>% 
  select(player, image_with_label)

# Combining data ====

final_data <- ud_top_100_top_5 %>% 
  left_join(player_images,
            by = "player") %>% 
  select(player,
         image_with_label,
         current_adp,
         previous_adp,
         change_adp) %>% 
  arrange(current_adp) %>% 
  mutate(appearance_order = row_number())

# Creating caption ====
plot_caption <- paste0(
  "<span style='font-family:Chivo;color:#999999;;font-size:11pt;'>Chart created by Michael Heery</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#999999;;font-size:11pt;'>&#xf099;</span>",
  "<span style='font-family:Chivo;color:#999999;;font-size:11pt;'> @heerymichael </span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#999999;font-size:11pt;'  >&#xf08c;</span>",
  "<span style='font-family:Chivo;color:#999999;font-size:11pt;'> michaelheery </span>"
)


# Annotation data ====

final_data = final_data %>% 
  mutate(annotation = paste0(
    "+", change_adp, " picks"
  )) %>% 
  mutate(annotation_position = current_adp + (change_adp / 2))

test <- ggplot() +
  theme_etr_white_2023() +
  theme(plot.caption = element_markdown()) +
  geom_segment(data = final_data,
               aes(x = appearance_order,
                   xend = appearance_order,
                   y = previous_adp,
                   yend = current_adp),
               color = "#37af4a",
               size = .75,
               arrow = arrow(length = unit(0.35, "cm"))) +
  geom_point(data = final_data,
             aes(x = appearance_order,
                 y = previous_adp),
             color = "#37af4a",
             size = 2) +
  geom_image(data = final_data,
             aes(x = appearance_order,
                 y = previous_adp + 3,
                 image = image_with_label),
             asp = 1.1,
             size = 0.18) +
  geom_shadowtext(data = final_data,
                  aes(x = appearance_order,
                      y = annotation_position,
                      label = annotation),
                  color = "#37af4a",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#ffffff",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 4.5) +
  scale_y_reverse(breaks = c(73, 85, 97),
                  labels = c("6th ROUND",
                             "7th ROUND",
                             "8th ROUND"),
                  limits = c(97,70),
                  expand = c(0,0)) +
  labs(title = "Underdog Top 100<span style = color:'#ffffff'>...</span><img src = 'ETR_Full_Blk.png' height = '32'>",
       subtitle = "Chart highlights five largest increases in ADP amongst the top<br>100 players on Underdog over the last week",
       caption = plot_caption,
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = NULL,
                     limits = c(0.75,5.25))

ggsave(test,
       filename = "test.png")
  
test_anim <- test + transition_filter(transition_length = 4,
                                        filter_length = 4,
                                        filter_0 = appearance_order == 0,
                                        filter_1 = appearance_order == 1,
                                        filter_2 = appearance_order == 2,
                                        filter_3 = appearance_order == 3,
                                        filter_4 = appearance_order == 4,
                                        filter_5 = appearance_order == 5) +
    enter_appear() +
    shadow_mark()
  

animate(test_anim,
        height = 7,
        width = 7,
        units = "in",
        res = 320,
        duration = 6,
        end_pause = 2,
        renderer = av_renderer())

anim_save("test_anim.mp4")  
 


# 101 Plus  =====