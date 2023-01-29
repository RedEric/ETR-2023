library(tidyverse)
library(janitor)
library(zoo)
library(nflreadr)

# Background scipts
source('Circle Cropping Headshots Background Script.R')

# Importing data ====

# Importing player headshots
player_images <- nflreadr::load_rosters() %>% 
  clean_names() %>%
  select(full_name,
         headshot_url) %>% 
  # filtering out the usual incorrect duplicates
  filter(headshot_url != "https://static.www.nfl.com/image/private/f_auto,q_auto/league/c1lvavhrved77o44fnol") %>% 
  filter(headshot_url != "https://static.www.nfl.com/image/private/f_auto,q_auto/league/yt5pwzmwiqh824tjj96x")

# Loading PFF data
qb_stats_2022 <- read_csv("data/PFF QB stats 2022.csv") %>% 
  clean_names()


# Processing data ====

# Getting only relevant fields
qb_rushes <- qb_stats_2022 %>% 
  select(player,
         games,
         rush_carries,
         rush_yds,
         rush_tds) %>% 
  # filtering for at least 8 games
  filter(games > 7) %>% 
  mutate(rushes_per_game = rush_carries / games)


# Setting up to add the circle cropped player headshots
sample_of_players <- player_images %>% 
  filter(full_name %in% qb_rushes$player)

all_players <- sample_of_players$full_name

walk(all_players, crop_and_save_image)

player_images <- sample_of_players %>% 
  mutate(headshot_centered = str_glue("cropped images/{to_snake_case(full_name)}.png")) %>% 
  mutate(headshot_cropped = circle_crop(headshot_centered)) %>% 
  select(full_name,
         headshot_cropped) %>% 
  rename(player = full_name)

# Combining cropped headshots to the data
qb_rushes <- left_join(
  qb_rushes,
  player_images,
  by = "player"
) %>% 
  mutate(y_axis = "y")

write.csv(qb_rushes, "data/qb_rushes_data_with_centered_and_cropped_images.csv")
