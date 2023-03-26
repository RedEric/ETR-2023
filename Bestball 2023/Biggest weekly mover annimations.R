library(tidyverse)
library(janitor)
library(googlesheets4)
library(googledrive)
library(nflreadr)
library(gganimate)

# Setting date for explort file control ====
date <- Sys.Date()

# Loading in data ====

ud_top_100 <- read_sheet("https://docs.google.com/spreadsheets/d/11QNIJCY59MERr_q2qlvodenLITWP63bY_Q84mRCoQ1o/edit#gid=1990779930",
                         sheet = "UDTop100Risers-Fallers") %>% 
  clean_names()

# Importing Team logos ====
team_logo <- load_teams() %>% 
  select(team_abbr,
         team_logo_espn,
         team_wordmark,
         team_nick) %>% 
  rename(team = team_abbr)

# Importing player headshot URLs ====
player_headshots <- load_players() %>% 
  select(display_name,
         first_name,
         last_name,
         headshot) %>% 
  rename(player = display_name)

# Adding headshots and logo URLs to dataframe ====
ud_top_100 <- ud_top_100 %>% 
  left_join(player_headshots,by = "player") %>% 
  left_join(team_logo, by = "team")

# Removing unnecessary data
rm(team_logo,
   player_headshots)

# Filtering for the top 4 movers each direction ====

# Top 4 positive
ud_top_100_top_4_positve <- ud_top_100 %>% 
  slice_max(change_adp,
            n = 4)

# Top 4 negative
ud_top_100_top_4_negative <- ud_top_100 %>% 
  slice_min(change_adp,
            n = 4)

# Top 4 both ways
ud_top_100_top_4_each_way <- bind_rows(
  ud_top_100_top_4_positve,
  ud_top_100_top_4_negative
) %>% 
  select(player,
         position,
         previous_adp,
         current_adp,
         change_adp,
         headshot,
         team_logo_espn) 

# Removing interim data 
rm(ud_top_100_top_4_positve,
   ud_top_100_top_4_negative)

# Calculating the frames needed for anniamtion ====
ud_top_100_top_4_each_way <- ud_top_100_top_4_each_way %>% 
  arrange(desc(change_adp)) %>% 
  mutate(appearance_order = row_number()) %>% 
  # making the change column a whole number
  mutate(change_frames = round(abs(change_adp), 0)) %>% 
  # adding two at the start (one blank and one static visisble) and one at the end for first player
  # then two for every other player
  mutate(player_additional_frames = case_when(
    appearance_order == 1 ~ 3,
    TRUE ~ 2
  )) %>% 
  mutate(change_frames = as.numeric(change_frames)) %>% 
  mutate(player_visible_frames = change_frames + player_additional_frames) %>% 
  mutate(player_invisible_frames = case_when(
    appearance_order == 1 ~ 1,
    TRUE ~ cumsum(player_visible_frames) - player_visible_frames))

# Setting ;player name variables ====

player_1 = ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% select(player)
player_2 = ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% select(player)
player_3 = ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% select(player)
player_4 = ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% select(player)
player_5 = ud_top_100_top_4_each_way %>% filter(appearance_order == 5) %>% select(player)
player_6 = ud_top_100_top_4_each_way %>% filter(appearance_order == 6) %>% select(player)
player_7 = ud_top_100_top_4_each_way %>% filter(appearance_order == 7) %>% select(player)
player_8 = ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% select(player)


# Calculating total frames required ====

total_frames_required_for_annimation <- sum(ud_top_100_top_4_each_way$player_visible_frames)



# Player frames alpha states ====

all_player_frames_dataframe <- ud_top_100_top_4_each_way %>% 
  mutate(player_static_intro = 1,
         player_static_outro = 1) %>% 
  select(player,
         player_invisible_frames,
         player_static_intro,
         change_frames,
         player_static_outro) %>% 
  mutate(action_frames = (
    player_invisible_frames + player_static_intro + change_frames + player_static_outro)) %>% 
  mutate(total_frames = total_frames_required_for_annimation) %>% 
  mutate(player_static_post_movement = total_frames_required_for_annimation - action_frames) %>% 
  select(player,
         player_invisible_frames,
         player_static_intro,
         change_frames,
         player_static_outro,
         player_static_post_movement) %>% 
 gather(key = frame_type,
        value = number_of_frames,
        player_invisible_frames:player_static_post_movement) %>% 
  arrange(player) %>% 
  mutate(alpha = case_when(
    frame_type == "player_invisible_frames" ~ 0,
    TRUE ~ 1)) %>%
  uncount(number_of_frames)

# Calculate position for playerdot/line at each frame ====

# Player 1 ====

player_1_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(previous_adp)
player_1_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(current_adp)
player_1_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(change_frames)
player_1_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>%  summarise(player_visible_frames)
player_1_fill_out <- total_frames_required_for_annimation - player_1_total_animation_frames
player_1_transition_frames_value = max(player_1_transition_frames$change_frames)

player_one_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_1$player)

player_one_movement <- c(
  rep(player_1_starting_position$previous_adp,2),
  seq(from = player_1_starting_position$previous_adp,
      to = player_1_finishing_position$current_adp,
      length.out = player_1_transition_frames_value),
  rep(player_1_finishing_position$current_adp, player_1_fill_out$player_visible_frames + 1)) 

player_1_table <- bind_cols(
  player_one_frames_dataframe,
  player_one_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_1,
   player_1_fill_out,
   player_1_finishing_position,
   player_1_starting_position,
   player_1_total_animation_frames,
   player_1_transition_frames,
   player_one_frames_dataframe,
   player_one_movement)

# Player 2 ====

player_2_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% select(player_invisible_frames)

player_2_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% summarise(previous_adp)
player_2_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% summarise(current_adp)
player_2_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% summarise(change_frames)
player_2_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>%  summarise(player_visible_frames)
player_2_fill_out <- total_frames_required_for_annimation - player_2_total_animation_frames
player_2_transition_frames_value = max(player_2_transition_frames$change_frames)

player_two_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_2$player)

player_two_movement <- c(
  rep(player_2_starting_position$previous_adp,player_2_invisible$player_invisible_frames + 1),
  seq(from = player_2_starting_position$previous_adp,
      to = player_2_finishing_position$current_adp,
      length.out = player_2_transition_frames_value),
  rep(player_2_finishing_position$current_adp, player_2_fill_out$player_visible_frames - player_2_invisible$player_invisible_frames + 1))

player_2_table <- bind_cols(
  player_two_frames_dataframe,
  player_two_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_2,
   player_2_invisible,
   player_2_fill_out,
   player_2_finishing_position,
   player_2_starting_position,
   player_2_total_animation_frames,
   player_2_transition_frames,
   player_two_frames_dataframe,
   player_two_movement)

# Player 3 ====

player_3_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% select(player_invisible_frames)

player_3_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% summarise(previous_adp)
player_3_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% summarise(current_adp)
player_3_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% summarise(change_frames)
player_3_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>%  summarise(player_visible_frames)
player_3_fill_out <- total_frames_required_for_annimation - player_3_total_animation_frames
player_3_transition_frames_value = max(player_3_transition_frames$change_frames)

player_three_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_3$player)

player_three_movement <- c(
  rep(player_3_starting_position$previous_adp,player_3_invisible$player_invisible_frames + 1),
  seq(from = player_3_starting_position$previous_adp,
      to = player_3_finishing_position$current_adp,
      length.out = player_3_transition_frames_value),
  rep(player_3_finishing_position$current_adp, player_3_fill_out$player_visible_frames - player_3_invisible$player_invisible_frames + 1))

player_3_table <- bind_cols(
  player_three_frames_dataframe,
  player_three_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_3,
   player_3_invisible,
   player_3_fill_out,
   player_3_finishing_position,
   player_3_starting_position,
   player_3_total_animation_frames,
   player_3_transition_frames,
   player_three_frames_dataframe,
   player_three_movement)

# Player 4 ====

player_4_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% select(player_invisible_frames)

player_4_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% summarise(previous_adp)
player_4_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% summarise(current_adp)
player_4_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% summarise(change_frames)
player_4_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>%  summarise(player_visible_frames)
player_4_fill_out <- total_frames_required_for_annimation - player_4_total_animation_frames
player_4_transition_frames_value = max(player_4_transition_frames$change_frames)

player_four_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_4$player)

player_four_movement <- c(
  rep(player_4_starting_position$previous_adp,player_4_invisible$player_invisible_frames + 1),
  seq(from = player_4_starting_position$previous_adp,
      to = player_4_finishing_position$current_adp,
      length.out = player_4_transition_frames_value),
  rep(player_4_finishing_position$current_adp, player_4_fill_out$player_visible_frames - player_4_invisible$player_invisible_frames + 1))

player_4_table <- bind_cols(
  player_four_frames_dataframe,
  player_four_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_4,
   player_4_invisible,
   player_4_fill_out,
   player_4_finishing_position,
   player_4_starting_position,
   player_4_total_animation_frames,
   player_4_transition_frames,
   player_four_frames_dataframe,
   player_four_movement)

# Player 5 ====

player_5_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 5) %>% select(player_invisible_frames)

player_5_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 5) %>% summarise(previous_adp)
player_5_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 5) %>% summarise(current_adp)
player_5_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 5) %>% summarise(change_frames)
player_5_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 5) %>%  summarise(player_visible_frames)
player_5_fill_out <- total_frames_required_for_annimation - player_5_total_animation_frames
player_5_transition_frames_value = max(player_5_transition_frames$change_frames)

player_five_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_5$player)

player_five_movement <- c(
  rep(player_5_starting_position$previous_adp,player_5_invisible$player_invisible_frames + 1),
  seq(from = player_5_starting_position$previous_adp,
      to = player_5_finishing_position$current_adp,
      length.out = player_5_transition_frames_value),
  rep(player_5_finishing_position$current_adp, player_5_fill_out$player_visible_frames - player_5_invisible$player_invisible_frames + 1))

player_5_table <- bind_cols(
  player_five_frames_dataframe,
  player_five_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_5,
   player_5_invisible,
   player_5_fill_out,
   player_5_finishing_position,
   player_5_starting_position,
   player_5_total_animation_frames,
   player_5_transition_frames,
   player_five_frames_dataframe,
   player_five_movement)


# Player 6 ====

player_6_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 6) %>% select(player_invisible_frames)

player_6_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 6) %>% summarise(previous_adp)
player_6_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 6) %>% summarise(current_adp)
player_6_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 6) %>% summarise(change_frames)
player_6_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 6) %>%  summarise(player_visible_frames)
player_6_fill_out <- total_frames_required_for_annimation - player_6_total_animation_frames
player_6_transition_frames_value = max(player_6_transition_frames$change_frames)

player_six_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_6$player)

player_six_movement <- c(
  rep(player_6_starting_position$previous_adp,player_6_invisible$player_invisible_frames + 1),
  seq(from = player_6_starting_position$previous_adp,
      to = player_6_finishing_position$current_adp,
      length.out = player_6_transition_frames_value),
  rep(player_6_finishing_position$current_adp, player_6_fill_out$player_visible_frames - player_6_invisible$player_invisible_frames + 1))

player_6_table <- bind_cols(
  player_six_frames_dataframe,
  player_six_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_6,
   player_6_invisible,
   player_6_fill_out,
   player_6_finishing_position,
   player_6_starting_position,
   player_6_total_animation_frames,
   player_6_transition_frames,
   player_six_frames_dataframe,
   player_six_movement)

# Player 7 ====

player_7_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 7) %>% select(player_invisible_frames)

player_7_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 7) %>% summarise(previous_adp)
player_7_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 7) %>% summarise(current_adp)
player_7_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 7) %>% summarise(change_frames)
player_7_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 7) %>%  summarise(player_visible_frames)
player_7_fill_out <- total_frames_required_for_annimation - player_7_total_animation_frames
player_7_transition_frames_value = max(player_7_transition_frames$change_frames)

player_seven_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_7$player)

player_seven_movement <- c(
  rep(player_7_starting_position$previous_adp,player_7_invisible$player_invisible_frames + 1),
  seq(from = player_7_starting_position$previous_adp,
      to = player_7_finishing_position$current_adp,
      length.out = player_7_transition_frames_value),
  rep(player_7_finishing_position$current_adp, player_7_fill_out$player_visible_frames - player_7_invisible$player_invisible_frames + 1))

player_7_table <- bind_cols(
  player_seven_frames_dataframe,
  player_seven_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())

rm(player_7,
   player_7_invisible,
   player_7_fill_out,
   player_7_finishing_position,
   player_7_starting_position,
   player_7_total_animation_frames,
   player_7_transition_frames,
   player_seven_frames_dataframe,
   player_seven_movement)



# Player 8 ====

player_8_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% select(player_invisible_frames)

player_8_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% summarise(previous_adp)
player_8_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% summarise(current_adp)
player_8_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% summarise(change_frames)
player_8_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>%  summarise(player_visible_frames)
player_8_fill_out <- total_frames_required_for_annimation - player_8_total_animation_frames
player_8_transition_frames_value = max(player_8_transition_frames$change_frames)

player_eight_frames_dataframe <- all_player_frames_dataframe %>% 
  filter(player %in% player_8$player)

player_eight_movement <- c(
  rep(player_8_starting_position$previous_adp,player_8_invisible$player_invisible_frames + 1),
  seq(from = player_8_starting_position$previous_adp,
      to = player_8_finishing_position$current_adp,
      length.out = player_8_transition_frames_value),
  rep(player_8_finishing_position$current_adp, player_8_fill_out$player_visible_frames - player_8_invisible$player_invisible_frames + 1))

player_8_table <- bind_cols(
  player_eight_frames_dataframe,
  player_eight_movement
) %>% 
  rename(adp = 4) %>% 
  mutate(frame = row_number())


rm(player_8,
   player_8_invisible,
   player_8_fill_out,
   player_8_finishing_position,
   player_8_starting_position,
   player_8_total_animation_frames,
   player_8_transition_frames,
   player_eight_frames_dataframe,
   player_eight_movement)

# Combining individual player tables ====

final_table <- bind_rows(
  player_1_table,
  player_2_table,
  player_3_table,
  player_4_table,
  player_5_table,
  player_6_table,
  player_7_table,
  player_8_table
)


# Static plot ====

test <- ggplot() +
  geom_line(data = final_table,
            aes(x = adp,
                y = player,
                group = player))


anim <- test + transition_reveal(frame) 

anim


# OLD CODE BELOW ====













# Player 1 data frame ====
player_1_name <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% select(player)
player_1_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(max(player_invisible_frames))
player_1_visible_static <- 1
player_1_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(previous_adp)
player_1_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(current_adp)
player_1_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% summarise(change_frames)
player_1_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>%  summarise(player_visible_frames)
player_1_fill_out <- total_frames_required_for_annimation - player_1_total_animation_frames
player_1_transition_frames_value = max(player_1_transition_frames$change_frames)


# player_1_name
player_name_data <- ud_top_100_top_4_each_way %>% filter(appearance_order == 1) %>% select(player) %>% mutate(rows = total_frames_required_for_annimation) %>% uncount(rows)

# Player 1 alpha
player_alpha <- c(
  0,
  rep(1, total_frames_required_for_annimation - 1)
)

# player 1 movement

player_movement <- c(
  rep(player_1_starting_position$previous_adp,2),
  seq(from = player_1_starting_position$previous_adp,
      to = player_1_finishing_position$current_adp,
      length.out = player_1_transition_frames_value),
  rep(player_1_finishing_position$current_adp, player_1_fill_out$player_visible_frames + 1))

# Combining player one data 

player_one_final_data <- tibble(
  player_name_data,
  player_movement,
  player_alpha
) %>% 
  mutate(frame = row_number())


# Player 2 data frame ====
player_2_name <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% select(player)
player_2_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% select(player_invisible_frames)
player_2_visible_static <- 1
player_2_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% summarise(previous_adp)
player_2_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% summarise(current_adp)
player_2_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% summarise(change_frames)
player_2_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>%  summarise(player_visible_frames)
player_2_fill_out <- total_frames_required_for_annimation - player_2_total_animation_frames
player_2_transition_frames_value = max(player_2_transition_frames$change_frames)


# player_2_name

player_name_data <- ud_top_100_top_4_each_way %>% filter(appearance_order == 2) %>% select(player) %>% mutate(rows = total_frames_required_for_annimation) %>% uncount(rows)

# Player 2 alpha
player_alpha <- c(
  rep(0, player_2_invisible$player_invisible_frames),
  rep(1, total_frames_required_for_annimation - player_2_invisible$player_invisible_frames)
)

# player 2 movement

player_movement <- c(
  rep(player_2_starting_position$previous_adp,player_2_invisible$player_invisible_frames + 1),
  seq(from = player_2_starting_position$previous_adp,
      to = player_2_finishing_position$current_adp,
      length.out = player_2_transition_frames_value),
  rep(player_2_finishing_position$current_adp, player_2_fill_out$player_visible_frames - player_2_invisible$player_invisible_frames + 1))

# Combining player two data 

player_two_final_data <- tibble(
  player_name_data,
  player_movement,
  player_alpha
) %>% 
  mutate(frame = row_number())



# Player 3 data frame ====
player_3_name <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% select(player)
player_3_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% select(player_invisible_frames)
player_3_visible_static <- 1
player_3_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% summarise(previous_adp)
player_3_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% summarise(current_adp)
player_3_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% summarise(change_frames)
player_3_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>%  summarise(player_visible_frames)
player_3_fill_out <- total_frames_required_for_annimation - player_3_total_animation_frames
player_3_transition_frames_value = max(player_3_transition_frames$change_frames)


# player_3_name

player_name_data <- ud_top_100_top_4_each_way %>% filter(appearance_order == 3) %>% select(player) %>% mutate(rows = total_frames_required_for_annimation) %>% uncount(rows)

# Player 2 alpha
player_alpha <- c(
  rep(0, player_3_invisible$player_invisible_frames),
  rep(1, total_frames_required_for_annimation - player_3_invisible$player_invisible_frames)
)

# player 3 movement

player_movement <- c(
  rep(player_3_starting_position$previous_adp,player_3_invisible$player_invisible_frames + 1),
  seq(from = player_3_starting_position$previous_adp,
      to = player_3_finishing_position$current_adp,
      length.out = player_3_transition_frames_value),
  rep(player_3_finishing_position$current_adp, player_3_fill_out$player_visible_frames - player_3_invisible$player_invisible_frames + 1))

# Combining player three data 

player_three_final_data <- tibble(
  player_name_data,
  player_movement,
  player_alpha
) %>% 
  mutate(frame = row_number())


# Player 4 data frame ====
player_4_name <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% select(player)
player_4_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% select(player_invisible_frames)
player_4_visible_static <- 1
player_4_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% summarise(previous_adp)
player_4_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% summarise(current_adp)
player_4_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% summarise(change_frames)
player_4_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>%  summarise(player_visible_frames)
player_4_fill_out <- total_frames_required_for_annimation - player_4_total_animation_frames
player_4_transition_frames_value = max(player_4_transition_frames$change_frames)


# player_4_name

player_name_data <- ud_top_100_top_4_each_way %>% filter(appearance_order == 4) %>% select(player) %>% mutate(rows = total_frames_required_for_annimation) %>% uncount(rows)

# Player 2 alpha
player_alpha <- c(
  rep(0, player_4_invisible$player_invisible_frames),
  rep(1, total_frames_required_for_annimation - player_4_invisible$player_invisible_frames)
)

# player 2 movement

player_movement <- c(
  rep(player_4_starting_position$previous_adp,player_4_invisible$player_invisible_frames + 1),
  seq(from = player_4_starting_position$previous_adp,
      to = player_4_finishing_position$current_adp,
      length.out = player_4_transition_frames_value),
  rep(player_4_finishing_position$current_adp, player_4_fill_out$player_visible_frames - player_4_invisible$player_invisible_frames + 1))

# Combining player two data 

player_four_final_data <- tibble(
  player_name_data,
  player_movement,
  player_alpha
) %>% 
  mutate(frame = row_number())


# Player 8 data frame ====
player_8_name <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% select(player)
player_8_invisible <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% select(player_invisible_frames)
player_8_visible_static <- 1
player_8_starting_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% summarise(previous_adp)
player_8_finishing_position <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% summarise(current_adp)
player_8_transition_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% summarise(change_frames)
player_8_total_animation_frames <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>%  summarise(player_visible_frames)
player_8_fill_out <- total_frames_required_for_annimation - player_8_total_animation_frames
player_8_transition_frames_value = max(player_8_transition_frames$change_frames)


# player_2_name

player_name_data <- ud_top_100_top_4_each_way %>% filter(appearance_order == 8) %>% select(player) %>% mutate(rows = total_frames_required_for_annimation) %>% uncount(rows)

# Player 2 alpha
player_alpha <- c(
  rep(0, player_8_invisible$player_invisible_frames),
  rep(1, total_frames_required_for_annimation - player_8_invisible$player_invisible_frames)
)

# player 2 movement

player_movement <- c(
  rep(player_8_starting_position$previous_adp,player_8_invisible$player_invisible_frames + 1),
  seq(from = player_8_starting_position$previous_adp,
      to = player_8_finishing_position$current_adp,
      length.out = player_8_transition_frames_value))

# Combining player two data 

player_eight_final_data <- tibble(
  player_name_data,
  player_movement,
  player_alpha
) %>% 
  mutate(frame = row_number())

# Combining all players ===

all_players <- bind_rows(
  player_one_final_data,
  player_two_final_data,
  player_three_final_data,
  player_four_final_data,
  player_eight_final_data
) %>% 
  mutate(player_movement = as.numeric(player_movement))

# Creating basic plot ====

plot <- ggplot() +
  geom_line(data = all_players,
            aes(x = player_movement,
                y = player,
                group = player))

anim <- plot + transition_reveal(frame) +
  geom_point(data = all_players,
             aes(x = player_movement,
                 y = player,
                 group = player,
                 alpha = player_alpha),
             shape = 21,
             color = "#ffffff",
             fill = "#333333",
             size = 6) +
  scale_alpha_identity()

anim
