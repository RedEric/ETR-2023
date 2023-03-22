library(tidyverse)
library(janitor)
library(googlesheets4)
library(googledrive)
library(scales)
library(gt)
library(gtExtras)

source('ETR Final 2023 Themes.R')
source('ETR palette.R')

# Overall QB advance rates ====
# Importing data
number_of_qbs_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
              sheet = "Number of QBs")

# Removing additional rows

number_of_qbs_data_2a <- number_of_qbs_data_raw[-c(1, 6:20),]

number_of_qbs_data_2a <- number_of_qbs_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

number_of_qbs_data_2b <- number_of_qbs_data_raw[-c(1:15, 20),]
number_of_qbs_data_2b <- number_of_qbs_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

number_of_qbs_data_2_final <- left_join(
  number_of_qbs_data_2a,
  number_of_qbs_data_2b,
  by = "number_of_q_bs"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric)


gt(number_of_qbs_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    number_of_q_bs = "NUMBER OF QBs",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = number_of_q_bs
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.29,
                           0,
                           0.29)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
# Setting column widths 
  cols_width(
    number_of_q_bs ~ px(60),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
# centering column headers
tab_style(
  style = cell_text(align = "center"),
  locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of QBs drafted**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")


# Stacked QB advance rates ====

stacked_qbs_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                      sheet = "Number of QBs Stacked")

stacked_qbs_data_2a <- stacked_qbs_data_raw[-c(1, 6:20),]

stacked_qbs_data_2a <- stacked_qbs_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance) %>% 
  rename(number_of_qbs_stacked = number_of_q_bs_stacked)

stacked_qbs_data_2b <- stacked_qbs_data_raw[-c(1:8, 13),]
stacked_qbs_data_2b <- stacked_qbs_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count)) %>% 
  rename(number_of_qbs_stacked = number_of_q_bs_stacked)

stacked_qbs_data_2_final <- left_join(
  stacked_qbs_data_2a,
  stacked_qbs_data_2b,
  by = "number_of_qbs_stacked"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)


gt(stacked_qbs_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    number_of_qbs_stacked = "NUMBER OF STACKED QBs",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = number_of_qbs_stacked
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.34,
                           0,
                           0.34)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    number_of_qbs_stacked ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of stacked QBs drafted**"),
    subtitle = "Stacked QBs are QBs are are stacked with at least one pass catcher from their own team") %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# Number of QBs game stacked ====

game_stacked_qbs_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                   sheet = "Number of QBs Game Stacked")

game_stacked_qbs_data_2a <- game_stacked_qbs_data_raw[-c(1, 6:20),]

game_stacked_qbs_data_2a <- game_stacked_qbs_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance) %>% 
  rename(number_of_qbs_game_stacked = number_of_q_bs_game_stacked)

game_stacked_qbs_data_2b <- game_stacked_qbs_data_raw[-c(1:8, 13),]
game_stacked_qbs_data_2b <- game_stacked_qbs_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count)) %>% 
  rename(number_of_qbs_game_stacked = number_of_q_bs_game_stacked)

stacked_qbs_data_2_final <- left_join(
  game_stacked_qbs_data_2a,
  game_stacked_qbs_data_2b,
  by = "number_of_qbs_game_stacked"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(stacked_qbs_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    number_of_qbs_game_stacked = "GAME STACKED QBs",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = number_of_qbs_game_stacked
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.63,
                           0,
                           0.63)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    number_of_qbs_game_stacked ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of game stacked QBs**"),
    subtitle = md("Game stacked QBs are QBs that have been stacked with at least one pass catcher from their own team and one skill player from the opposing team")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# Number of QBs opp stacked ====


opp_stacked_qbs_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                        sheet = "Number of QBs Opp Stacked")

opp_stacked_qbs_data_2a <- opp_stacked_qbs_data_raw[-c(1, 6:20),]

opp_stacked_qbs_data_2a <- opp_stacked_qbs_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance) %>% 
  rename(number_of_qbs_opp_stacked = number_of_q_bs_opp_stacked)

opp_stacked_qbs_data_2b <- opp_stacked_qbs_data_raw[-c(1:8, 13),]
opp_stacked_qbs_data_2b <- opp_stacked_qbs_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count)) %>% 
  rename(number_of_qbs_opp_stacked = number_of_q_bs_opp_stacked)

opp_stacked_qbs_data_2_final <- left_join(
  opp_stacked_qbs_data_2a,
  opp_stacked_qbs_data_2b,
  by = "number_of_qbs_opp_stacked"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(opp_stacked_qbs_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    number_of_qbs_opp_stacked = "GAME STACKED QBs",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = number_of_qbs_opp_stacked
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.12,
                           0,
                           0.12)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    number_of_qbs_opp_stacked ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of opponent stacked QBs**"),
    subtitle = md("Opponent stacked QBs are QBs that have been stacked with at least one skill player from the opposing team")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# QB1 Pass catcher stack count ====

qb_1pc_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                       sheet = "QB1PCStackCount")

qb_1pc_stack_count_data_2a <- qb_1pc_stack_count_data_raw[-c(1, 8:17),]

qb_1pc_stack_count_data_2a <- qb_1pc_stack_count_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

qb_1pc_stack_count_data_2b <- qb_1pc_stack_count_data_raw[-c(1:10, 17),]
qb_1pc_stack_count_data_2b <- qb_1pc_stack_count_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

qb_1pc_stack_count_data_2_final <- left_join(
  qb_1pc_stack_count_data_2a,
  qb_1pc_stack_count_data_2b,
  by = "qb1pc_stack_count"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(qb_1pc_stack_count_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    qb1pc_stack_count = "PASS CATCHERS STACKED WITH QB1",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = qb1pc_stack_count
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.28,
                           0,
                           0.28)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    qb1pc_stack_count ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of pass catchers stacked with QB1**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# QB2 Pass catcher stack count ====

qb_2pc_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                          sheet = "QB2PCStackCount")

qb_2pc_stack_count_data_2a <- qb_2pc_stack_count_data_raw[-c(1, 7:17),]

qb_2pc_stack_count_data_2a <- qb_2pc_stack_count_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

qb_2pc_stack_count_data_2b <- qb_2pc_stack_count_data_raw[-c(1:9, 15),]
qb_2pc_stack_count_data_2b <- qb_2pc_stack_count_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

qb_2pc_stack_count_data_2_final <- left_join(
  qb_2pc_stack_count_data_2a,
  qb_2pc_stack_count_data_2b,
  by = "qb2pc_stack_count"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(qb_2pc_stack_count_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    qb2pc_stack_count = "PASS CATCHERS STACKED WITH QB2",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = qb2pc_stack_count
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.42,
                           0,
                           0.42)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    qb2pc_stack_count ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of pass catchers stacked with QB2**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")


# QB3 Pass catcher stack count ====

qb_3pc_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                          sheet = "QB3PCStackCount")

qb_3pc_stack_count_data_2a <- qb_3pc_stack_count_data_raw[-c(1, 7:17),]

qb_3pc_stack_count_data_2a <- qb_3pc_stack_count_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

qb_3pc_stack_count_data_2b <- qb_3pc_stack_count_data_raw[-c(1:9, 15:17),]
qb_3pc_stack_count_data_2b <- qb_3pc_stack_count_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

qb_3pc_stack_count_data_2_final <- left_join(
  qb_3pc_stack_count_data_2a,
  qb_3pc_stack_count_data_2b,
  by = "qb3pc_stack_count"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(qb_3pc_stack_count_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    qb3pc_stack_count = "PASS CATCHERS STACKED WITH QB3",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = qb3pc_stack_count
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.28,
                           0,
                           0.28)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    qb3pc_stack_count ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of pass catchers stacked with QB3**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# QB1 Game stack count ====


qb_1_game_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                          sheet = "QB1GameStackCount")

qb_1_game_stack_count_data_2a <- qb_1_game_stack_count_data_raw[-c(1, 7:23),]

qb_1_game_stack_count_data_2a <- qb_1_game_stack_count_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

qb_1_game_stack_count_data_2b <- qb_1_game_stack_count_data_raw[-c(1:9, 15:23),]
qb_1_game_stack_count_data_2b <- qb_1_game_stack_count_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

qb_1_game_stack_count_data_2_final <- left_join(
  qb_1_game_stack_count_data_2a,
  qb_1_game_stack_count_data_2b,
  by = "qb1opp_stack_count"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(qb_1_game_stack_count_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    qb1opp_stack_count = "OPPOSING SKILL PLAYERS STACKED WITH QB1",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = qb1opp_stack_count
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.28,
                           0,
                           0.28)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    qb1opp_stack_count ~ px(100),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of opposing skill players stacked with self-stacked QB1**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# QB2 Game stack count ====


qb_2_game_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                             sheet = "QB2GameStackCount")

qb_2_game_stack_count_data_2a <- qb_2_game_stack_count_data_raw[-c(1, 7:23),]

qb_2_game_stack_count_data_2a <- qb_2_game_stack_count_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

qb_2_game_stack_count_data_2b <- qb_2_game_stack_count_data_raw[-c(1:9, 15:23),]
qb_2_game_stack_count_data_2b <- qb_2_game_stack_count_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

qb_2_game_stack_count_data_2_final <- left_join(
  qb_2_game_stack_count_data_2a,
  qb_2_game_stack_count_data_2b,
  by = "qb2opp_stack_count"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(qb_2_game_stack_count_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    qb2opp_stack_count = "OPPOSING SKILL PLAYERS STACKED WITH QB2",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = qb2opp_stack_count
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.28,
                           0,
                           0.28)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    qb2opp_stack_count ~ px(100),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of opposing skill players stacked with self-stacked QB2**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# QB3 Game stack count ====


qb_3_game_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                             sheet = "QB3GameStackCount")

qb_3_game_stack_count_data_2a <- qb_3_game_stack_count_data_raw[-c(1, 6:23),]

qb_3_game_stack_count_data_2a <- qb_3_game_stack_count_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

qb_3_game_stack_count_data_2b <- qb_3_game_stack_count_data_raw[-c(1:8, 13:23),]
qb_3_game_stack_count_data_2b <- qb_3_game_stack_count_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

qb_3_game_stack_count_data_2_final <- left_join(
  qb_3_game_stack_count_data_2a,
  qb_3_game_stack_count_data_2b,
  by = "qb3opp_stack_count"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters - 1,
         odds_change_semis = odds_change_semis - 1,
         odds_change_finals = odds_change_finals - 1)

gt(qb_3_game_stack_count_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    qb3opp_stack_count = "OPPOSING SKILL PLAYERS STACKED WITH QB3",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = qb3opp_stack_count
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.28,
                           0,
                           0.28)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    qb3opp_stack_count ~ px(100),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Number of opposing skill players stacked with self-stacked QB3**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# Total stacked players ====

total_stacked_players_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                             sheet = "TotalStackedPlayers")

total_stacked_players_data_2a <- total_stacked_players_data_raw[-c(1, 16:33),]

total_stacked_players_data_2a <- total_stacked_players_data_2a %>% 
  select(1:4) %>% 
  clean_names() %>% 
  rename(act_finals_adv = finals_advance,
         act_semis_adv = semis_advance,
         act_quarters_adv = quarters_advance)

total_stacked_players_data_2b <- total_stacked_players_data_raw[-c(1:18, 33),]
total_stacked_players_data_2b <- total_stacked_players_data_2b %>% 
  select(1:5) %>% 
  clean_names() %>% 
  rename(count = x5,
         odds_change_finals = finals_advance,
         odds_change_semis = semis_advance,
         odds_change_quarters = quarters_advance) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count_percent = count / sum(count))

total_stacked_players_data_2_final <- left_join(
  total_stacked_players_data_2a,
  total_stacked_players_data_2b,
  by = "total_stacked_players"
) %>% 
  select(1,4,3,2,7,6,5,8,9) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "") %>% 
  mutate_at(c(2:9), as.numeric) %>% 
  mutate(odds_change_quarters = odds_change_quarters ,
         odds_change_semis = odds_change_semis ,
         odds_change_finals = odds_change_finals )

gt(total_stacked_players_data_2_final) %>% 
  gt_theme_etr_white() %>% 
  # Grouping relevant columns
  tab_spanner(
    label = md("<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE..."),
    columns = c(act_quarters_adv,
                act_semis_adv,
                act_finals_adv)
  ) %>% 
  tab_spanner(
    label = "INCREASE/DECREASE IN ODDS OF SUCCESSFULLY ACHIEVING A WEEKLY SCORE THAT WOULD ADVANCE TO THE...",
    columns = c(odds_change_quarters,
                odds_change_semis,
                odds_change_finals,
                buffer_4,
                buffer_5)
  ) %>% 
  # renaming columns 
  cols_label(
    total_stacked_players = "TOTAL SKILL PLAYERS STACKED",
    act_quarters_adv = "QUARTER FINALS",
    act_semis_adv = "SEMI FINALS",
    act_finals_adv = "FINALS",
    odds_change_quarters = "QUARTER FINALS",
    odds_change_semis = "SEMI FINALS",
    odds_change_finals = "FINALS",
    count = "NUMBER OF TEAMS",
    count_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = ""
  ) %>% 
  # Relocating buffer bars
  cols_move(
    columns = buffer_1,
    after = total_stacked_players
  ) %>% 
  cols_move(
    columns = buffer_2,
    after = act_finals_adv
  ) %>% 
  cols_move(
    columns = buffer_3,
    after = odds_change_finals
  ) %>% 
  cols_move(
    columns = buffer_4,
    after = odds_change_quarters
  ) %>% 
  cols_move(
    columns = buffer_5,
    after = odds_change_semis
  ) %>% 
  # Reformating numbers
  fmt_percent(columns = c(2:4),
              decimals = 2) %>% 
  fmt_percent(columns = c(5:7),
              decimals = 1) %>% 
  fmt_number(columns = count,
             sep_mark = ",",
             decimals = 0) %>% 
  # Color fill
  gt_color_rows(columns = c(5:7),
                palette = c(etr_purple,
                            "#ffffff",
                            etr_green),
                domain = c(-0.28,
                           0,
                           0.28)) %>% 
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = count_percent,
                 scaled = FALSE,
                 fill = "#808080",
                 background = "#ffffff") %>% 
  # Setting column widths 
  cols_width(
    total_stacked_players ~ px(90),
    act_quarters_adv ~ px(80),
    act_semis_adv ~ px(80),
    act_finals_adv ~ px(80),
    odds_change_quarters ~ px(80),
    odds_change_semis ~ px(80),
    odds_change_finals ~ px(80),
    count ~ px(80) ,
    count_percent ~ px(100) ,
    buffer_1 ~ px(30) ,
    buffer_2 ~ px(30) ,
    buffer_3 ~ px(30) ,
    buffer_4 ~ px(5),
    buffer_5 ~ px(5)
  ) %>% 
  # centering column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1:7))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = count)) %>% 
  tab_header(
    title = md("**Total number of same team and opponent skills players stacked with QB1 through 3**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_labels(columns = c(2:4, 8))) %>% 
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_column_spanners(spanners = "<BR>% OF TEAMS THAT ACHIEVED A WEEKLY SCORE THAT WOULD ADVANCE TO THE...")) %>% 
  # Adding footnote
  # tab_footnote(
  #   footnote = "These columns show how much more or less likely these teams were to progress measured against a random expected advance rate of 10% to the quarterfinals, 6.3% to the semi-finals, and 0.21% to the finals",
  #   locations = cells_column_spanners(spanners = "INCREASE/DECREASE IN ODDS OF SUCCESS OF ADVANCING TO THE...")
  # ) %>% 
  # Adding caption 
  tab_source_note(source_note = "SOURCE: UNDERDOG")

# ADP buckets ====

adp_buckets_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                             sheet = "ADPBuckets")
