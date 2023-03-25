library(tidyverse)
library(janitor)
library(googlesheets4)
library(googledrive)
library(scales)
library(gt)
library(gtExtras)
library(ggtext)

source('ETR Final 2023 Themes.R')
source('ETR palette.R')

# Overall QB advance rates ====
# Importing data
qb1_pc_stack_count_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11qsgfnAe9K0IChnyEEKYBZLpov_9w9yhKhIa4baxsXg/edit#gid=621482116",
                                     sheet = "QB1PCStackCount") %>% 
  clean_names()


# Creating mini data frames

qb1_pc_stack_count_odds_increase <- qb1_pc_stack_count_data_raw[-c(1:10, 17),c(1:4)]
qb1_pc_stack_count_ev <- qb1_pc_stack_count_data_raw[c(2:7), c(1,6,7)]
qb1_pc_stack_count_number_of_teams <- qb1_pc_stack_count_data_raw[c(2:7), c(1,5)]

# Reformatting column headers
qb1_pc_stack_count_odds_increase <- qb1_pc_stack_count_odds_increase %>% 
  rename(qb1_pc_stack_count = 1,
         quarterfinals = 4,
         semifinals = 3,
         finals = 2) %>% 
  select(1,4,3,2) %>% 
  mutate_at(c(2,3,4), as.numeric) %>% 
  mutate(quarterfinals = round(quarterfinals, 4),
         semifinals = round(semifinals, 4),
         finals = round(finals, 4)) %>% 
  mutate(quarterfinals = quarterfinals - 1,
         semifinals = semifinals - 1,
         finals = finals - 1)

qb1_pc_stack_count_ev <- qb1_pc_stack_count_ev %>% 
  rename(qb1_pc_stack_count = 1,
         overall_ev = 2,
         change_in_ev = 3) %>% 
  mutate_at(c(2,3), as.numeric) %>% 
  mutate(overall_ev = round(overall_ev, 0),
         change_in_ev = round(change_in_ev, 4))

qb1_pc_stack_count_number_of_teams <- qb1_pc_stack_count_number_of_teams %>% 
  rename(qb1_pc_stack_count  = 1,
         number_of_teams = 2)

# Combining data 
qb1_pc_stack_count_final <- left_join(
  qb1_pc_stack_count_odds_increase,
  qb1_pc_stack_count_ev,
  by = "qb1_pc_stack_count"
) %>% left_join(
  qb1_pc_stack_count_number_of_teams,
  by = "qb1_pc_stack_count"
) %>% 
  # adding duplicate columns for icons
  mutate(quarterfinals_icon = quarterfinals,
         semifinals_icon = semifinals,
         finals_icon = finals) %>% 
  mutate(number_of_teams = as.numeric(number_of_teams)) %>% 
  mutate(teams_percent = number_of_teams / sum(number_of_teams)) %>% 
  select(
    qb1_pc_stack_count,
    overall_ev,
    change_in_ev,
    quarterfinals_icon,
    quarterfinals,
    semifinals_icon,
    semifinals,
    finals_icon,
    finals,
    number_of_teams,
    teams_percent
  ) %>% 
  arrange(desc(overall_ev)) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "",
         buffer_5 = "",
         buffer_6 = "",
         buffer_7 = "",
         buffer_8 = "",
         buffer_9 = "")

# Creating table

gt(qb1_pc_stack_count_final) %>% 
  gt_theme_etr_white() %>% 
  cols_label(
    qb1_pc_stack_count = "NUMBER OF PASS CATCHERS STACKED WITH QB1",
    quarterfinals_icon  = "",
    quarterfinals = "QUARTER FINALS",
    semifinals_icon = "",
    semifinals = "SEMI FINALS",
    finals_icon = "",
    finals = md("<br>FINALS"),
    overall_ev = md("<br>OVERALL ($)"),
    change_in_ev = "VALUE GAINED / LOST",
    number_of_teams = "NO. OF TEAMS",
    teams_percent = "",
    buffer_1 = "",
    buffer_2 = "",
    buffer_3 = "",
    buffer_4 = "",
    buffer_5 = "",
    buffer_6 = "",
    buffer_7 = "",
    buffer_8 = "",
    buffer_9 = "") %>% 
  # Aligning column labels
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(1,2,3,5,7,9))) %>% 
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_column_labels(columns = c(10))) %>% 
  # ALigning text in columns
  cols_align(
    align = "center",
    columns = c(1,2)
  ) %>% 
  # Moving buffer columns into position
  cols_move(buffer_1, after = qb1_pc_stack_count) %>% 
  cols_move(buffer_2, after = buffer_1) %>% 
  cols_move(buffer_3, after = overall_ev) %>% 
  cols_move(buffer_4, after = change_in_ev) %>% 
  cols_move(buffer_5, after = buffer_4) %>% 
  cols_move(buffer_6, after = quarterfinals) %>% 
  cols_move(buffer_7, after = semifinals) %>% 
  cols_move(buffer_8, after = finals) %>% 
  cols_move(buffer_9, after = buffer_8) %>% 
  # Adjusting column widths 
  cols_width(
    qb1_pc_stack_count ~ px(140),
    buffer_1 ~ px(15),
    buffer_2 ~ px(15),
    
    overall_ev ~ px(80),
    change_in_ev ~ px(80),
  
    buffer_3 ~ px(15),
    buffer_4 ~ px(30),
    buffer_5 ~ px(30),
    
    quarterfinals_icon ~ px(25),
    quarterfinals ~ px(65),
    buffer_6 ~ px(25),
    
    semifinals_icon ~ px(25),
    semifinals ~ px(65),
    buffer_7 ~ px(25),
    
    finals_icon ~ px(25),
    finals ~ px(65),
    buffer_8 ~ px(15),
    buffer_9 ~ px(15),

    number_of_teams ~ px(80),
    teams_percent ~ px(100)) %>% 
  
  # Adding spanners
  tab_spanner(columns = c(buffer_2,
                          overall_ev,
                          buffer_3,
                          change_in_ev,
                          buffer_4),
              level = 1,
              label = md("<br>EXPECTED VALUE")) %>% 
  
  tab_spanner(columns = c(buffer_5,
                          quarterfinals_icon,
                          quarterfinals,
                          buffer_6,
                          semifinals_icon,
                          semifinals,
                          buffer_7,
                          finals_icon,
                          finals,
                          buffer_8),
              level = 1,
              label = "INCREASE/DECREASE IN ODDS OF ACHIEVING A WEEKLY SCORE THAT WOULD QUALIFY FOR THE...") %>% 
  
  # Adding internal vertical divider lines
  # gt_add_divider(columns = c(buffer_1, buffer_4, buffer_8),
  #                sides = "right",
  #                color = "#999999",
  #                weight = px(.6),
  #                style = "solid") %>% 
  
  tab_style(style = list(
    cell_borders(sides = "right"),
    color = "#bbbbbb",
    weight = px(1)),
    locations = list(
      cells_body(columns = c(buffer_1, buffer_4, buffer_8)))) %>% 
  
  # Changing the icon columns from numbers to trend arrows
  gt_fa_rank_change(column = quarterfinals_icon,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  
  gt_fa_rank_change(column = semifinals_icon,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  
  gt_fa_rank_change(column = finals_icon,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  
  # Formatting numbers as percents
  fmt_percent(columns = c(3,5,7,9),
              decimals = 0) %>% 
  fmt_number(columns = number_of_teams,
             sep_mark = ",",
             decimals = 0) %>% 
  
  # Applying heatmap colouring
  gt_color_rows(columns = c(3),
                palette = c(etr_red,
                            "#ffffff",
                            etr_green),
                domain = c(-0.15,
                           0,
                           0.15)) %>% 
  
  # Removing formatting for very small sample size columns
  tab_style(
    style = list(
      cell_fill(color = "#f5f5f5"),
      cell_text(color = "#999999")
    ),
    locations = cells_body(rows = number_of_teams < 10000)
  ) %>% 
  
  tab_style(
    style = cell_text(color = "#808080"),
    locations = cells_body(columns = number_of_teams)) %>% 
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(2,3))) %>% 
  
  # adding bar plot to show the volume of each category
  gt_plt_bar_pct(column = teams_percent,
                 scaled =  FALSE,
                 fill = "#808080",
                 background = NA) %>% 
  
  # Adding title and footnote and caption
  tab_header(
    title = md("**Number of pass catchers stacked with QB1**")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_source_note(source_note = "SOURCE: UNDERDOG")


# Testing a visual ====

visual_data <- qb1_pc_stack_count_final %>% 
  select(qb1_pc_stack_count,
         overall_ev) %>% 
  mutate(baseline_ev = 133) %>% 
  mutate(label_string = paste0(
    "<b><span style = font-color:'#333333'; family:'Chivo'>",
    "QB1",
    " + ",
    qb1_pc_stack_count,
    " PASS CATCHERS")) %>% 
  select(label_string,
         baseline_ev,
         overall_ev) %>% 
  gather(key = measure,
         value = ev,
         baseline_ev:overall_ev) %>% 
  mutate(fill_color = case_when(
    measure == "baseline_ev" ~ "#666666",
    measure == "overall_ev" & ev < 133 ~ "#c53160",
    TRUE ~ "#37af4a"
  )) %>% 
  mutate(size_scale = case_when(
    measure == "baseline_ev" ~ 5,
    TRUE ~ 5
  ))


ggplot() +
  scale_x_continuous(position = "bottom") +
  scale_fill_identity() +
  scale_size_identity() +
  theme_etr_light_2023() +
  theme(axis.text.y = element_markdown(color = "#333333")) +
  geom_line(data = visual_data,
            aes(y = label_string,
                x = ev,
                group = label_string),
            color = "#333333",
            size = 0.6) +
  geom_vline(xintercept = 133,
             color = "#333333",
             size = 1.5,
             lty = "11") +
  geom_point(data = visual_data %>% 
               filter(measure != "baseline_ev"),
             aes(y = label_string,
                 x = ev,
                 color = measure,
                 fill = fill_color,
                 size = size_scale),
             color = "#333333",
             shape = 21,
             stroke = 1,
             show.legend = FALSE) +
  labs(title = "QB1 Pass catcher stack count",
       subtitle = "Plot shows the expected value achieved by different QB1-pass catcher stacking strategies",
       y = NULL,
       x = "EXPECTED VALUE ($)",
       caption = "caption")
