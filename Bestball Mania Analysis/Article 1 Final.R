library(tidyverse)
library(janitor)
library(googlesheets4)
library(googledrive)
library(scales)
library(shadowtext)
library(fontawesome)
library(showtext)
library(shadowtext)
font_add_google('Chivo', 'Chivo')
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)


source('ETR Final 2023 Themes.R')
source('ETR palette.R')
source('geom_rtile.R')
rm(df, a)

# Creating caption ====
plot_caption <- paste0(
  "<span style='font-family:Chivo;color:#999999;;font-size:11pt;'>Analysis by Michael Leone | Data: Underdog<br>",
  "Chart  by Michael Heery</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#999999;;font-size:11pt;'>&#xf099;</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>.</span>",
  "<span style='font-family:Chivo;color:#999999;;font-size:11pt;'> @heerymichael </span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#999999;font-size:11pt;'  >&#xf08c;</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>.</span>",
  "<span style='font-family:Chivo;color:#999999;font-size:11pt;'> michaelheery </span>"
)

# Figure 1: Stacked QB advance rates ====
# Setting up initial data
number_of_stacked_qbs <- c(0,1,2,3)
advance_rate <- c(0.159833, 0.164892, 0.170756, 0.163970)
count <- c(61752, 161948, 194927, 32573)

# Creating tibble
stacked_qbs_df <- tibble(number_of_stacked_qbs, advance_rate, count) %>% 
  mutate(count_percent = count / sum(count)) %>% 
  mutate(count_percent = percent(count_percent,accuracy = 2)) %>% 
  mutate(dot_fill = case_when(
    advance_rate > 0.1666666 ~ "#37af4a",
    TRUE ~ "#c53160"
  )) %>% 
  mutate(y_label = paste0(
    "<span style = font-family:'Chivo';color:'#333333';font-size:14pt>",
    "<b>",
    number_of_stacked_qbs,
    "<br>",
    "<span style = font-family:'Chivo';color:'#999999';font-size:10pt>",
    count_percent,
    "</b><span style = font-family:'Chivo';color:'#999999';font-size:10pt> of teams"
  )) %>% 
  mutate(advance_rate_round = round(advance_rate, 3)) %>% 
  mutate(advance_rate_label = paste0(
    sprintf("%.1f",(advance_rate_round * 100)),
    "%")) %>% 
  arrange(desc(number_of_stacked_qbs)) %>% 
  mutate(axis_order = row_number()) %>% 
  add_row(axis_order = 5,
          y_label = "<span style = font-family:'Chivo';color:'#333333'><b>NUMBER OF<br>STACKED QBs")

# Creating ggplot
figure_1 <- ggplot() +
  theme_etr_white_2023() +
  theme(axis.text.y = element_markdown(lineheight = 1),
        panel.grid.major.y = element_blank()) + 
  
  # Scales
  scale_x_continuous(breaks = c(0.16666666),
                     limits = c(0.146666666, 0.186666666),
                     labels = NULL) +
  scale_y_discrete(expand = expansion(add = c(0.5,0.1))) +
  scale_fill_identity() +
  geom_hline(yintercept = c(1,2,3,4),
             color = "#bbbbbb",
             size = 0.2) +
  
  # Data points and lines
  geom_segment(data = stacked_qbs_df,
               aes(x = 0.16666666,
                   xend = advance_rate,
                   y = reorder(y_label, axis_order),
                   yend = reorder(y_label, axis_order)),
               size = 0.8,
               color = "#333333") +
  geom_point(data = stacked_qbs_df,
             aes(x = 0.16666666,
                 y = reorder(y_label, axis_order)),
             shape = 21,
             fill = "#bbbbbb",
             color = "#333333",
             size = 3.5,
             stroke = 1) +
  geom_point(data = stacked_qbs_df,
             aes(x = advance_rate,
                 y = reorder(y_label, axis_order),
                 fill = dot_fill),
             shape = 21,
             stroke = 1,
             color = "#333333",
             size = 5) +
  
  # Data point annotations
  geom_shadowtext(data = stacked_qbs_df %>% filter(advance_rate > 0.166666),
                  aes(x = advance_rate + 0.0008,
                      y = reorder(y_label, axis_order),
                      label = advance_rate_label),
                  color = "#37af4a",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 0,
                  family = "Chivo",
                  fontface = "bold",
                  size = 4) +
  geom_shadowtext(data = stacked_qbs_df %>% filter(advance_rate < 0.166666),
                  aes(x = advance_rate - 0.0008,
                      y = reorder(y_label, axis_order),
                      label = advance_rate_label),
                  color = "#c53160",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 1,
                  family = "Chivo",
                  fontface = "bold",
                  size = 4) +
  
  # Neutral advance rate annotation
  geom_rect(aes(xmin = 0.162666666,
                xmax = 0.170666666,
                ymin = 4.6,
                ymax = 5.4),
            color = "#333333",
            fill = "#f5f5f5",
            size = 1) +
  geom_shadowtext(aes(x = 0.16666666,
                      y = 5,
                      label = "NEUTRAL\nADVANCE RATE"),
                  color = "#333333",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#f5f5f5",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3) +
  
  # Lower than neutral annotation
  geom_rect(aes(xmin = 0.148666666,
                xmax = 0.156666666,
                ymin = 4.6,
                ymax = 5.4),
            color = "#333333",
            fill = "#c53160",
            size = 1) +
  
  geom_shadowtext(aes(x = 0.15266666,
                      y = 5,
                      label = "LOWER\nADVANCE RATE"),
                  color = "#ffffff",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#c53160",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3) +
  
  # Higher advance rate
  geom_rect(aes(xmin = 0.17666666,
                xmax = 0.18466666,
                ymin = 4.6,
                ymax = 5.4),
            color = "#333333",
            fill = "#37af4a",
            size = 1) +
  geom_shadowtext(aes(x = 0.1806666,
                      y = 5,
                      label = "HIGHER\nADVANCE RATE"),
                  color = "#ffffff",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#37af4a",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3) +

  # Labels and captions
  labs(title = "Baseline advance rates<span style = color:'#ffffff'>...............</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the average advance rates for different QB roster<br>constructions in Best Ball Mania 3",
       caption = plot_caption,
       x = NULL,
       y = NULL)

ggsave(figure_1,
       filename = "article_1_figure_1.png",
       units = "in",
       width = 5 * 1.618,
       height = 5)

rm(stacked_qbs_df)

# Figure 2: Stacking QB buckets advance rates ====

# Setting up initial data
number_of_qbs <- c(1,1,2,2,2,3,3,3,3,4,4,4,4)
number_of_stacked_qbs <- c(0,1,0,1,2,0,1,2,3,0,1,2,3)
advance_rate <- c(0.132738, 0.144406,
                 0.165575, 0.171027, 0.176711,
                 0.156673, 0.157254, 0.158888, 0.165010,
                 0.134014, 0.137427, 0.126828, 0.133616)
count <- c(1680, 2029,
           36171, 100990, 136460,
           20629, 53245, 55001, 31598,
           3134, 5472, 3351, 943)

# Creating tibble
qb_stacking_detail_df <- tibble(number_of_qbs,number_of_stacked_qbs, advance_rate, count) %>% 
  mutate(count_percent = count / sum(count)) %>% 
  mutate(count_percent = percent(count_percent,accuracy = 2)) %>% 
  mutate(count_percent = case_when(
    count < (0.01 * sum(count)) ~ "Less than 1%",
    TRUE ~ count_percent
  )) %>% 
  mutate(dot_fill = case_when(
    advance_rate > 0.1666666 ~ "#37af4a",
    TRUE ~ "#c53160"
  )) %>% 
    mutate(qb_column = case_when(
      number_of_qbs == 1 ~ "QB",
      TRUE ~ "QBs"
    )) %>% 
  mutate(number_of_qbs_label_1 = case_when(
    number_of_stacked_qbs == 0 ~ paste(number_of_qbs),
    TRUE ~ ""
  )) %>% 
  mutate(number_of_qbs_label_2 = case_when(
    number_of_qbs_label_1 == 1 ~ "QB",
    number_of_qbs_label_1 == 2 ~ "QBs",
    number_of_qbs_label_1 == 3 ~ "QBs",
    number_of_qbs_label_1 == 4 ~ "QBs",
    TRUE ~ ""
  )) %>% 
  mutate(final_label = paste0(number_of_qbs_label_1,
                              " ",
                              number_of_qbs_label_2)) %>% 
  mutate(y_label = paste0(
    "<span style = font-family:'Chivo';color:'#333333';font-size:14pt>",
    "<b>",
    final_label,
    "<span style = 'color:#ffffff;font-size:7pt;'>.......</span>",
    number_of_stacked_qbs,
    " stacked",
    "<br>",
    "<span style = font-family:'Chivo';color:'#999999';font-size:10pt>",
    count_percent,
    "</b><span style = font-family:'Chivo';color:'#999999';font-size:10pt> of teams"
  )) %>% 
  mutate(advance_rate_round = round(advance_rate, 3)) %>% 
  mutate(advance_rate_label = paste0(
    sprintf("%.1f",(advance_rate_round * 100)),
    "%")) %>% 
    arrange(desc(number_of_qbs), desc(number_of_stacked_qbs)) %>% 
    mutate(axis_order = row_number()) %>% 
  add_row(axis_order = 14,
                                                y_label = "<span style = font-family:'Chivo';color:'#333333'><b>QB APPROACH")


# Creating ggplot
figure_2 <- ggplot() +
  theme_etr_white_2023() +
  theme(axis.text.y = element_markdown(lineheight = 1.1),
        panel.grid.major.y = element_blank()) +
  geom_hline(yintercept = c(1:13),
             color = "#bbbbbb",
             size = 0.2) +
  
  # Scales
  scale_x_continuous(breaks = c(0.16666666),
                     limits = c(0.116666666, 0.18999999),
                     expand = c(0,0),
                     labels = NULL) +
  scale_y_discrete(expand = expansion(add = c(0, 0.75))) +
  scale_fill_identity() +
  
  # Gray boxes to separate the QB buckets
  geom_rect(aes(xmin = 0.116666666, xmax = 0.18999999,
                ymin = 0.5, ymax = 4.5),
            color = "#f5f5f5",
            fill = "#f5f5f5",
            alpha = 0.9) +
  
  geom_rect(aes(xmin = 0.116666666, xmax = 0.18999999,
                ymin = 8.5, ymax = 11.5),
            color = "#f5f5f5",
            fill = "#f5f5f5",
            alpha = 0.9) +
  
  # Data points and lines
  geom_segment(data = qb_stacking_detail_df,
               aes(x = 0.16666666,
                   xend = advance_rate,
                   y = reorder(y_label, axis_order),
                   yend = reorder(y_label, axis_order)),
               size = 0.8,
               color = "#333333") +
  geom_point(data = qb_stacking_detail_df,
             aes(x = 0.16666666,
                 y = reorder(y_label, axis_order)),
             shape = 21,
             fill = "#bbbbbb",
             color = "#333333",
             size = 3.5,
             stroke = 1) +
  geom_point(data = qb_stacking_detail_df,
             aes(x = advance_rate,
                 y = reorder(y_label, axis_order),
                 fill = dot_fill),
             shape = 21,
             stroke = 1,
             color = "#333333",
             size = 5) +
  
  # Data point annotations
  geom_shadowtext(data = qb_stacking_detail_df %>% filter(advance_rate > 0.166666),
                  aes(x = advance_rate + 0.0015,
                      y = reorder(y_label, axis_order),
                      label = advance_rate_label),
                  color = "#37af4a",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 0,
                  family = "Chivo",
                  fontface = "bold",
                  size = 5) +
  geom_shadowtext(data = qb_stacking_detail_df %>% filter(advance_rate < 0.166666),
                  aes(x = advance_rate - 0.0015,
                      y = reorder(y_label, axis_order),
                      label = advance_rate_label),
                  color = "#c53160",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 1,
                  family = "Chivo",
                  fontface = "bold",
                  size = 5) +
  
  # Neutral advance rate annotation
  geom_rect(aes(xmin = 0.160666666,
                xmax = 0.172666666,
                ymin = 13.4,
                ymax = 14.6),
            color = "#333333",
            fill = "#f5f5f5",
            size = 1) +
  geom_shadowtext(aes(x = 0.16666666,
                      y = 14,
                      label = "NEUTRAL\nADVANCE RATE"),
                  color = "#333333",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#f5f5f5",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3.7) +
  
  # Lower than neutral annotation
  geom_rect(aes(xmin = 0.140666666,
                xmax = 0.152666666,
                ymin = 13.4,
                ymax = 14.6),
            color = "#333333",
            fill = "#c53160",
            size = 1) +
  
  geom_shadowtext(aes(x = 0.14666666,
                      y = 14,
                      label = "LOWER\nADVANCE RATE"),
                  color = "#ffffff",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#c53160",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3.7) +
  
  # Higher advance rate
  geom_rect(aes(xmin = 0.17466666,
                xmax = 0.18666666,
                ymin = 13.4,
                ymax = 14.6),
            color = "#333333",
            fill = "#37af4a",
            size = 1) +
  geom_shadowtext(aes(x = 0.1806666,
                      y = 14,
                      label = "HIGHER\nADVANCE RATE"),
                  color = "#ffffff",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#37af4a",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3.7) +
  
  # Labels and captions
  labs(title = "Stacking advance rates<span style = color:'#ffffff'>..............................</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the average advance rates for different appraoches to QB stacking in<br>Best Ball Mania 3",
       caption = plot_caption,
       x = NULL,
       y = NULL)

ggsave(figure_2,
       filename = "article_1_figure_2.png",
       units = "in",
       width = 6 * 1.618,
       height = 10)

rm(qb_stacking_detail_df)

# Figure 3: Total number of stacked players ====


# Setting up initial data
number_of_stacked_players <- seq(from = 0, to = 9, by = 1)

advance_rate <- c(0.157372,
                  0.162865,
                  0.166434,
                  0.171005,
                  0.169992,
                  0.172449,
                  0.162128,
                  0.162597,
                  0.143854,
                  0.133132)


count <- c(37135, 83425,
           105501, 97050,
          68409, 36057,
          15241, 5529,
          1863, 661)

# Creating tibble
total_stacked_player_df <- tibble(number_of_stacked_players,
                                  advance_rate, count) %>% 
  mutate(count_percent = count / sum(count)) %>% 
  mutate(count_percent = percent(count_percent,accuracy = 2)) %>% 
  mutate(count_percent = case_when(
    count < (0.01 * sum(count)) ~ "Less than 1%",
    TRUE ~ count_percent
  )) %>% 
  mutate(dot_fill = case_when(
    advance_rate > 0.1666666 ~ "#37af4a",
    TRUE ~ "#c53160"
  )) %>% 
  mutate(y_label = paste0(
    "<span style = font-family:'Chivo';color:'#333333';font-size:14pt>",
    "<b>",
    number_of_stacked_players,
    "<br>",
    "<span style = font-family:'Chivo';color:'#999999';font-size:10pt>",
    count_percent,
    "</b><span style = font-family:'Chivo';color:'#999999';font-size:10pt> of teams"
  )) %>% 
  mutate(advance_rate_round = round(advance_rate, 3)) %>% 
  mutate(advance_rate_label = paste0(
    sprintf("%.1f",(advance_rate_round * 100)),
    "%")) %>% 
  arrange(desc(number_of_stacked_players)) %>% 
  mutate(axis_order = row_number()) %>% 
  add_row(axis_order = 11,
          y_label = "<span style = font-family:'Chivo';color:'#333333'><b>NUMBER OF<br>STACKED PLAYERS")


# Creating ggplot
figure_3 <- ggplot() +
  theme_etr_white_2023() +
  theme(axis.text.y = element_markdown(lineheight = 1.1),
        panel.grid.major.y = element_blank()) +
  geom_hline(yintercept = c(1:10),
             color = "#bbbbbb",
             size = 0.2) +
  
  
  # Scales
  scale_x_continuous(breaks = c(0.16666666),
                     limits = c(0.126666666, 0.186666666),
                     expand = c(0,0),
                     labels = NULL) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
  scale_fill_identity() +
  
  # # Gray boxes to separate the QB buckets
  # geom_rect(aes(xmin = 0.116666666, xmax = 0.186666666,
  #               ymin = 0.5, ymax = 4.5),
  #           color = "#f5f5f5",
  #           fill = "#f5f5f5",
  #           alpha = 0.9) +
  # 
  # geom_rect(aes(xmin = 0.116666666, xmax = 0.186666666,
  #               ymin = 8.5, ymax = 11.5),
  #           color = "#f5f5f5",
  #           fill = "#f5f5f5",
  #           alpha = 0.9) +
  
  # Data points and lines
  geom_segment(data = total_stacked_player_df ,
               aes(x = 0.16666666,
                   xend = advance_rate,
                   y = reorder(y_label, axis_order),
                   yend = reorder(y_label, axis_order)),
               size = 0.8,
               color = "#333333") +
  geom_point(data = total_stacked_player_df ,
             aes(x = 0.16666666,
                 y = reorder(y_label, axis_order)),
             shape = 21,
             fill = "#bbbbbb",
             color = "#333333",
             size = 3.5,
             stroke = 1) +
  geom_point(data = total_stacked_player_df ,
             aes(x = advance_rate,
                 y = reorder(y_label, axis_order),
                 fill = dot_fill),
             shape = 21,
             stroke = 1,
             color = "#333333",
             size = 5) +
  
  # Data point annotations
  geom_shadowtext(data = total_stacked_player_df  %>% filter(advance_rate > 0.166666),
                  aes(x = advance_rate + 0.0015,
                      y = reorder(y_label, axis_order),
                      label = advance_rate_label),
                  color = "#37af4a",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 0,
                  family = "Chivo",
                  fontface = "bold",
                  size = 4) +
  geom_shadowtext(data = total_stacked_player_df  %>% filter(advance_rate < 0.166666),
                  aes(x = advance_rate - 0.0015,
                      y = reorder(y_label, axis_order),
                      label = advance_rate_label),
                  color = "#c53160",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 1,
                  family = "Chivo",
                  fontface = "bold",
                  size = 4) +
  
  # Neutral advance rate annotation
  geom_rect(aes(xmin = 0.161666666,
                xmax = 0.171666666,
                ymin = 10.4,
                ymax = 11.6),
            color = "#333333",
            fill = "#f5f5f5",
            size = 1) +
  geom_shadowtext(aes(x = 0.16666666,
                      y = 11,
                      label = "NEUTRAL\nADVANCE RATE"),
                  color = "#333333",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#f5f5f5",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3) +
  
  # Lower than neutral annotation
  geom_rect(aes(xmin = 0.141666666,
                xmax = 0.151666666,
                ymin = 10.4,
                ymax = 11.6),
            color = "#333333",
            fill = "#c53160",
            size = 1) +
  
  geom_shadowtext(aes(x = 0.14666666,
                      y = 11,
                      label = "LOWER\nADVANCE RATE"),
                  color = "#ffffff",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#c53160",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3) +
  
  # Higher advance rate
  geom_rect(aes(xmin = 0.17566666,
                xmax = 0.18566666,
                ymin = 10.4,
                ymax = 11.6),
            color = "#333333",
            size = 1,
            fill = "#37af4a") +
  geom_shadowtext(aes(x = 0.1806666,
                      y = 11,
                      label = "HIGHER\nADVANCE RATE"),
                  color = "#ffffff",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#37af4a",
                  bg.r= 0.5,
                  show.legend = FALSE,
                  size = 3) +
  
  # Labels and captions
  labs(title = "Stacking advance rates<span style = color:'#ffffff'>..........................</span><img src = 'ETR_Full_Blk.png' height = '32'>",
       subtitle = "Chart shows the average advance rates for Best Ball Mania 3 depnding on how many<br>stacked players they had in their roster",
       caption = plot_caption,
       x = NULL,
       y = NULL)

ggsave(figure_3,
       filename = "article_1_figure_3.png",
       units = "in",
       width = 6 * 1.618,
       height = 8)

rm(qb_stacking_detail_df)

# Figure 4 ====



