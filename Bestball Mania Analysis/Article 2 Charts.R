# Article link

https://docs.google.com/document/d/1WkCSqVlF1CeqZi0iWYBQfsCZVyT3EHlDgzhH7x2_pIU/edit


library(tidyverse)
library(janitor)
library(googlesheets4)
library(googledrive)
library(scales)
library(shadowtext)
library(fontawesome)
library(showtext)
library(shadowtext)
library(gt)
library(gtExtras)
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
  "<span style='font-family:Chivo;color:#999999;;font-size:8pt;'>Data:Underdog<br>",
  "Analysis by Michael Leone</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#999999;;font-size:8pt;'>&#xf099;</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>.</span>",
  "<span style='font-family:Chivo;color:#999999;;font-size:8pt;'> @2Hats1Mike<br>",
  "Chart by Michael Heery</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#999999;;font-size:8pt;'>&#xf099;</span>",
  "<span style = 'color:#ffffff;font-size:7pt;'>.</span>",
  "<span style='font-family:Chivo;color:#999999;;font-size:8pt;'> @heerymichael </span>"
)

# Figure 1 ====

adp_value_bucket <- seq(1:10)
adp_value = c(115.7, 56.6, 28.5, 7.5, -11.8, -31.5, -53.5, -80.8, -119.5, -211.5)

figure_1_data <- tibble(adp_value_bucket,adp_value) %>% 
  mutate(fill_color = case_when(
    adp_value > 0 ~ etr_green,
    TRUE ~ etr_red
  ))


figure_1 = ggplot() +
  
  # Chart basics
  theme_etr_white_2023() +
  scale_x_continuous(breaks = c(1:10),
                     position = "top") +
  scale_y_continuous(limits = c(-235,235)) +
  scale_fill_identity() +
  
  # data
  geom_col(data = figure_1_data,
           aes(y = adp_value,
               x = adp_value_bucket,
               fill = fill_color),
           width = 0.6) +
  geom_hline(yintercept = 0,
             color = "#333333") +
  
  # Annotation
  geom_shadowtext(aes(x = 1.55,
                      y = 110,
                      label = "On average the top 10% of teams were\nable to gain ADP value woth 116 picks"),
                  color = "#666666",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#ffffff",
                  bg.r = 0.35,
                  hjust = 0) +
  geom_curve(aes(x = 1.75, y = 140,
                 xend = 1, yend = 120),
             curvature = 0.66,
             color = "#999999",
             arrow = arrow(type = "open",
                           length = unit(0.02, "npc"))) +
  
  geom_shadowtext(aes(x = 9.4,
                      y = -170,
                      label = "The bottom 10% of teams gave up\nADP value equivalent to 212 picks"),
                  color = "#666666",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 1) +
  geom_curve(aes(xend = 10, yend = -220,
                 x = 9.25, y = -200),
             curvature = 0.66,
             color = "#999999",
             arrow = arrow(type = "open",
                           length = unit(0.02, "npc"))) +

  # Final touches
  labs(title = "ADP value <span style = color:'#ffffff'>...............................</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the average numbers of picks gained or lost by BBM3<br> playoff teams grouped into percentile buckets.<br>Bucket 1 contains the top 10% of teams in terms of value gained,<br>and bucket 10 the bottom 10%.",
       y = NULL,
       x = NULL,
       caption = plot_caption) +
  theme(plot.caption = element_markdown(lineheight = 1.1))

ggsave(figure_1,
       filename = "article_2_figure_1.png",
       width = 6.5,
       height = 6.5,
       units = "in")





# Figure 2 ====

# Asked Leone if there is more granular data? HE IS GOING TO PROVIDE

# Figure 3 ====

adp_draft_capital = c(850.6, 819.6, 806.1, 796.1, 787.0, 778.0, 768.1, 756.4, 739.9, 698.0)

figure_3_data <- tibble(adp_value_bucket,
                        adp_draft_capital) %>% 
  mutate(divergance = adp_draft_capital - 786) %>% 
  mutate(fill_color = case_when(
    divergance > 0 ~ etr_green,
    TRUE ~ etr_red
))


figure_3 = ggplot() +
  
  # Chart basics
  theme_etr_white_2023() +
  scale_x_continuous(breaks = c(1:10),
                     position = "top",
                     expand = c(0,0),
                     limits = c(0.3,10.7)) +
  scale_y_continuous(limits = c(-101, 101)) +
  scale_fill_identity() +
  
  # data
  geom_col(data = figure_3_data,
           aes(y = divergance,
               x = adp_value_bucket,
               fill = fill_color),
           width = 0.7) +
  geom_hline(yintercept = 0,
             color = "#333333") +
  
  # Annotation
  geom_shadowtext(aes(x = 1.6,
                      y = 65,
                      label = "On average the top 10% of teams gained\n65 points in draft capital value"),
                  color = "#666666",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 0,
                  vjust = 1) +
  geom_curve(aes(x = 1.75,
                 xend = 1,
                 y = 68,
                 yend = 68),
             size = 0.6,
             arrow = arrow(length = unit(0.2,"cm"),
                           type = "open"),
             curvature = .8,
             color = "#999999") +
  
  geom_shadowtext(aes(x = 9.4,
                      y = -88,
                      label = "The bottom 10% of teams lost\n88 points in draft capital value"),
                  color = "#666666",
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#ffffff",
                  bg.r = 0.2,
                  hjust = 1,
                  vjust = 0) +
  geom_curve(aes(x = 9.25,
                 xend = 10,
                 y = -91,
                 yend = -91),
             size = 0.6,
             arrow = arrow(length = unit(0.2,"cm"),
                           type = "open"),
             curvature = .8,
             color = "#999999") +
  
  
  # geom_shadowtext(aes(x = 0.55,
  #                     y = -10,
  #                     label = "Each team starts a BBM3 draft\nwith around 786 points of capital"),
  #                 color = "#bbbbbb",
  #                 family = "Chivo",
  #                 fontface = "bold",
  #                 bg.color = "#ffffff",
  #                 bg.r = 0.2,
  #                 vjust = 1,
  #                 hjust = 0) +
  # geom_curve(aes(x = 0.5,
  #                xend = 0.5,
  #                y = -13,
  #                yend = 0),
  #            size = 1,
  #            arrow = arrow(length = unit(0.2,"cm"),
  #                          type = "open"),
  #            curvature = -.8,
  #            color = "#ffffff") +
  # geom_curve(aes(x = 0.5,
  #                xend = 0.5,
  #                y = -13,
  #                yend = 0),
  #            size = 0.6,
  #            arrow = arrow(length = unit(0.2,"cm"),
  #                          type = "open"),
  #            curvature = -.8,
  #            color = "#bbbbbb") +
  
  
  # Final touches
  labs(title = "ADP draft capital value <span style = color:'#ffffff'>........</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the average draft capital gained or lost by all BBM3<br>playoff teams, grouped into percentile buckets.",
       y = NULL,
       x = NULL,
       caption = plot_caption) +
  theme(plot.caption = element_markdown(lineheight = 1.1))

ggsave(figure_3,
       filename = "article_2_figure_3.png",
       width = 6.5,
       height = 6.5,
       units = "in")

# 
# 
# figure_3 = ggplot() +
#   
#   # Setting up the basics
#   theme_etr_white_2023() +
#   scale_x_continuous(limits = c(0.5, 1.5)) +
#   scale_y_continuous(limits = c(786-100, 786+100)) +
#   scale_color_identity() +
#   scale_fill_identity() +
#   
#   # Annotation rectabgle
#   geom_rect(aes(xmin = 0.98, xmax = 1.02,
#                 ymin = 750, ymax = 813),
#             color = "#999999",
#             fill = "#bbbbbb",
#             alpha = 0.25) +
#   
#   # Average starting capital line and annotation
#   geom_hline(yintercept = 786,
#              color = "#bbbbbb",
#              linetype = "dotted") +
#   geom_shadowtext(aes(x = 0.625,
#                       y = 786,
#                       label = "AVERAGE\nSTARTING\nCAPITAL"),
#                   color = "#bbbbbb",
#                   bg.color = "#ffffff",
#                   bg.r = 0.3,
#                   family = "Chivo",
#                   fontface = "bold") +
#   
#   # Adding data
# 
#   geom_point(data = figure_3_data,
#            aes(y = adp_draft_capital,
#                x = 0.75,
#                color = dot_color,
#                fill = dot_fill),
#            size = 5,
#            shape = 21) +
#   # geom_shadowtext(data = figure_3_data,
#   #                 aes(y = adp_draft_capital,
#   #                     x = 1.1,
#   #                     label = paste0("BUCKET", " ", adp_value_bucket)),
#   #                 color = "#bbbbbb",
#   #                 bg.color = "#ffffff",
#   #                 bg.r = 0.3,
#   #                 family = "Chivo",
#   #                 fontface = "bold") +
#   
#   # Annotation
#   geom_shadowtext(aes(x = 1.1,
#                     y = 776,
#                     label = "There are only 40\npicks worth of capital\nbetween bucket 3\nand bucket 8"),
#                 color = "#999999",
#                 bg.r = 0.6,
#                 bg.color = "#ffffff",
#                 family = "Chivo",
#                 hjust = 0,
#                 vjust = 0.5) +
#   geom_shadowtext(aes(x = 0.8,
#                       y = 850.6,
#                       label = "BUCKET 1"),
#                   color = "#333333",
#                   bg.r = 0.6,
#                   bg.color = "#ffffff",
#                   family = "Chivo",
#                   fontface = "bold",
#                   hjust = 0,
#                   vjust = 0.5) +
#   geom_shadowtext(aes(x = 1.05,
#                       y = 850.6,
#                       label = "851"),
#                   color = "#333333",
#                   bg.r = 0.6,
#                   bg.color = "#ffffff",
#                   family = "Chivo",
#                   hjust = 1,
#                   vjust = 0.5) +
#   geom_shadowtext(aes(x = 1.15,
#                       y = 850.6,
#                       label = "+65"),
#                   color = "#333333",
#                   bg.r = 0.6,
#                   bg.color = "#ffffff",
#                   family = "Chivo",
#                   fontface = "bold",
#                   hjust = 1,
#                   vjust = 0.5) +
#   
# 
# 
#   
#   # Final touches
#   labs(title = "ADP draft capital <span style = color:'#ffffff'>...................</span><img src = 'ETR_Full_Blk.png' height = '26'>",
#        subtitle = "Chart shows the average draft capital gained or lost by all<br>BBM3 playoff teams, grouped into percentile buckets.",
#        y = NULL,
#        x = NULL,
#        caption = plot_caption) +
#   theme(plot.caption = element_markdown(lineheight = 1.1))
# 
# ggsave(figure_3,
#        filename = "article_2_figure_3.png",
#        width = 6.5,
#        height = 6.5,
#        units = "in")
#   
#   



# # Figure 3 v 2
# baseline_capital = c(rep(786, 10))
# bucket_list = c(1:10)
# 
# figure_3_v2_data <- tibble(adp_draft_capital, baseline_capital,
#                            bucket_list)
# 
# ggplot() +
#   
#   # Setting up basics
#   theme_etr_white_2023() +
#   scale_x_continuous(breaks = c(1:10),
#                      position = "bottom") +
#   scale_y_continuous(limits = c(0,900)) +
#   scale_fill_identity() +
#   
#   # Data
#   geom_col(data = figure_3_v2_data,
#            aes(x = bucket_list,
#                y = adp_draft_capital),
#            fill = "#bbbbbb") +
#   geom_col(data = figure_3_v2_data,
#            aes(x = bucket_list,
#                y = 786),
#            fill = NA,
#            color = "#999999")
  

# Figure 1 and 3 combo ====

 #Do a little scatterplot

# Figure 4 ====
# Initial data
fig_4_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1wRAfZOeX2V_tbtBfTwCyOOQC-Z8pQEzYP33AWE2v7W0/edit#gid=1277344311",
                                sheet = "estimated_poff_win") %>% 
  clean_names()

# Value bucket data
figure_4_data_a <- fig_4_data_raw[c(1:10), c(1,5:8)]

figure_4_data_a <- tibble(figure_4_data_a) %>% 
  gather(key = poff_round,
         value = odds_change,
         finals_odd_change:quarters_odd_change) %>% 
  mutate(data_group = "<b>ADP Value<br>")

figure_4_data_b <- fig_4_data_raw[c(13:22), c(1,5:8)]
  
figure_4_data_b <- tibble(figure_4_data_b) %>% 
  gather(key = poff_round,
         value = odds_change,
         finals_odd_change:quarters_odd_change) %>% 
  mutate(data_group = "<b>Draft Capital<br>Value")

combined_figure_4_data <- bind_rows(
  figure_4_data_a,
  figure_4_data_b
) %>% 
  mutate(adp_value_bucket = as.numeric(adp_value_bucket)) %>% 
  mutate(bar_color = case_when(
    odds_change > 0 ~ etr_green,
    TRUE ~ etr_red
  )) %>% 
  mutate(facet_labels = case_when(
    poff_round == "finals_odd_change" ~ "<b>FINALS",
    poff_round == "semis_odd_change" ~"<b>SEMI<br>FINALS",
    TRUE ~ "<b>QTR<br>FINALS"
  )) %>% 
  mutate(facet_labels = factor(facet_labels,
                               levels = c("<b>QTR<br>FINALS",
                                          "<b>SEMI<br>FINALS",
                                          "<b>FINALS")))


figure_4 = ggplot() +
  
  # Setting up preliminaries
  theme_etr_white_2023() +
  scale_x_continuous(breaks = c(1:10)) +
  scale_y_continuous(limits = c(-.25,.25),
                     breaks = c(-.2,-.1,0,.1,.2),
                     labels = c("-20%", "-10%","", "+10%", "+20%")) +
  scale_color_identity() +
  scale_fill_identity() +
  geom_hline(yintercept = 0,
             color = "#333333") +
  
  # Adding data and facet
  geom_col(data = combined_figure_4_data,
           aes(x = adp_value_bucket,
               y = odds_change,
               fill = bar_color),
           width = 0.6) +
  facet_grid(rows = vars(facet_labels),
             cols = vars(data_group),
             switch = "y") +
  
  # Fnishing touches
  labs(title = "ADP and Draft Capital Value<span style = color:'#ffffff'>..</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the imact that ADP and Draft Capital value had on a<br>team's estimated playoff win rate in BBM3.",
       y = NULL,
       x = NULL,
       caption = plot_caption) +
  theme(
    plot.caption = element_markdown(lineheight = 1.1),
    strip.text = element_markdown(hjust = 0),
    strip.placement = "outside",
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(2, "lines"),
    strip.text.y.left = element_markdown(angle = 0,
                                         hjust = 0,
                                         lineheight = 0.8))


ggsave(figure_4,
       filename = "article_2_figure_4.png",
       width = 6.5,
       height = 8,
       units = "in")

 
# 
# 
# 
# figure_4 <- ggplot() +
#   
#   # Basic set up
#   theme_etr_white_2023() +
#   geom_hline(yintercept = 0,
#              color = "#333333") +
#   scale_y_continuous(labels = c("-20%",
#                                 "-10%",
#                                 "",
#                                 "+10%")) +
#   scale_x_continuous(breaks = c(1,2,3),
#                      labels = c("QTR<br>FNLS",
#                                 "SEMI<br>FNLS",
#                                 "FNLS")) +
#   
#   # Adding value bucket data
#   geom_line(data = figure_4_data_a ,
#             aes(x = round_x,
#                 y = odds_change,
#                 group = bucket),
#             color = etr_orange) +
#   geom_point(data = figure_4_data_a ,
#              aes(x = round_x,
#                  y = odds_change,
#                  group = bucket),
#              shape = 21,
#              size = 3,
#              color = "#ffffff",
#              fill = etr_orange) +
#   
#   # adding capital value data 
#   geom_line(data = figure_4_data_b,
#             aes(x = round_x,
#                 y = odds_change,
#                 group = bucket),
#             color = etr_purple) +
#   geom_point(data = figure_4_data_b,
#              aes(x = round_x,
#                  y = odds_change,
#                  group = bucket),
#              shape = 21,
#              size = 3,
#              color = "#ffffff",
#              fill = etr_purple) +
#   
#   facet_wrap(~ bucket) +
#   labs(title = "ADP and Draft Capital Value<span style = color:'#ffffff'>..</span><img src = 'ETR_Full_Blk.png' height = '26'>",
#        subtitle = "Chart shows how a team's <b><span style = color:#f88b01>ADP value</span></b> and <b><span style = color:#7d42be>Draft Capital value</span></b><br>impacted on its estimated playoff win odds in BBM3.",
#        y = NULL,
#        x = NULL,
#        caption = plot_caption,
#        tag = "Whilst there is a positive trend,<br>its magnitude is small.
#        <br><br>It's <b>good to be in the top 3-4<br>value buckets</b>, and <b>really bad<br>to be in the bottom 2.</b>") +
#   theme(plot.tag.position = c(0.55, 0.21),
#         plot.tag = element_markdown(hjust = 0,
#                                     lineheight = 1.1),
#         plot.caption = element_markdown(lineheight = 1.1))



# Do up a basic table with the actual data that could sit at the end of the article


# Figure 5 ==== 

# Simlar to figure 4 but for advance rates
fig_5_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1wRAfZOeX2V_tbtBfTwCyOOQC-Z8pQEzYP33AWE2v7W0/edit#gid=1277344311",
                                sheet = "reg_advance_rates") %>% 
  clean_names()

figure_5_data_a <- fig_5_data_raw[c(1:10), c(1,2,4)] %>% 
  mutate(bucket = as.numeric(adp_value_bucket)) %>% 
  mutate(measure = "<b>ADP Value")

figure_5_data_b <- fig_5_data_raw[c(14:23), c(1,2,4)] %>% 
  mutate(bucket = as.numeric(adp_value_bucket)) %>% 
  mutate(measure = "<b>Draft Capital Value")

figure_5_combined_data = bind_rows(
  figure_5_data_a,
  figure_5_data_b
) %>% 
  mutate(bar_color = case_when(
    odds_increase > 0 ~ etr_green,
    TRUE ~ etr_red
  ))


measure <- "<b>ADP Value"
x1 <- c(1.8)
x2 <- c(1)
y1 <- c(0.56)
y2 <- c(.51)

arrow_df_1 <- tibble(measure, x1, x2, y1, y2)

measure_2 <- "<b>Draft Capital Value"
x3 <- c(0.35)
x4 <- c(1)
y3 <- c(0.56)
y4 <- c(.51)

arrow_df_2 <- tibble(measure_2, x3, x4, y3, y4) %>% 
  rename(measure = measure_2)


# backing rectangle
xmin_a = 1.8
xmax_a = 10.25
ymin_a = 0.41
ymax_a =0.59

rec_df_a <- tibble(measure,
                   xmin_a,
                   xmax_a,
                   ymin_a,
                   ymax_a)

figure_5 = ggplot() +
  # Setting up plot basics
  theme_etr_white_2023() +
  scale_x_continuous(breaks = c(1:10),
                     expand = c(0,0),
                     limits = c(0.25,10.75),
                     position = "bottom") +
  scale_y_continuous(limits = c(-.61, .61),
                     breaks = c(-.6, -.4, -.2, .2, .4, .6),
                     labels = c("-60%", "-40%", "-20%", "+20%", "+40%", "+60%")) +
  scale_fill_identity() +
  
  # Data and facet
  geom_col(data = figure_5_combined_data,
           aes(x = bucket,
               y = odds_increase,
               fill = bar_color),
           width = 0.6) +
  facet_wrap(~ measure) +
  geom_hline(yintercept = 0,
             color = "#333333") +
  
  # Adding annotation arrows
  geom_curve(data = arrow_df,
             aes(x = x1, y =y1,
                 xend = x2, yend = y2),
             curvature = 0.5,
             color = "#999999",
             size = 0.4,
             arrow = arrow(type = "open",
                           length = unit(0.02, "npc"))) +
  geom_curve(data = arrow_df_2,
             aes(x = x3, y =y3,
                 xend = x4, yend = y4),
             size = 0.4,
             curvature = -0.5,
             color = "#999999",
             arrow = arrow(type = "open",
                           length = unit(0.02, "npc"))) +
  
  # Adding rectangle behind annotation
  geom_rect(data = rec_df_a,
           aes(xmin = xmin_a, xmax = xmax_a,
               ymin = ymin_a, ymax = ymax_a),
           fill = "#ffffff",
           color = "#ffffff") +
  
  # Finishing touches
  labs(title = "ADP and Draft Capital Value<span style = color:'#ffffff'>..</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the imact that ADP and Draft Capital value had on a<br>team's regular season advance rate in BBM3.",
       y = NULL,
       x = NULL,
       tag = "<span style = 'font-family:Chivo;color:#666666;font-size:9pt'><b>Across both measures the top 10% of teams<br>increased their advance rates by 50%",
       caption = plot_caption) +
  theme(plot.tag.position = c(.12225,.6175),
        plot.tag = element_markdown(hjust = 0,
                                    lineheight = 0.9))



ggsave(figure_5,
       filename = "article_2_figure_5.png",
       width = 6.5,
       height = 5,
       units = "in")


# Figure 6 ====

fig_6_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1wRAfZOeX2V_tbtBfTwCyOOQC-Z8pQEzYP33AWE2v7W0/edit#gid=1277344311",
                             sheet = "EV - ADP Value") %>% 
  clean_names()

figure_6_data_a <- fig_6_data_raw[c(1:10),c(1:6)] %>% 
  mutate_at(c(1:6), as.numeric) %>% 
  rename(bucket = 1) %>% 
  select(1,2,6,5,4,3) %>% 
  mutate(measure = "ADP Value")

figure_6_data_b <- fig_6_data_raw[c(14:23),c(1:6)] %>% 
  mutate_at(c(1:6), as.numeric) %>% 
  rename(bucket = 1) %>% 
  select(1,2,6,5,4,3) %>% 
  mutate(measure = "Draft Capital Value")
  

# Tall condensed table
figure_6_combined_data <- bind_rows(
  figure_6_data_a,
  figure_6_data_b
) %>% 
  mutate(trend_arrow_reg_season = reg_season_advance_rate - 0.1667,
         trend_arrow_quarters = quarters_win_rate - .1,
         trend_arrow_semis = semis_win_rate - .063,
         trend_arrow_finals = finals_win_rate - .0021)
         
figure_6_condensed_data <- figure_6_combined_data %>% 
  select(7,1,2,8:11) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "")

figure_6_tall_condensed_table <- gt(figure_6_condensed_data,
   groupname_col = "measure") %>% 
  gt_theme_etr_white() %>% 
  tab_options(row_group.font.weight = "bold") %>% 
  
  # cols move
  cols_move(columns = buffer_1, after = bucket) %>% 
  cols_move(columns = buffer_2, after = ev) %>% 
  
  # Setting column names
  cols_label(bucket = "VALUE BUCKET",
             ev = "EV",
             trend_arrow_reg_season = "REG SEASON",
             trend_arrow_quarters = "QTR FINALS",
             trend_arrow_semis = "SEMI FINALS",
             trend_arrow_finals = "FINALS",
             buffer_1 = "",
             buffer_2 = "") %>% 
  
  # column widths 
  cols_width(bucket ~ px(80),
             ev ~ px(100),
             trend_arrow_reg_season ~ px(60),
             trend_arrow_quarters ~ px(60),
             trend_arrow_semis ~ px(60),
             trend_arrow_finals ~ px(60),
             buffer_1 ~ px(20),
             buffer_2 ~ px(20)
  ) %>% 
  
  # Adding spanner
  tab_spanner(columns = c(trend_arrow_reg_season:trend_arrow_finals),
              label = "PERFORMANCE vs BASELINE ADVANCE/ESTIMATED WIN RATES") %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_spanners()) %>% 
  
  # column alignment 
  
  cols_align(align = "center",
             columns = ev) %>% 
  cols_align(align = "left",
             columns = c(trend_arrow_reg_season,
                         trend_arrow_quarters,
                         trend_arrow_semis,
                         trend_arrow_finals)) %>% 
  
  # Adding trend arrows
  gt_fa_rank_change(column = trend_arrow_reg_season,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  gt_fa_rank_change(column = trend_arrow_quarters,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  gt_fa_rank_change(column = trend_arrow_semis,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  gt_fa_rank_change(column = trend_arrow_finals,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  
  # EV heatmap
  gt_color_rows(columns = c(ev),
                palette = c(etr_red,
                            "#ffffff",
                            etr_green),
                domain = c(5,
                           22.16,
                           39.16)) %>% 

# Formatting ev column
fmt_currency(columns = ev,
             currency = "USD",
             decimals = 2)  %>% 
  
  # FInishing touches
  tab_header(
    title = md("**ADP and Draft Capital Value**"),
    subtitle = md("Table shows the impact that ADP and Draft Capital value had on a teams's overall EV in BBM3")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_source_note(source_note = "Data: Underdog")
  

gtsave(figure_6_tall_condensed_table,
       filename = "figure_6_tall_condensed_table.html")

webshot2::webshot("figure_6_tall_condensed_table.html",
                  "figure_6_tall_condensed_table.png",
                  vwidth = 500,
                  zoom = 2)


# Figure 6 Wide condensed table
figure_6_wide_condensed_data <- left_join(
  figure_6_data_a, figure_6_data_b,
  by = "bucket"
) %>% 
  select(1:6,8:12) %>% 
  mutate(trend_arrow_reg_season.x = reg_season_advance_rate.x - 0.1667,
         trend_arrow_quarters.x = quarters_win_rate.x - .1,
         trend_arrow_semis.x = semis_win_rate.x - .063,
         trend_arrow_finals.x = finals_win_rate.x - .0021) %>% 
  mutate(trend_arrow_reg_season.y = reg_season_advance_rate.y - 0.1667,
         trend_arrow_quarters.y = quarters_win_rate.y - .1,
         trend_arrow_semis.y = semis_win_rate.y - .063,
         trend_arrow_finals.y = finals_win_rate.y - .0021) %>% 
  select(1,2,12:15, 7, 16:19) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "") %>% 
  select(1,12,2,13,3:6,14,7,15,8:11)
  # # Adding baseline information row
  # add_row(bucket = 0,
  #         ev.x = 22.16,
  #         ev.y = 22.16,
  #         trend_arrow_reg_season.x = 0,
  #         trend_arrow_quarters.x = 0,
  #         trend_arrow_semis.x = 0,
  #         trend_arrow_finals.x = 0,
  #         trend_arrow_reg_season.y = 0,
  #         trend_arrow_quarters.y = 0,
  #         trend_arrow_semis.y = 0,
  #         trend_arrow_finals.y = 0,
  #         buffer_1 = "",
  #         buffer_2 = "",
  #         buffer_3 = "",
  #         buffer_4 = "",.before = 1)


figure_6_wide_condensed_table <- gt(figure_6_wide_condensed_data) %>% 
  gt_theme_etr_white() %>% 
  
  # Colum labels
  cols_label(bucket = "VALUE BUCKET",
             ev.x = "EXPECTED VALUE",
             ev.y = "EXPECTED VALUE",
             trend_arrow_reg_season.x = "REG SEASON",
             trend_arrow_quarters.x = "QTR FINALS",
             trend_arrow_semis.x = "SEMI FINALS",
             trend_arrow_finals.x = "FINALS",
             ev.x = "EXPECTED VALUE",
             trend_arrow_reg_season.y = "REG SEASON",
             trend_arrow_quarters.y = "QTR FINALS",
             trend_arrow_semis.y = "SEMI FINALS",
             trend_arrow_finals.y = "FINALS",
             buffer_1 = "",
             buffer_2 = "",
             buffer_3 = "",
             buffer_4 = ""
             ) %>% 
  
  # Adding spanners
  tab_spanner(label = md("<b>ADP VALUE</b><br>PERFORMANCE vs BASELINE ADVANCE/ESTIMATED WIN RATES"),
              columns = c(ev.x, buffer_2, 
                          trend_arrow_reg_season.x,
                          trend_arrow_quarters.x,
                          trend_arrow_semis.x,
                          trend_arrow_finals.x)) %>% 
  tab_spanner(label = md("<b>DRAFT CAPITAL VALUE</b><br>PERFORMANCE vs BASELINE ADVANCE/ESTIMATED WIN RATES"),
              columns = c(ev.y, buffer_4, 
                          trend_arrow_reg_season.y,
                          trend_arrow_quarters.y,
                          trend_arrow_semis.y,
                          trend_arrow_finals.y)) %>% 
  
  # Column widths
  cols_width(bucket ~ px(60),
             ev.x ~ px(90),
             ev.y ~ px(90),
             trend_arrow_reg_season.x ~ px(65),
             trend_arrow_quarters.x ~ px(65),
             trend_arrow_semis.x ~ px(65),
             trend_arrow_finals.x ~ px(65),
             trend_arrow_reg_season.y ~ px(65),
             trend_arrow_quarters.y ~ px(65),
             trend_arrow_semis.y ~ px(65),
             trend_arrow_finals.y ~ px(65),
             buffer_1 ~ px(30),
             buffer_2 ~ px(30),
             buffer_3 ~ px(30),
             buffer_4 ~ px(30)) %>% 
  
  # column alignment
  cols_align(align = "left",
             columns = c(5:8, 12:15)) %>% 
  cols_align(align = "center",
             columns = c(1,3,10)) %>% 
  
  # Adding trend arrows
  gt_fa_rank_change(column = trend_arrow_reg_season.x,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_quarters.x,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_semis.x,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_finals.x,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_reg_season.y,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_quarters.y,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_semis.y,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red))  %>% 
  gt_fa_rank_change(column = trend_arrow_finals.y,
                    fa_type = "arrow",
                    show_text = FALSE,
                    palette = c(etr_green,
                                "#ffffff",
                                etr_red)) %>% 
  
  # Number formatting
  # Formatting ev column
  fmt_currency(columns = c(ev.x, ev.y),
               currency = "USD",
               decimals = 2)  %>% 
  
  # Heatmapping ev column
  # EV heatmap
  gt_color_rows(columns = c(ev.x, ev.y),
                palette = c(etr_red,
                            "#ffffff",
                            etr_green),
                domain = c(5,
                           22.16,
                           39.16)) %>% 
  
  # FInishing touches
  tab_header(
    title = md("**ADP and Draft Capital Value**"),
    subtitle = md("Table shows the impact that ADP and Draft Capital value had on a teams's overall EV in BBM3")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_source_note(source_note = "Data: Underdog")


gtsave(figure_6_wide_condensed_table,
       filename = "figure_6_wide_condensed_table.html")

webshot2::webshot("figure_6_wide_condensed_table.html",
                  "figure_6_wide_condensed_table.png",
                  vwidth = 900,
                  zoom = 2)


# Heatmap option
figure_6_wide_heatmap_data <- left_join(
  figure_6_data_a, figure_6_data_b,
  by = "bucket"
) %>% 
  select(1:6,8:12) %>% 
  mutate( reg_season_odds_increase.x = reg_season_advance_rate.x / .1667 - 1,
          quarters_odds_increase.x = quarters_win_rate.x / .1 - 1,
          semis_odds_increase.x = semis_win_rate.x / .063 - 1,
          finals_odds_increase.x = finals_win_rate.x / .0021 - 1) %>% 
  mutate( reg_season_odds_increase.y = reg_season_advance_rate.y / .1667 - 1,
          quarters_odds_increase.y = quarters_win_rate.y / .1 - 1,
          semis_odds_increase.y = semis_win_rate.y / .063 - 1,
          finals_odds_increase.y = finals_win_rate.y / .0021 - 1) %>% 
  select(1,2,12:15, 7, 16:19) %>% 
  mutate(buffer_1 = "",
         buffer_2 = "",
         buffer_3 = "",
         buffer_4 = "") %>% 
  select(1,12,2,13,3:6,14,7,15,8:11)

# Creating table
gt(figure_6_wide_heatmap_data) %>% 
  gt_theme_etr_white() %>% 
  
  # Colum labels
  cols_label(bucket = "VALUE BUCKET",
             ev.x = "EXPECTED VALUE",
             ev.y = "EXPECTED VALUE",
             reg_season_odds_increase.x = "REG SEASON",
             quarters_odds_increase.x = "QTR FINALS",
             semis_odds_increase.x = "SEMI FINALS",
             finals_odds_increase.x = "FINALS",
             reg_season_odds_increase.y = "REG SEASON",
             quarters_odds_increase.y = "QTR FINALS",
             semis_odds_increase.y = "SEMI FINALS",
             finals_odds_increase.y = "FINALS",
             buffer_1 = "",
             buffer_2 = "",
             buffer_3 = "",
             buffer_4 = "")  %>% 
  
  # Adding spanners
  tab_spanner(label = md("<b>ADP VALUE</b><br>INCREASE IN ODDS OF ADVANCE/ESTIMATED PLAYOFF WIN"),
              columns = c(ev.x, buffer_2, 
                          reg_season_odds_increase.x,
                          quarters_odds_increase.x,
                          semis_odds_increase.x,
                          finals_odds_increase.x)) %>% 
  tab_spanner(label = md("<b>DRAFT CAPITAL VALUE</b><br>INCREASE IN ODDS OF ADVANCE/ESTIMATED PLAYOFF WIN"),
              columns = c(ev.y, buffer_4, 
                          reg_season_odds_increase.y,
                          quarters_odds_increase.y,
                          semis_odds_increase.y,
                          finals_odds_increase.y)) %>% 
  
  # Column widths
  cols_width(bucket ~ px(80),
             ev.x ~ px(90),
             ev.y ~ px(90),
             reg_season_odds_increase.x ~ px(75),
             quarters_odds_increase.x ~ px(75),
             semis_odds_increase.x ~ px(75),
             finals_odds_increase.x ~ px(75),
             reg_season_odds_increase.y ~ px(75),
             quarters_odds_increase.y ~ px(75),
             semis_odds_increase.y ~ px(75),
             finals_odds_increase.y ~ px(75),
             buffer_1 ~ px(30),
             buffer_2 ~ px(10),
             buffer_3 ~ px(30),
             buffer_4 ~ px(10)) %>% 
  
  # column alignment
  cols_align(align = "right",
             columns = c(5:8, 12:15)) %>% 
  cols_align(align = "center",
             columns = c(1,3,10)) %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(5:8, 12:15))) %>% 
  
  tab_style(
    style = cell_text(weight = "bold"),
    location = cells_body(columns = c(3,10))) %>% 
  
  # Number formatting
  fmt_currency(columns = c(ev.x, ev.y),
               currency = "USD",
               decimals = 2)  %>% 
  fmt_percent(columns = c(5,12),decimals = 1) %>%
  fmt_percent(columns = c(6:8, 13:15),decimals = 1) %>%
  
  # Heatmapping odds increase columns
  gt_color_rows(columns = c(5:8, 12:15),
                palette = c(etr_red,
                            "#ffffff",
                            etr_green),
                domain = c(-0.55,0,0.55)) %>% 
  
  # FInishing touches
  tab_header(
    title = md("**ADP and Draft Capital Value**"),
    subtitle = md("Table shows the impact that ADP and Draft Capital value had on a teams's overall EV in BBM3")) %>% 
  tab_style(
    style = cell_text(align = "right",
                      color = "#808080"),
    locations = cells_source_notes()
  ) %>% 
  tab_source_note(source_note = "Data: Underdog")
  


# Figure 6 Viz option

figure_6_viz_option_data <- figure_6_wide_heatmap_data %>% 
  select(1,5:8,12:15) %>% 
  gather(key = round,
         value = odds_change,
         2:9)



# Figure 7 Advance rates vs playoff win rates see saw ====




# Figure 8 Live players - like the late figure last time ====

fig_8_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1wRAfZOeX2V_tbtBfTwCyOOQC-Z8pQEzYP33AWE2v7W0/edit#gid=1277344311",
                             sheet = "non_zero_players") %>% 
  clean_names()
       

figure_8_data <- fig_8_data_raw[c(2:14), c(1,5,6)] %>% 
  rename(non_zero_players = 1,
         count = 2,
         ev = 3) %>% 
  mutate(count = as.numeric(count),
         ev = as.numeric(ev),
         count_percent = count / sum(count)) %>% 
  mutate(non_zero_players = as.numeric(non_zero_players)) %>% 
  mutate(non_zero_players = replace_na(non_zero_players, 6)) %>% 
  mutate(count_percent = round(count_percent, 2)) %>% 
  mutate(count_percent_label = percent(count_percent,accuracy = 1)) %>% 
  mutate(count_percent_label = case_when(
    count_percent_label== "0%" ~ "<1%",
    TRUE ~ count_percent_label
  ))

figure_8 <- ggplot() +
  
  # Plot basics
  theme_etr_white_2023() +
  theme(panel.grid.major.x = element_blank()) +
  scale_x_continuous(breaks = c(6:18),
                     limits = c(5.6,18.4),
                     expand = c(0,0),
                     position = "top",
                     labels = c("6\nor less",
                                "7\n",
                                "8\n",
                                "9\n",
                                "10\n",
                                "11\n",
                                "12\n",
                                "13\n",
                                "14\n",
                                "15\n",
                                "16\n",
                                "17\n",
                                "18\n")) +
  scale_y_continuous(limits = c(-100,400),
                     breaks = c(0,100,200,300, 400),
                     labels = c("$0",
                                "$100",
                                "$200",
                                "$300",
                                "$400")) +
  scale_fill_gradient(low = "#ffffff",
                      high = "#bbbbbb") +

  # Adding data
  geom_col(data = figure_8_data %>% filter(ev > 133),
           aes(x = non_zero_players,
               y = ev),
           width = 0.6,
           fill = etr_green) +
  geom_col(data = figure_8_data %>% filter(ev < 133),
           aes(x = non_zero_players,
               y = ev),
           width = 0.6,
           fill = etr_red) +
  geom_hline(yintercept = 0,
             color = "#333333") +
  
  # Adding contextual information lines
  geom_hline(yintercept = 133,
             color = "#bbbbbb",
             lty="11") +
  geom_shadowtext(aes(x = 5.8,
                      y = 133,
                      label = "BASELINE EV: $133"),
                  color = "#bbbbbb",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  hjust = 0,
                  family = "Chivo",
                  fontface = "bold",
                  size = 3) +
  
  # adding 3 game stacked QBs ev
  geom_hline(yintercept = 164,
             color = "#999999",
             lty="11") +
  geom_shadowtext(aes(x = 5.8,
                      y = 164,
                      label = "3 GAME STACKED QBs EV: $164"),
                  color = "#999999",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  hjust = 0,
                  family = "Chivo",
                  fontface = "bold",
                  size = 3) +
  
  # Adding team count tiles
  geom_tile(data = figure_8_data,
            aes(x = non_zero_players,
                y = -75,
                fill = count_percent),
            color = "#999999",
            width = 0.75,
            height = 50,
            show.legend = FALSE) +
  geom_shadowtext(data = figure_8_data,
                  aes(x = non_zero_players,
                      y = -75,
                      label = count_percent_label),
                  color = "#666666",
                  bg.color = "#ffffff",
                  bg.r = 0.1,
                  hjust = 0.5,
                  family = "Chivo",
                  size = 3) +
  geom_shadowtext(aes(x = 5.65,
                      y = -40,
                      label = "% of teams"),
                  color = "#333333",
                  bg.color = "#ffffff",
                  bg.r = 0.6,
                  hjust = 0,
                  family = "Chivo",
                  size = 3,
                  fontface = "bold") +
  
  # Adding annotation
  # geom_shadowtext(aes(x = 14.45,
  #                     y = 85,
  #                     label = "Having 14+ live players on a\nroster is significantly more\nimpactful on expected value\nthan having 3 game stacked QBs"),
  #                 color = "#333333",
  #                 bg.color = "#ffffff",
  #                 bg.r = 0.6,
  #                 hjust = 0,
  #                 family = "Chivo",
  #                 size = 3) +
  
  # Finishing touches
  labs(title = "Number of live players<span style = color:'#ffffff'>..........</span><img src = 'ETR_Full_Blk.png' height = '26'>",
       subtitle = "Chart shows the impact that the number of live players on a roster<br>during the playoffs has on the expected value of that team",
       y = NULL,
       x = NULL,
       caption = plot_caption)


ggsave(figure_8,
       filename = "article_2_figure_8.png",
       width = 6.5,
       height = 5,
       units = "in")


# PULL together a table alterantive

number of players | qf improvement | semis improvement | finals improvement | EV improvement | % of teams

