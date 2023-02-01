library(tidyverse)
library(janitor)
library(zoo)
library(ggtext)
library(ggimage)

# Importing data ====

data_2022 <- read_csv("data/pff 2022 dk stats rushing receiving download.csv") %>% 
  clean_names() %>% 
  select(player,
         team,
         games,
         position,
         rush_carries,
         rush_yds,
         rush_tds,
         ypc, # yards per carry
         yac, # yards after contact per carry
         tat, # tackles avoided per carry
         rec_targ,
         rec_yds,
         rec_tds,
         rac, # yards after the catch per reception
         ypt, #yards per target
         ypr)#yards per reception



# Reformatting data and calculating percentiles for each measure

rbs_2022 <- data_2022 %>% 
  # filtering for RBs only
  filter(position == "RB") %>% 
  # filtering to only get players who played 6 games or more
  filter(games > 5) %>% 
  # transforming to long data
  gather(key = metric,
         value = value,
         rush_carries:ypr) %>% 
  # calculating percentiles for each metric
  group_by(metric) %>% 
  mutate(percentile_rank = ntile(value,100)) %>% 
  ungroup()

# Creating list of High ADP RBs ====

high_rbs <- read_csv("data/final etr ranks 08 september 2022.csv") %>% 
  clean_names() %>% 
  select(player, adp,
         position) %>% 
  filter(position == "RB") %>% 
  filter(adp < 25) %>% 
  # rearranging to allow use of this list to create factor to control later player ordering on plot
  arrange(adp)

# Filtering data for only the top ADP RBs ====

high_rbs_2022 <- rbs_2022 %>% 
  filter(player %in% high_rbs$player)

testing_all_rbs_in <- high_rbs_2022 %>% 
  group_by(player) %>% 
  summarise(count = n())

# Creating a field to group metrics ====

high_rbs_2022 <- high_rbs_2022 %>% 
  mutate(stat_group = case_when(
    metric == "rush_carries" ~ "RUSHING",
    metric == "rush_yds" ~ "RUSHING",
    metric == "rush_tds" ~ "RUSHING",
    metric == "ypc" ~ "RUSHING",
    metric == "yac" ~ "RUSHING",
    metric == "tat" ~ "RUSHING",
    metric == "rec_targ" ~ "RECEIVING",
    metric == "rec_yds" ~ "RECEIVING",
    metric == "rec_tds" ~ "RECEIVING",
    metric == "rac" ~ "RECEIVING",
    metric == "ypt" ~ "RECEIVING",
    metric == "ypr" ~ "RECEIVING"
  ))

# Creating metrics as a factor to control layout ====
high_rbs_2022 <- high_rbs_2022 %>% 
  mutate(metric = factor(metric,
                         levels = c(
                           "rush_carries",
                           "rush_yds",
                           "rush_tds",
                           "ypc",
                           "yac",
                           "tat",
                           "rec_targ",
                           "rec_yds",
                           "rec_tds",
                           "rac",
                           "ypt",
                           "ypr")))

# Creating column labels

high_rbs_2022 <- high_rbs_2022 %>% 
  mutate(metric_full_name = case_when(
    metric == "rush_carries" ~ "RUSHING<br>ATTEMPTS",
    metric == "rush_yds" ~ "RUSHING YARDS",
    metric == "rush_tds" ~ "RUSHING TDS",
    metric == "ypc" ~ "YARDS/CARRY",
    metric == "yac" ~ "YARDS AFTER<br>CONTACT/CARRY",
    metric == "tat" ~ "TACKLES<br>AVOIDED/CARRY",
    metric == "rec_targ" ~ "RECEIVING<br>TARGETS",
    metric == "rec_yds" ~ "RECEIVING YARDS",
    metric == "rec_tds" ~ "RECEIVING TDS",
    metric == "rac" ~ "RECEIVING YARDS<br>AFTER CATCH",
    metric == "ypt" ~ "RECEIVING<br>YARDS/TARGET",
    metric == "ypr" ~ "RECEIVING<br>YARDS/REC"
  )) %>% 
  mutate(bar_label = paste0("<span style='color:#bbbbbb;font-size:8pt;'><b>",
                            metric_full_name,
                            "<br>",
                            "<span style='color:#808080;font-size:12pt;'><b>",
                            value))


# Creating indivdual player labels ====

player_adps <- high_rbs %>% select(player, adp)

high_rbs_2022 <- high_rbs_2022 %>% 
  left_join(player_adps,
            by = "player") %>% 
  mutate(player_labels = paste0("<span style='color:#666666;font-size:32pt;'><b>",
                                player,
                                "<br>",
                                "<span style='color:#999999;font-size:20pt;'><b>",
                                "FINAL ADP: ",
                                adp)) 




# Creating circle cropped player images ====
source('Circle Cropping Headshots Background Script.R')

sample_of_players <- nflreadr::load_rosters() %>% 
  filter(full_name %in%high_rbs_2022$player) %>% 
  select(full_name,
         headshot_url)

all_players <- sample_of_players$full_name

walk(all_players, crop_and_save_image)

player_images <- sample_of_players %>% 
  mutate(headshot_centered = str_glue("cropped images/{to_snake_case(full_name)}.png")) %>% 
  mutate(headshot_cropped = circle_crop(headshot_centered)) %>% 
  rename(player = full_name)

high_rbs_2022 <- left_join(high_rbs_2022,
                           player_images,
                           by = "player")

# Creating player labels as factor to control ordering ====

player_label_order <- high_rbs_2022 %>% 
  select(player,
         player_labels,
         adp) %>% 
  group_by(player_labels) %>% 
  distinct() %>% 
  arrange(adp)

high_rbs_2022 <- high_rbs_2022 %>% 
  mutate(player_labels = factor(player_labels,
                                levels = player_label_order$player_labels))



# Caption script ====
source('etr 2023 dataviz caption.R')
caption = paste0("Chart by Michael Heery | ",
                 social_caption(icon_color ="#666666",
                                bg_color="#ffffff",
                                font_color = "#999999",
                                font_family="Chivo"),
                 " | <b>Data</b>: PFF")

# Creating plot ====

source('Final ETR dataviz themes.R')


high_adp_rb_performance_2022_plot <- ggplot() +
  theme_etr() +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_markdown(family = "Chivo"),
        panel.spacing = unit(3, "lines"),
        plot.title = element_markdown(family = "Chivo",
                                      face = "bold",
                                      size = 50,
                                      color = "#333333",
                                      hjust = 0),
        plot.subtitle = element_markdown(hjust = 0,
                                         family = "Chivo",
                                         color = "#999999",
                                         size = 34),
        legend.text = element_markdown(family = "Chivo",
                                       color = "#666666",
                                       size = 20,
                                       margin = margin(t =1,
                                                                b =1,
                                                                unit = "lines")),
        legend.title = element_markdown(family = "Chivo",
                                        color = "#999999",
                                        face = "bold",
                                        size = 20,
                                        margin = margin(t =1,
                                                        b =1,
                                                        unit = "lines")),
        legend.key.height = unit(0.8, unit = "cm"),
        plot.margin = margin(t = 1,
                             b = 1,
                             r = 0.1,
                             l = 0.1,
                             unit = "lines")) +
  geom_hline(yintercept= c(0,25,50,75,100),
             colour="#dddddd",
             alpha=0.5,
             linewidth = 0.2) +
  geom_vline(xintercept = c(1:18),
             color = "#dddddd",
             linewidth = 0.2) +
  geom_col(data = high_rbs_2022,
           aes(x = metric,
               y = percentile_rank,
               fill = stat_group),
           width = 1,
           color = "#ffffff",
           alpha = 0.9) +
  geom_image(data = high_rbs_2022,
             aes(x =1,
                 y=-20,
                 image=headshot_cropped),
             size=0.11,
             image_fun = border) +
  geom_hline(yintercept = 0,
             color = "#333333",
             linewidth = 0.6) +
  geom_richtext(data = high_rbs_2022,
                aes(x = metric,
                    y = 130,
                    label = bar_label),
                family = "Chivo",
                fill = NA,
                label.size = NA) +
  facet_wrap(~ player_labels,
             ncol = 3) +
  coord_polar(clip = "off") +
  scale_fill_manual(values = c("#f88b01",
                               "#7d42be")) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(-20,130),
                     breaks = c(0,25,50,75,100),
                     expand = c(0,0),
                     labels = NULL) +
  labs(title = "Highest value Running Backs 2022",
       subtitle = "Plots show rushing and receiving performance for RBs with a<br>top-24 ADP at the end of 2022 Bestball season",
       x = NULL,
       y = NULL,
       fill = "STATS CATEGORY",
       caption = paste0(
         "Percentile performance calculated against all other RBs who played at 6 games during the 2022 season<br>",
         "<br>",
         caption))

ggsave(high_adp_rb_performance_2022_plot,
       filename = "high_adp_rb_performance_2022_plot.png",
       unit = "in",
       height = 24,
       width = 16,
       dpi = "retina")
