# Load relevant libraries and fonts etc ====

library(tidyverse)
library(janitor)
library(zoo)
library(ggimage)
library(ggtext)
source('Final ETR dataviz themes.R')
library(fontawesome)
library(showtext)
library(statebins)
library(shadowtext)

font_add_google('Chivo', 'Chivo')
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
showtext::showtext_opts(dpi = 300)

# Specify player ====

player_full_name <- "Jahmyr Gibbs"
player_first_name <- "Jahmyr"
player_surname <- "Gibbs"
player_age <- "20.8 YEARS"
player_experience <- "3 YEARS"
player_height <- "INSERT"
player_weight <- "INSERT lbs"
headshot_url <- "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4429795.png&w=350&h=254"

# College History
years <- c(2020,2021,2022)
college <- c("Georgia Tech",
             "Georgia Tech",
             "Alabama")

# Importing data ====

data <- read_csv(paste0(
  "data/",
  player_full_name,
  " stats.csv")) %>% 
  clean_names() %>% 
  mutate(player = na.locf(player),
         year = na.locf(year)) %>% 
  mutate(value = round(value, 1))  %>% 
select(-x5,-x6,-x7)

# Getting cropped headshot ====

full_name <- player_full_name

sample_of_players <- tibble(
  full_name,
  headshot_url
)

# Processing headshot ====
source('Circle Cropping Headshots Background script.R')

all_players <- sample_of_players$full_name

walk(all_players, crop_and_save_image)

player_images <- sample_of_players %>% 
  mutate(headshot_centered = str_glue("cropped images/{to_snake_case(full_name)}.png")) %>% 
  mutate(headshot_cropped = circle_crop(headshot_centered)) %>% 
  rename(player = full_name)

# Setting columns coordinates ====

data <- data %>% 
  mutate(x_position = case_when(
    measure == "games" ~ 2,
    measure == "rush_avg" ~ 3,
    measure ==  "rec_avg" ~ 4,
    measure == "rec_yd_pta" ~ 5,
    measure == "adj_ypp" ~ 6
  )) %>% 
  mutate(y_position = case_when(
    year == 2022 ~ 2.5,
    year == 2021 ~ 3.5,
    year == 2020 ~ 4.5
  )) %>% 
  mutate(column_headers = case_when(
    measure == "games" ~ "<br>GAMES",
    measure == "rush_avg" ~ "AVG. RUSHING YARDS<br>PER GAME",
    measure ==  "rec_avg" ~ "AVG. RECEIVING YARDS<br>PER GAME",
    measure == "rec_yd_pta" ~ "RECEIVING YARDS<br>PER TEAM ATTEMPT",
    measure == "adj_ypp" ~ "ADJUSTED YARDS<br>PER PLAY"
  )) 

# Player name annotation ====

player_annotation <- paste0(
  "<span style = 'font-family: Chivo;font-size:36pt;color: #333333'>",
  player_first_name,
  " <b>",
  player_surname,
  "</b>",
  "<span style = 'color:white;font-size:4pt;'>...</span>",
  "<br>",
  "<span style = 'font-family:Chivo;font-size:6pt;color: #999999'>",
  "Age: ",
  "<span style = 'font-family:Chivo;font-size:9pt;color: #333333'>",
  "<b>",
  player_age,
  "</b>",
  "<span style = 'color:white;font-size:14pt;'>...</span>",
  "<span style = 'font-family:Chivo;font-size:6pt;color: #999999'>",
  "Experience: ",
  "<span style = 'font-family:Chivo;font-size:9pt;color: #333333'>",
  "<b>",
  player_experience,
  "</b>",
  "<span style = 'color:white;font-size:14pt;'>...</span>",
  "<span style = 'font-family:Chivo;font-size:6pt;color: #999999'>",
  "Height: ",
  "<span style = 'font-family:Chivo;font-size:9pt;color: #333333'>",
  "<b>",
  player_height,
  "</b>",
  "<span style = 'color:white;font-size:14pt;'>...</span>",
  "<span style = 'font-family:Chivo;font-size:6pt;color: #999999'>",
  "Weight: ",
  "<span style = 'font-family:Chivo;font-size:9pt;color: #333333'><b>",
  player_weight)

# Season annotation ====


y_position = rev(c(2.5,3.5,4.5))
season_annotation <- tibble(years, college,y_position) %>% 
  mutate(annotation = paste0(
    years,
    ": ",
    college
  ))

# Creating colour scales for each metric ====

data <- data %>% 
  group_by(measure) %>% 
  mutate(max_value = max(value)) %>% 
  mutate(fill_value = value / max_value)


# Creating caption ====

caption = paste0(
  "<span style = 'font-size:8pt;'>Chart created by Michael Heery<br>",
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style = 'color:white;font-size:8pt;'>.</span>",
  " @heerymichael",
  "<span style = 'color:white;font-size:8pt;'>...</span>",
  "<span style='font-family:fb;'>&#xf08c;</span>",
  "<span style = 'color:white;font-size:8pt;'>.</span>",
  " michaelheery </span>")

# Round borders for geom_tile function ====

`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

GeomRtile <- ggproto("GeomRtile", 
                     statebins:::GeomRrect, # 1) only change compared to ggplot2:::GeomTile
                     
                     extra_params = c("na.rm"),
                     setup_data = function(data, params) {
                       data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                       data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                       
                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                 ymin = y - height / 2, ymax = y + height / 2, height = NULL
                       )
                     },
                     default_aes = aes(
                       fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                       alpha = NA, width = NA, height = NA
                     ),
                     required_aes = c("x", "y"),
                     
                     # These aes columns are created by setup_data(). They need to be listed here so
                     # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                     # limits, not just those for which x and y are outside the limits
                     non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                     draw_key = draw_key_polygon
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"), # 2) add radius argument
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile, # 3) use ggproto object here
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

# Creating basic plot ====

plot_name <- paste0(player_first_name,
                    "_",
                    player_surname,
                    "_profile_summary_plot") 

profile_summary_plot <- ggplot() +
  scale_x_continuous(limits = c(0.75, 6.25),
                     breaks = NULL) +
  scale_y_reverse(limits = c(5,-1.75),
                  expand = c(0,0),
                  breaks = NULL) +
  theme_etr() +
  theme(plot.caption = element_markdown(lineheight = 0.5),
        panel.grid.major.y = element_blank()) +
  geom_point(aes(x = 1,
                 y = -0.25),
             color = "#999999",
             fill = "#ffffff",
             shape = 21,
             size = 30) +
  geom_image(data = player_images,
             aes(x = 1,
                 y = -0.25,
                 image = headshot_cropped),
             size = 0.16,
             asp = 2.9) +
  geom_point(aes(x = 1,
                 y = -0.25),
             color = "#ffffff",
             fill = NA,
             stroke = 6,
             shape = 21,
             size = 30) +
  geom_point(aes(x = 1,
                 y = -0.25),
             color = "#333333",
             fill = NA,
             stroke = 0.4,
             shape = 21,
             size = 30) +
  geom_richtext(aes(x = 1.6,
                    y = -0.25,
                    label = player_annotation),
                family = "Chivo",
                fill = NA,
                label.size = NA,
                hjust = 0) +
  geom_richtext(data = data,
                aes(x = x_position,
                    y = 1.75,
                    label = column_headers),
                size = 2,
                family = "Chivo",
                color = "#999999",
                fill = NA,
                label.size = NA,
                hjust = 0.5,
                vjust = 0.35) +
  geom_rtile(data = data,
            aes(x = x_position,
                y = y_position,
                fill = fill_value),
            width = 0.5,
            height = 0.7,
            show.legend = FALSE) +
  scale_fill_gradientn(limits = c(min(data$fill_value),
                                  max(data$fill_value)),
                       colours = c("#EBF7ED",
                                   "#37af4a")) +
  geom_shadowtext(data = data,
                  aes(x = x_position,
                      y = y_position,
                      label = value),
                  color = "#333333",
                  family = "Chivo",
                  bg.r = 0.275,
                  bg.color = "#ffffff",
                  show.legend = FALSE,
                  size = 2.75) + 
  # geom_richtext(data = data,
  #               aes(x = x_position,
  #                   y = y_position,
  #                   label = value),
  #               size = 2.75,
  #               family = "Chivo",color = "#333333",
  #               fill = NA,
  #               label.size = NA,
  #               hjust = 0.5) +
  geom_richtext(data = season_annotation,
                aes(x = 1,
                    y = y_position,
                    label = annotation),
                size = 2.75,
                family = "Chivo",color = "#333333",
                fill = NA,
                label.size = NA,
                hjust = 0.5) +
  geom_richtext(aes(x = 1,
                    y = 1.75,
                    label = "<br>SEASON"),
                size = 2,
                family = "Chivo",color = "#666666",
                fill = NA,
                label.size = NA,
                hjust = 0.5,
                vjust = 0.35) +
  geom_hline(yintercept = 2,
             color = "#333333",
             linewidth = 0.4) +
  geom_hline(yintercept = c(3,4,5),
             color = "#666666",
             linewidth = 0.2) +
  labs(caption = caption,
       y = NULL,
       x = NULL)

ggsave(profile_summary_plot,
        filename = paste0(player_first_name,
                          "_",
                          player_surname,
                          "_profile_summary_plot.png"),
        unit = "in",
        height = 3,
        width = 6.5)

# Adding logo

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.0125 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.99 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.05 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.045 * plot_width
    y_pos = plot_height - logo_height - 0.08 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


# Adding logo
profile_summary_plot_with_logo <- paste0(plot_name,"_with_logo") 
 
profile_summary_plot_with_logo <- add_logo(
  plot_path = paste0(player_first_name,
                      "_",
                      player_surname,
                      "_profile_summary_plot.png"), # url or local file for the plot
  logo_path = "/Users/michaelheery/Desktop/Establish the Run data viz/etr-datviz/ETR Horizontal Logo Cropped.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 2.5 #10 as default, but can change to manually make logo bigger
)



# save the image and write to working directory
magick::image_write(profile_summary_plot_with_logo,
                    paste0(plot_name,"_with_logo.png"))



# Rookie PPG comparatives ====

rookie_comparatives <- read_csv(paste0(
  "data/",
  player_full_name,
  " rookie ppg comparatives.csv")) %>% 
  clean_names() %>% 
  mutate(college = toupper(college)) %>% 
  mutate(axis_label = paste0(
    "<span style = 'font-size:11pt;font-family:Chivo;color:#333333;'><b>",
    player,
    "</b></span>",
    "<span style = 'font-size:7pt;font-family:Chivo;color:#999999'>",
    "<br>",
    college,
    "<span style = 'color:white;font-size:8pt;'>..</span>",
    years
  ))


# Caption 2 =====


caption_2 = paste0(
  "<span style = 'font-size:10pt;'>Chart created by Michael Heery",
  "<span style = 'color:white;font-size:8pt;'>...</span>",
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style = 'color:white;font-size:10pt;'>.</span>",
  " @heerymichael",
  "<span style = 'color:white;font-size:10pt;'>...</span>",
  "<span style='font-family:fb;'>&#xf08c;</span>",
  "<span style = 'color:white;font-size:8pt;'>.</span>",
  " michaelheery </span>")

comparatives_plot <- 
paste0(player_first_name,
       "_",
       player_surname,
       "_rookie_ppg_comp_plot")

comparatives_plot <- ggplot() +
  theme_etr() +
  theme(plot.subtitle = element_markdown(size = 12,
                                         lineheight = 1.1)) +
  scale_y_discrete(labels = NULL,
                   breaks = NULL) +
  scale_x_continuous(limits = c(-7, 20),
                     position = "top") +
  geom_col(data = rookie_comparatives,
           aes(x = ppg,
               y = reorder(axis_label, ppg)),
           fill = "#37af4a",
           width = 0.6) +
  geom_richtext(data = rookie_comparatives,
                aes(x = -0.2,
                    y = reorder(axis_label, ppg),
                    label = axis_label),
                lineheight = 0.1,
                hjust = 1,
                fill = NA,
                label.size = NA) +
  geom_vline(xintercept = 0,
             color = "#333333") +
  geom_vline(xintercept = 13,
             lty = "11",
             color = "#666666",
             linewidth = 0.8) +
  geom_shadowtext(data = data,
                  aes(x = 13,
                      y = 2,
                      label = "RB2\nproduction"),
                  color = "#666666",
                  family = "Chivo",
                  fontface = "bold",
                  bg.r = 0.7,
                  bg.color = "#ffffff",
                  show.legend = FALSE,
                  size = 4,
                  lineheight = 0.9) + 
  labs(title = paste0(player_full_name, " Comps"),
       subtitle = "Plot shows Points Per Game performance during rookie season for closest<br>comparable players in previous drafts",
       caption = caption_2,
       y = NULL,
       x = "ROOKIE SEASON POINTS PER GAME (PPR)")

ggsave(comparatives_plot,
       filename = paste0(comparatives_plot_name, ".png"),
       unit = "in",
       height = 7,
       width = 6.5)

# Adding logo

add_logo_2 <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.04 * plot_width
    y_pos = 0.035 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.99 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.05 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.045 * plot_width
    y_pos = plot_height - logo_height - 0.08 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


# Adding logo
comparatives_plot_with_logo <- add_logo_2(
  plot_path = paste0(comparatives_plot_name,".png"), # url or local file for the plot
  logo_path = "/Users/michaelheery/Desktop/Establish the Run data viz/etr-datviz/ETR_Full_Blk.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 6 #10 as default, but can change to manually make logo bigger
)



# save the image and write to working directory
magick::image_write(comparatives_plot_with_logo,
                    paste0(comparatives_plot_name,"_with_logo.png"))
