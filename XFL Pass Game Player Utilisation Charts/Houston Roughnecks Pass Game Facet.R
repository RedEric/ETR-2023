# Define team and week ====
chart_team <- "HOU"

# Chart essentials ====

title <- "Houston Roughnecks"
subtitle <- paste0(
  "<span style = 'font-family:Chivo; color:",
  team_color_1,
  "'><b>Green</b> <span style = color:#666666>bar shows % of team dropbacks where player ran a route<br>",
  "<span style = 'font-family:Chivo; color:",
  team_color_2,
  "'><b>Orange</b> <span style = color:#666666>bar shows the % of team dropbacks where the player was targeted"
)

caption <- paste0(
  "<span style='font-family:Chivo;color:#808080;;font-size:11pt;'>Chart created by Michael Heery</span>",
  "<span style = 'color:white;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#808080;;font-size:11pt;'>&#xf099;</span>",
  "<span style='font-family:Chivo;color:#808080;;font-size:11pt;'> @heerymichael </span>",
  "<span style = 'color:white;font-size:7pt;'>...</span>",
  "<span style='font-family:fb;color:#808080;font-size:11pt;'  >&#xf08c;</span>",
  "<span style='font-family:Chivo;color:#808080;font-size:11pt;'> michaelheery </span>"
)

# Processing data ====

hou_data <- all_teams_wr_te_data %>% 
  # Filtering for team
  filter(team == chart_team) %>% 
  # Including only the last 4 weeks
  mutate(week_number = parse_number(week)) %>% 
  filter(week_number > (week_number - 4)) %>% 
  select(player, position,
         week,
         week_number,
         route_percent,
         target_percent)

# Removing players with no involvement over the entire period
not_involved <- hou_data %>% 
  group_by(player) %>% 
  summarise(total_routes = sum(route_percent)) %>% 
  filter(total_routes == 0)

hou_data <- hou_data %>% 
  filter(player %ni% not_involved$player)

# processing data ====
hou_data <- hou_data %>%  
  gather(key = measure,
         value = percent,
         route_percent:target_percent) %>% 
  mutate(percent = round(percent, 2)) %>% 
  mutate(label_position = case_when(
    measure == "route_percent" ~ week_number - 0.2,
    measure == "target_percent" ~ week_number + 0.2
  )) %>% 
  mutate(facet_title = paste0(
    "<b><span style = 'font-family:Chivo;color:#333333;font-size:16pt'><br>",
    player,
    "</b><span style = 'font-family: Chivo; color:#808080; font-size:12pt'><br>",
    position
  ))

# facet ordering ====

hou_facet_ordering <- all_teams_wr_te_data %>% 
  filter(team == chart_team) %>% 
  select(player,position,
         week,
         team_dropbacks,
         routes,
         targets) %>% 
  mutate(facet_title = paste0(
    "<b><span style = 'font-family:Chivo;color:#333333;font-size:16pt'><br>",
    player,
    "</b><span style = 'font-family: Chivo; color:#808080; font-size:12pt'><br>",
    position
  )) %>% 
  group_by(facet_title) %>% 
  summarise(total_targets = sum(targets)) %>% 
  arrange(desc(total_targets))

hou_data <- hou_data %>% 
  mutate(facet_title = factor(facet_title,
                              levels = hou_facet_ordering$facet_title))

# Plot ====

test_plot <- ggplot() +
  theme_etr() +
  theme(panel.grid.major.x = element_blank(),
        strip.text = element_markdown(hjust = 0),
        panel.spacing.x = unit(2.25, "lines"),
        panel.spacing.y = unit(1, "lines"),
        plot.title = element_markdown(lineheight = 1.1,
                                      size = 40),
        plot.subtitle = element_markdown(lineheight = 1.3,
                                         size = 16),
        plot.caption = element_markdown()) +
  geom_col(data = hou_data,
           aes(x = week_number,
               y = percent,
               fill = measure),
           color = "#ffffff",
           size = 0.8,
           width = 0.8,
           position = position_dodge(width = 0.6),
           show.legend = FALSE) +
  geom_shadowtext(data = hou_data,
                  aes(x = label_position,
                      y = percent + 0.09,
                      label = percent(percent, 1),
                      color = measure),
                  hjust = 0.5,
                  family = "Chivo",
                  fontface = "bold",
                  bg.color = "#ffffff",
                  bg.r= 0.35,
                  show.legend = FALSE,
                  size = 4.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ facet_title,
             ncol = 3,
             scales = "free_x") +
  scale_fill_manual(values = c(team_color_1,
                               team_color_2)) +
  scale_color_manual(values = c(team_color_1,
                                team_color_2)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1.15),
                     breaks = c(0,
                                .25,
                                .5,
                                .75,
                                1),
                     labels = c("",
                                "25%",
                                "50%",
                                "75%",
                                "100%")) +
  scale_x_continuous(limits = c(0.5, 2.5),
                     breaks = c(1,2),
                     expand = c(0.1,0.1)) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       x = NULL,
       y = NULL)

ggsave(test_plot,
       filename = "Plots without Logos/hou utilisation facet.png",
       units = "in",
       height = 9,
       width = 9)



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
    x_pos = plot_width - logo_width - 0.03 * plot_width
    y_pos = 0.02 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.99 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.05 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.045 * plot_width
    y_pos = plot_height - logo_height - 0.06 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


# Adding logo


houston_pass_game_facet_with_logo <- add_logo(
  plot_path = "Plots without Logos/hou utilisation facet.png", # url or local file for the plot
  logo_path = "/Users/michaelheery/Desktop/Establish the Run data viz/etr-datviz/ETR_Full_Blk.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 7.5 #10 as default, but can change to manually make logo bigger
)



# save the image and write to working directory
magick::image_write(houston_pass_game_facet_with_logo,
                    "Plots with Logos/houston_pass_game_facet_with_logo.png")

