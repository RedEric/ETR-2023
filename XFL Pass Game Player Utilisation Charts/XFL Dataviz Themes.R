
# Important notes ====

# Font sizes are set for a 2000px tall image
# If increasing the height of the image tou need to add
# scale = (old height in px/new height in px)
# to the ggsave function to adjust the text sizing and make the test appear
# proportionately the same height
# Increasing width does not affect


# XFL Dark Theme v1 ====

theme_xfl_dark_v1 <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#121212",
                        fill = "#121212"),
    plot.background = element_rect(color ="#121212",
                                   fill = "#121212"),
    panel.background = element_rect(color ="#121212",
                                    fill = "#121212"),
    strip.background = element_rect(color ="#121212",
                                    fill = "#121212"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#f5f5f5",
                                  size = 34),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#f5f5f5",
                                     lineheight = 1.1,
                                     size = 20,
                                 margin = margin(b = 0.5,
                                                 unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#f5f5f5",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              lineheight = 1.1,
                              size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#4E5254",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.text.y = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.title = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}


# Test Chart ====
library(tidyverse)
library(ggtext)
a <- c("a", "b", "c", "d", "e")
b <- c(100, 80, 60, 40, 30)
c <- c("#37af4a", "#f88b01", "#2dc6d2", "#7d42be", "#ffee00")

df <- tibble(a,b,c)

title <- paste0("Title narrative")
subtitle <- paste0("Subtitle narrative")
caption <- paste0("Insert the caption code")
  
  
ggplot() +
  geom_col(data = df,
           aes(x = a,
               y = b,
               fill = c),
           width = 0.65) +
  geom_hline(yintercept = 0,
             color = "#f5f5f5",
             size = 0.8) +
  scale_fill_identity() +
  theme_xfl_dark_v1() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)



# XFL Dark Theme v2 ====

theme_xfl_dark_v2 <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#1b2021",
                        fill = "#1b2021"),
    plot.background = element_rect(color ="#1b2021",
                                   fill = "#1b2021"),
    panel.background = element_rect(color ="#1b2021",
                                    fill = "#1b2021"),
    strip.background = element_rect(color ="#1b2021",
                                    fill = "#1b2021"),
    
    
    # Setting default text font and colour
    text = element_text(family = "Chivo",
                        colour = "#f5f5f5"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#f5f5f5",
                                  size = 34),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#f5f5f5",
                                     lineheight = 1.1,
                                     size = 20,
                                     margin = margin(b = 0.5,
                                                     unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#f5f5f5",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              lineheight = 1.1,
                              size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#4E5254",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.text.y = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.title = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}




ggplot() +
  geom_col(data = df,
           aes(x = a,
               y = b,
               fill = c),
           width = 0.65) +
  geom_hline(yintercept = 0,
             color = "#f5f5f5",
             size = 0.8) +
  scale_fill_identity() +
  theme_xfl_dark_v2() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)



# XFL Dark Theme v3 ====

theme_xfl_dark_v3 <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#2a3135",
                        fill = "#2a3135"),
    plot.background = element_rect(color ="#2a3135",
                                   fill = "#2a3135"),
    panel.background = element_rect(color ="#2a3135",
                                    fill = "#2a3135"),
    strip.background = element_rect(color ="#2a3135",
                                    fill = "#2a3135"),
    
    
    # Setting default text font and colour
    text = element_text(family = "Chivo",
                        colour = "#f5f5f5"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#f5f5f5",
                                  size = 34),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#f5f5f5",
                                     lineheight = 1.1,
                                     size = 20,
                                     margin = margin(b = 0.5,
                                                     unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#f5f5f5",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              lineheight = 1.1,
                              size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#4E5254",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.text.y = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.title = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}



library(ggfx)



ggplot() +
  with_outer_glow(
    geom_col(data = df %>% 
               filter(a == "a"),
           aes(x = a,
               y = b,
               fill = c),
           width = 0.65),
    sigma = 10,
    colour = "#37af4a") +
  with_outer_glow(
    geom_col(data = df %>% 
               filter(a == "b"),
             aes(x = a,
                 y = b,
                 fill = c),
             width = 0.65),
    sigma = 10,
    colour = "#f88b01") +
  with_outer_glow(
    geom_col(data = df %>% 
               filter(a == "c"),
             aes(x = a,
                 y = b,
                 fill = c),
             width = 0.65),
    sigma = 10,
    colour = "#2dc6d2") +
  with_outer_glow(
    geom_col(data = df %>% 
               filter(a == "d"),
             aes(x = a,
                 y = b,
                 fill = c),
             width = 0.65),
    sigma = 10,
    colour = "#7d42be") +
  with_outer_glow(
    geom_col(data = df %>% 
               filter(a == "e"),
             aes(x = a,
                 y = b,
                 fill = c),
             width = 0.65),
    sigma = 10,
    colour = "#ffee00") +
  geom_hline(yintercept = 0,
             color = '#f5f5f5',
             size = 0.8) +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 110)) +
  theme_xfl_dark_v3() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)


# XFL Dark Theme v4 ====

theme_xfl_dark_v4 <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#353d42",
                        fill = "#353d42"),
    plot.background = element_rect(color ="#353d42",
                                   fill = "#353d42"),
    panel.background = element_rect(color ="#353d42",
                                    fill = "#353d42"),
    strip.background = element_rect(color ="#353d42",
                                    fill = "#353d42"),
    
    
    # Setting default text font and colour
    text = element_text(family = "Chivo",
                        colour = "#f5f5f5"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#f5f5f5",
                                  size = 34),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#f5f5f5",
                                     lineheight = 1.1,
                                     size = 20,
                                     margin = margin(b = 0.5,
                                                     unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#f5f5f5",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              lineheight = 1.1,
                              size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#4E5254",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.text.y = element_text(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.title = element_text(family = "Chivo",
                              color = "#f5f5f5",
                              size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}




ggplot() +
  geom_col(data = df,
           aes(x = a,
               y = b,
               fill = c),
           width = 0.65) +
  geom_hline(yintercept = 0,
             color = "#f5f5f5",
             size = 0.8) +
  scale_fill_identity() +
  theme_xfl_dark_v4() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)



# Testing font sizes at different resolutions

test_plt <- ggplot() +
  geom_col(data = df,
           aes(x = a,
               y = b,
               fill = c),
           width = 0.65) +
  geom_hline(yintercept = 0,
             color = "#f5f5f5",
             size = 0.8) +
  scale_fill_identity() +
  theme_xfl_dark_v4() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)


ggsave(test_plt,
        filename = "basic_square.png",
        units = "px",
        dpi = "retina",
        height = 2000,
        width = 2000)

ggsave(test_plt,
       filename = "basic_wide.png",
       units = "px",
       dpi = "retina",
       height = 2000,
       width = 4000)

ggsave(test_plt,
       filename = "basic_tall.png",
       units = "px",
       dpi = "retina",
       height = 4000,
       width = 2000,
       scale = 0.5)



## ==== {gt} theme ====


gt_theme_etr_dark <- function(data, ...){
  data %>% 
    opt_all_caps(locations = c("column_labels"))  %>%
    opt_table_font(font = "Chivo") %>% 
    tab_options(
      heading.border.bottom.style = "none",
      table.background.color = "#2a3135",
      table.font.color.light = "#f5f5f5",
      table.border.top.style = "none",
      table.border.bottom.color = "#2a3135",
      table.border.left.color = "#2a3135",
      table.border.right.color = "#2a3135",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "#2a3135",
      column_labels.border.top.style = "none",
      column_labels.background.color = "#2a3135",
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "#4E5254",
      table_body.hlines.color = "#4E5254",
      data_row.padding = px(7),
      source_notes.font.size = 14,
      table.font.size = 16,
      heading.align = "left",
      heading.title.font.size = 24,
      heading.title.font.weight = "bolder",
      heading.subtitle.font.size = 16,
      heading.subtitle.font.weight = "lighter",
      
      ...
    ) 
}



library(gt)
library(gtExtras)

df %>% 
  gt() %>% 
  gt_theme_etr_dark()
