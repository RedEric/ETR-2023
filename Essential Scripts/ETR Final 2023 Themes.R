# Dark background theme ==== 

theme_etr_dark_2023 <- function() {
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
   
     # Facet panel spanging
    panel.spacing.x = unit(2.25, "lines"),
    panel.spacing.y = unit(1, "lines"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#f5f5f5",
                                  size = 40,
                                  lineheight = 1.1),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#f5f5f5",
                                     lineheight = 1.3,
                                     size = 16,
                                     margin = margin(b = 0.5,
                                                     unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#f5f5f5",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_markdown(family = "Chivo",
                              color = "#f5f5f5",
                              lineheight = 1.1,
                              size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#4E5254",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_markdown(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.text.y = element_markdown(family = "Chivo",
                               color = "#f5f5f5",
                               size = 12),
    axis.title = element_markdown(family = "Chivo",
                              color = "#f5f5f5",
                              size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}

# Light background theme ====

theme_etr_light_2023 <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#fdfaf1",
                        fill = "#fdfaf1"),
    plot.background = element_rect(color ="#fdfaf1",
                                   fill = "#fdfaf1"),
    panel.background = element_rect(color ="#fdfaf1",
                                    fill = "#fdfaf1"),
    strip.background = element_rect(color ="#fdfaf1",
                                    fill = "#fdfaf1"),
    
    
    # Setting default text font and colour
    text = element_text(family = "Chivo",
                        colour = "#333333"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Facet panel spanging
    panel.spacing.x = unit(2.25, "lines"),
    panel.spacing.y = unit(1, "lines"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#333333",
                                  size = 40,
                                  lineheight = 1.1),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#333333",
                                     lineheight = 1.3,
                                     size = 16,
                                     margin = margin(b = 0.5,
                                                     unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#b9b9b9",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_markdown(family = "Chivo",
                                  color = "#333333",
                                  lineheight = 1.1,
                                  size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#d3d3d3",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_markdown(family = "Chivo",
                                   color = "#666666",
                                   size = 12),
    axis.text.y = element_markdown(family = "Chivo",
                                   color = "#666666",
                                   size = 12),
    axis.title = element_markdown(family = "Chivo",
                                  color = "#333333",
                                  size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}



# White background theme ====

theme_etr_white_2023 <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#ffffff",
                        fill = "#ffffff"),
    plot.background = element_rect(color ="#ffffff",
                                   fill = "#ffffff"),
    panel.background = element_rect(color ="#ffffff",
                                    fill = "#ffffff"),
    strip.background = element_rect(color ="#ffffff",
                                    fill = "#ffffff"),
    
    
    # Setting default text font and colour
    text = element_text(family = "Chivo",
                        colour = "#333333"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Facet panel spanging
    panel.spacing.x = unit(2.25, "lines"),
    panel.spacing.y = unit(1, "lines"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#333333",
                                  size = 40,
                                  lineheight = 1.1),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#333333",
                                     lineheight = 1.3,
                                     size = 16,
                                     margin = margin(b = 0.5,
                                                     unit = "cm")),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#b9b9b9",
                                    lineheight = 1.1,
                                    size = 17,
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_markdown(family = "Chivo",
                                  color = "#333333",
                                  lineheight = 1.1,
                                  size = 17),
    
    # gridlines
    panel.grid.major = element_line(color = "#d3d3d3",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_markdown(family = "Chivo",
                                   color = "#666666",
                                   size = 12),
    axis.text.y = element_markdown(family = "Chivo",
                                   color = "#666666",
                                   size = 12),
    axis.title = element_markdown(family = "Chivo",
                                  color = "#333333",
                                  size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}




# GT Theme dark background ====

gt_theme_etr_dark <- function(data, ...){
  data %>% 
    opt_all_caps(locations = c("column_labels"))  %>%
    opt_table_font(font = "Chivo") %>% 
    tab_options(
      table.background.color = "#1b2021",
      table_body.border.top.width = px(2),
      table_body.border.top.color = "#f5f5f5",
      table.border.top.width = px(2),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(1),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(0.8),
      column_labels.border.bottom.color = "#f5f5f5",
      table_body.border.bottom.color = "#f5f5f5",
      table_body.border.bottom.width = px(2),
      data_row.padding = px(3),
      source_notes.font.size = 14,
      table.font.size = 16,
      table.font.color = "#f5f5f5",
      heading.align = "left",
      heading.title.font.size = 24,
      heading.title.font.weight = "bolder",
      heading.subtitle.font.size = 16,
      heading.subtitle.font.weight = "lighter",
      
      ...
    ) 
}

# GT Theme light background ====

gt_theme_etr_light <- function(data, ...){
  data %>% 
    opt_all_caps(locations = c("column_labels"))  %>%
    opt_table_font(font = "Chivo") %>% 
    tab_options(
      table.background.color = "#fdfaf1",
      table_body.border.top.width = px(2),
      table_body.border.top.color = "#333333",
      table.border.top.width = px(2),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(1),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(0.8),
      column_labels.border.bottom.color = "#333333",
      table_body.border.bottom.color = "#333333",
      table_body.border.bottom.width = px(2),
      data_row.padding = px(3),
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

# GT Theme white background ====

gt_theme_etr_white <- function(data, ...){
  data %>% 
    opt_all_caps(locations = c("column_labels"))  %>%
    opt_table_font(font = "Chivo") %>% 
    tab_options(
      table_body.border.top.width = px(2),
      table_body.border.top.color = "#666666",
      column_labels.background.color = "white",
      table.border.top.width = px(2),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(1),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(0.8),
      column_labels.border.bottom.color = "#666666",
      table_body.border.bottom.color = "#666666",
      table_body.border.bottom.width = px(2),
      data_row.padding = px(3),
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



# Reactable theme white ====


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
  theme_etr_dark_2023() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)

ggplot() +
  geom_col(data = df,
           aes(x = a,
               y = b,
               fill = c),
           width = 0.65,
           color = "#333333") +
  geom_hline(yintercept = 0,
             color = "#333333",
             size = 0.8) +
  scale_fill_identity() +
  theme_etr_light_2023() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption)

library(gt)


gt(df) %>% 
  gt_theme_etr_light() %>% 
  tab_header(
    title = md("Test table"),
    subtitle = md("Testing how subtitle looks")
  )

gt(df) %>% 
  gt_theme_etr_white() %>% 
  tab_header(
    title = md("Test table"),
    subtitle = md("Testing how subtitle looks"))


gt(df) %>% 
  gt_theme_etr_dark() %>% 
  tab_header(
    title = md("Test table"),
    subtitle = md("Testing how subtitle looks"))


