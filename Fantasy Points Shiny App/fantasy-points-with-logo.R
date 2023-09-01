# Libraries
library(shiny)
library(tidyverse)
library(janitor)
library(ggtext)
library(ggimage)
library(showtext)
library(shadowtext)
library(rsconnect)
library(magick)
font_add_google('Chivo', 'Chivo')
showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)

# Themes

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
                       unit = "cm"),
    
    # Facet panel spanging
    panel.spacing.x = unit(2.25, "lines"),
    panel.spacing.y = unit(1, "lines"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Chivo",
                                  face = "bold",
                                  color = "#333333",
                                  size = 30,
                                  lineheight = 1.6),
    plot.subtitle = element_markdown(family = "Chivo",
                                     color = "#333333",
                                     lineheight = 1.3,
                                     size = 16),
    plot.caption = element_markdown(family = "Chivo",
                                    color = "#b9b9b9",
                                    lineheight = 1.1,
                                    size = 17),
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
                                   color = "#bbbbbb",
                                   size = 12),
    axis.text.y = element_markdown(family = "Chivo",
                                   color = "#bbbbbb",
                                   size = 12),
    axis.title = element_markdown(family = "Chivo",
                                  color = "#bbbbbb",
                                  size = 12),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}

# UI ====

ui <- fluidPage(
  fluidRow(
    column(5,
           fileInput(inputId = "the_file",
                     label = "Upload your file here please",
                     accept = c("csv/text", ".csv", "text/comma-separated-values")),
    column(7,
            plotOutput(outputId = "plot1")))))


# Server ====
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    req(input$the_file) # Do not read the file until it is provided
    
    # Create a basic data frame from the data
    df <- read_csv(input$the_file$datapath) %>% clean_names()
    
    # Player label
    player_name = df %>% slice_max(player, n = 1)
    player = player_name$player
    
    # Subtitle annotation
    player_label = paste0(
      "<br>",
      "<span style='font-size:40px;color:#333333'><b>",
      player,
      "</span><br><br></b>",
      "<span style='font-size:12px;color:#999999'>",
      "WEEK 15: BUF vs MIA")
    
    # Create the basic density curve
    base_plot = ggplot() +
      geom_density(data = df,
                   aes(x = fantasy_pts))
    
    # Calculate the coordinates of the density curve
    calculating_curve_locations = ggplot_build(base_plot)$data[[1]]
    
    # Calculate y-axis limit
    y_axis_data_max = max(calculating_curve_locations$y)
    # y_axis_max = round(y_axis_data_max + 0.02, 2)
    y_axis_max = 0.07
    
    # Calcualting percentile limits
    fifteenth_centile = df %>% filter(percentile == 0.15) %>% select(fantasy_pts)
    eightyfifth_centile = df %>% filter(percentile == 0.85) %>% select(fantasy_pts)
    
    # Calculating floor and ceiling direct labels axis position
    floor_label_position = df %>% filter(percentile == 0.15 | percentile == 0.01) %>% select(fantasy_pts) %>% 
      summarise(minimum = min(fantasy_pts), maximum = max(fantasy_pts)) %>% mutate(position = minimum + ((maximum - minimum)/2)) 
  
    ceiling_label_position = df %>% filter(percentile == 0.85 | percentile == 0.99) %>% select(fantasy_pts) %>% 
      summarise(minimum = min(fantasy_pts), maximum = max(fantasy_pts)) %>% mutate(position = minimum + ((maximum - minimum)/2)) 
      
    # Creating new plot
    ggplot() +
      
      # Basic theming elements
      theme_etr_white_2023() +
      theme(panel.grid.major.x = element_blank(),
            plot.subtitle = element_markdown(size = 4),
            plot.title = element_markdown(color = "#333333",
                                          size = 12,
                                          family = "Chivo",
                                          face = "bold",
                                          hjust = 0)) +
      
      # Axis scales
      scale_y_continuous(expand = c(0,0),
                         limits = c(0, y_axis_max),
                         breaks = NULL) +
      scale_x_continuous(limits = c(0,50),
                         expand = c(0,0)) +
      
      
      
      
      geom_hline(yintercept = y_axis_max,
                 size = 0.2,
                 color = "#333333") +
      
      # Mid-zone area plot and lines
      geom_area(data = subset(calculating_curve_locations, x > fifteenth_centile$fantasy_pts & x < eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                fill = "#aaaaaa",
                alpha = 0.2) +
      geom_line(data = subset(calculating_curve_locations, x > fifteenth_centile$fantasy_pts & x < eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#ffffff",
                size = 6) +
      geom_line(data = subset(calculating_curve_locations, x > fifteenth_centile$fantasy_pts & x < eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#aaaaaa",
                size = 4) +
      
      # Floor area and lines
      
      geom_area(data = subset(calculating_curve_locations, x < fifteenth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                fill = "#c53160",
                alpha = 0.2) +
      
      geom_line(data = subset(calculating_curve_locations, x < fifteenth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#ffffff",
                size = 6) +
      geom_line(data = subset(calculating_curve_locations, x < fifteenth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#c53160",
                size = 4) +
      
      # Ceiling area and lines
      geom_area(data = subset(calculating_curve_locations, x > eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                fill = "#37af4a",
                alpha = 0.2) +
      
      geom_line(data = subset(calculating_curve_locations, x > eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#ffffff",
                size = 6) +
      geom_line(data = subset(calculating_curve_locations, x > eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#37af4a",
                size = 4) +
      
      # Ceiling and Floor area direct annotations
      geom_shadowtext(data = floor_label_position,
                      aes(x = maximum - 1,
                          y = 0.005,
                          label = "FLOOR"),
                      color = "#c53160",
                      bg.color = "#ffffff",
                      bg.r = 0.1,
                      family = "Chivo",
                      size = 5,
                      hjust = 1) +
      
      geom_shadowtext(data = ceiling_label_position,
                      aes(x = minimum + 1,
                          y = 0.005,
                          label = "CEILING"),
                      color = "#37af4a",
                      bg.color = "#ffffff",
                      bg.r = 0.1,
                      family = "Chivo",
                      size = 5,
                      hjust = 0) +
      
      # Floor annotation
      geom_richtext(data = floor_label_position,
                    aes(x = 2,
                        y = 0.06,
                        label = "<span style='font-size:30px;color:#c53610;'><b>15%"),
                    family = "Chivo",
                    hjust = 0,
                    vjust = 1,
                    fill = NA, label.color = NA) +
      
      geom_richtext(data = floor_label_position,
                    aes(x = 2,
                        y = 0.0525,
                        label = paste0(
                          "<span style='font-size:20px;color:#333333;'>chance of<br>scoring<br><b><span style='color:#c53610;'>less than<br>",
                          round(floor_label_position$maximum,1),
                          " points")),
                    family = "Chivo",
                    hjust = 0,
                    vjust = 1,
                    fill = NA, label.color = NA) +
      
      # Ceiling annotation
      
      geom_richtext(data = ceiling_label_position,
                    aes(x = minimum + 2,
                        y = 0.06,
                        label = "<span style='font-size:30px;color:#37af4a;'><b>15%"),
                    family = "Chivo",
                    hjust = 0,
                    vjust = 1,
                    fill = NA, label.color = NA) +
      
      geom_richtext(data = ceiling_label_position,
                    aes(x = minimum + 2,
                        y = 0.0525,
                        label = paste0(
                          "<span style='font-size:20px;color:#333333;'>chance of<br>scoring<br><b><span style='color:#37af4a;'>more than<br>",
                          round(ceiling_label_position$minimum,1),
                          " points")),
                    family = "Chivo",
                    hjust = 0,
                    vjust = 1,
                    fill = NA, label.color = NA) +
      

    
      # Axis line
      geom_hline(yintercept = 0,
                 color = "#333333") +
      
      # Finishing touches
      labs(title = "Projected Fantasy Points: NFL.com (0.5 PPR)",
           subtitle = player_label,
           caption = "Data: Establish the Run",
           x = NULL,
           y = NULL,
           tag = "<img src = 'ETR_Full_Blk.png' height = '110'>") +
      theme(plot.tag.position = c(1,1),
            plot.tag = element_markdown(hjust = 1,
                                        vjust = 1))
    
  },
  height = 680,
  width = 780 * 1.618)
  
  
}


# The App ====
shinyApp(ui = ui, server = server)

