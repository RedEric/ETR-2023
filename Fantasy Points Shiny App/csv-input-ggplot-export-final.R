# Libraries
library(shiny)
library(tidyverse)
library(janitor)
library(ggtext)
library(ggimage)
library(showtext)

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
  
  
  fileInput(inputId = "the_file", 
            label = "Upload your file here please", 
            accept = c("csv/text", ".csv", "text/comma-separated-values")),
  
  plotOutput(outputId = "plot1")
  
)


# Server ====
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    req(input$the_file) # Do not read the file until it is provided
    
    # Create a basic data frame from the data
    df <- read_csv(input$the_file$datapath) %>% clean_names()
    
    
    #####
    ##### to be automated!!!!
    #### Player headshot 
    player = "Josh Allen"
    headshot =   "https://static.www.nfl.com/image/private/t_player_profile_landscape_2x/f_auto/league/btfnqtymqsqgybnv4u6n"
    
    headshot_tibble = tibble(player, headshot)
    
    # Player label
    ##### REQUIRES AUTOMATION - PERHAPS MATCH COULD BE BUILT INTO THE CSV???
    player_label = paste0(
      "<span style='font-size:36px;color:#333333'>",
      player,
      "<br>",
      "<span style='font-size:14px;color:#999999'>",
      "WEEK 15: BUF vs MIA")
    
    # Create the basic density curve
    base_plot = ggplot() +
      geom_density(data = df,
                   aes(x = fantasy_pts))
    
    # Calcualting percentile limits
    fifteenth_centile = df %>% filter(percentile == 0.15) %>% select(fantasy_pts)
    eightyfifth_centile = df %>% filter(percentile == 0.85) %>% select(fantasy_pts)
    
    # Calculate the coordinates of the density curve
    calculating_curve_locations = ggplot_build(base_plot)$data[[1]]
    
    # Creating new plot
    ggplot() +
      
      # Basic theming elements
      theme_etr_white_2023() +
      theme(panel.grid.major.x = element_blank(),
            plot.subtitle = element_markdown(),
            plot.title = element_markdown(color = "#333333",
                                          size = 16,
                                          family = "Chivo",
                                          face = "bold",
                                          hjust = 0)) +
      
      # Axis scales
      scale_y_continuous(expand = c(0,0),
                         limits = c(0, 0.1),
                         breaks = NULL) +
      scale_x_continuous(limits = c(0,50),
                         expand = c(0,0)) +
      
      # Player annotation
      geom_image(data  = headshot_tibble,
                 aes(x = 2.5,
                     y = 0.09,
                     image = headshot),
                 size = 0.15,
                 asp = 2) +
      geom_richtext(aes(x = 6,
                        y = 0.088,
                        label = player_label),
                    family = "Chivo",
                    hjust = 0,
                    vjust = 0.5,
                    fill = NA, label.color = NA) +
      
      geom_hline(yintercept = 0.075,
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
                size = 1.9) +
      geom_line(data = subset(calculating_curve_locations, x > fifteenth_centile$fantasy_pts & x < eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#aaaaaa",
                size = 1.25) +
      
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
                size = 1.9) +
      geom_line(data = subset(calculating_curve_locations, x < fifteenth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#c53160",
                size = 1.25) +
      
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
                size = 1.9) +
      geom_line(data = subset(calculating_curve_locations, x > eightyfifth_centile$fantasy_pts),
                aes(x = x,
                    y = y),
                color = "#37af4a",
                size = 1.25) +
    
      # Axis line
      geom_hline(yintercept = 0,
                 color = "#333333") +
      
      # Finishing touches
      labs(title = "Projected Fantasy Points: NFL.com (0.5 PPR)",
           subtitle = "",
           caption = "INSERT CAPTION",
           x = "FANTASY POINTS",
           y = NULL)
    
  },
  height = 350,
  width = 350 * 1.618)
  
  
}


# The App ====
shinyApp(ui = ui, server = server)

