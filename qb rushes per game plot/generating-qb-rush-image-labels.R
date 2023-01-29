library(ggimage)
library(geomtextpath)
library(glue)

# Background scipts
source('Circle Cropping Headshots Background Script.R')


# Importing data ====

qb_rush_data <- read_csv("data/qb_rushes_data_with_centered_and_cropped_images.csv")


# # Test function ====
# 
# hjust = 0.5
# plot_image_label(image= qb_rush_data$headshot_cropped[1],
#                  label= qb_rush_data$player[1],
#                  font_color="#333333",
#                  top_bottom = "bottom", 
#                  hjust=hjust)



#create new file path for images
qb_rush_data = qb_rush_data%>% mutate(new_image_path = paste0(tolower(str_replace_all(player," ","_")),".png"))

#list of hjust values by .11
list_hjust = seq(0,1, by=0.1)



#create loop to generate and save all images with labels
for(i in 1:nrow(qb_rush_data)){
  pos = sample(c("top","bottom"),1)
  hjust = sample(list_hjust,1)
  path = qb_rush_data$new_image_path[i]
  plot = plot_image_label(image=qb_rush_data$headshot_cropped[i],
                          label= qb_rush_data$player[i],
                          font_color="#333333",
                          top_bottom = pos, 
                          hjust=hjust)
  ggsave(filename=glue("images/circle-labels/{path}"), plot=plot, height=3.95, width=4.5)
}


#create loop to save all image files without the text
for(i in 1:nrow(qb_rush_data)){
  img = border(image_read(qb_rush_data$headshot_cropped[i]))
  path = qb_rush_data$new_image_path[i]
  image_write(img, path = glue("images/circles/{path}"), format = "png")
}
