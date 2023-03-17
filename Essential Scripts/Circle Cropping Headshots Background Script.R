
library(snakecase)
library(cropcircles)
library(magick)

resize_fit <- function(image, size = 600) {
  info <- image_info(image)
  size <- min(size, info$width, info$height)
  image_resize(
    image,
    geometry_size_pixels(
      height = if (info$width >= info$height) size,
      width = if (info$height > info$width) size
    )
  )
}

find_face_center <- function(image) {
  detections <- image.libfacedetection::image_detect_faces(image)$detections
  best_face <- which(detections$confidence == max(detections$confidence))
  dims <- as.list(detections[best_face[[1]], ])
  list(
    x = dims$x + dims$width / 2,
    y = dims$y + dims$height / 2
  )
}

crop_offset <- function(point, range, width) {
  # 4. Catch the edge case first
  if (width >= range) return(0)
  
  if ((point - width / 2) < 0) {
    # 1. must start at left edge
    return(0)
  }
  if ((point + width / 2) > range) {
    # 2. must start at right edge
    return(range - width)
  }
  # 3. enough space on both sides to center width in range
  point - width / 2
}

resize_crop_to_face <- function(image, size = 600) {
  image_original <- image_read(image)
  image <- resize_fit(image_original, size)
  info <- image_info(image)
  
  # size may have changed after refit
  size <- min(info$height, info$width)
  
  is_image_square <- info$width == info$height
  if (is_image_square) {
    return(image)
  }
  
  face <- find_face_center(image)
  
  image_crop(
    image,
    geometry = geometry_area(
      width = size,
      height = size,
      x_off = crop_offset(face$x, info$width, size),
      y_off = crop_offset(face$y, info$height, size)
    )
  )
  
}

crop_and_save_image <- function(player_name) {
  
  filtered_data <- sample_of_players %>% 
    filter(full_name == player_name)
  
  file_name <- to_snake_case(filtered_data$full_name)
  
  cropped_image <- resize_crop_to_face(filtered_data$headshot_url)
  
  image_write(cropped_image,
              path = str_glue("cropped images/{file_name}.png"))
  
}




#custom function to apply border to circle image with magick
border <- function(im) {
  ii <- magick::image_info(im)
  ii_min <- min(ii$width, ii$height)
  
  img <- image_blank(width = ii_min, height = ii_min, color = "none")
  drawing <- image_draw(img)
  symbols(ii_min/2, ii_min/2, circles = ii_min/2, bg = 'white', inches = FALSE, add = TRUE)
  dev.off()
  
  x = image_composite(image_scale(drawing, "x430"), image_scale(im, "x400"), offset = "+15+15")
  
  x
}
