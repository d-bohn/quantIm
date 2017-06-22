plot_landmarks <- function(landmarks=NULL, image, save=NULL){

  # #### FOR DEGUGGING NOT RUN
  # landmarks <- NULL
  # image <- 'data/S001.Post.Anger.jpg'
  # save <- NULL
  # ### END DEBUGGING NOT RUN

  if (is.null(landmarks)==TRUE){
    landmarks <- quantIm::face_landmarks(image=image)$face_landmarks[1:3]
  }

  img <- try(jpeg::readJPEG(image))
  if (class(img) == 'try-error') img <- png::readPNG(image)
  if (class(img) == 'try-error') message('Please supply either a .jpg or .png image.')

  g <- grid::rasterGrob(img, interpolate = TRUE)
  dim(img)

  library(ggplot2);library(grid)
  plot <- ggplot(data, aes(x = x, y = y, label=point)) +
    #annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point(show.legend=FALSE, na.rm = TRUE, aes(colour='red')) +
    geom_text() +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
      )
  plot
}
