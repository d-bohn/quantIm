plot_landmarks <- function(landmarks=NULL, image, save=NULL){

  # #### FOR DEGUGGING NOT RUN
  # landmarks <- NULL
  # image <- 'data/S001.Post.Anger.jpg'
  # save <- NULL
  # ### END DEBUGGING NOT RUN

  if (is.null(landmarks)==TRUE){
    landmarks <- quantIm::face_landmarks(image=image, condaenv = 'quantIm')$face_landmarks[1:3]
  }

  img <- try(jpeg::readJPEG(image))
  if (class(img) == 'try-error') img <- png::readPNG(image, native = TRUE)
  if (class(img) == 'try-error') message('Please supply either a .jpg or .png image.')

  img <- readPNG(image, TRUE)
  g <- grid::rasterGrob(img, interpolate=TRUE)
  # g$width <- unit(1, "npc")
  # g$height <- unit(1, "npc")

  g <- grid::grid.raster(img@.Data)
  dim(img)
  data <- coords[c('x_new','y_new')]
  #data <- apply(data, 2, rev)
  data <- as.data.frame(data) %>% mutate(., point=1:nrow(.),
                                         x=as.numeric(x),
                                         y=as.numeric(y))


  library(ggplot2);library(grid)
  plot <- ggplot(data, aes(x = x_new, y = y_new, label=point)) +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point(show.legend=FALSE, na.rm = TRUE, aes(colour='red')) +
    geom_text() +
    theme_bw() +
    ylim(512,0) +
    xlim(0,512) +
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

  library(raster);library(dplyr)
  img <- EBImage::readImage(coords$image[1]) %>%
    EBImage::rotate(., 180) %>%
    EBImage::flop(.)
  head(img)
  dim(img)
  head(points[1:2])

  res = dim(img)[2:1]
  plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
       xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(img,1,1,res[1],res[2])
  #points(,0,pch=20,col='red')
  points(coords[1:2], pch = 20, col='red')
  points(0,0,col='red',lwd=.5)

}
