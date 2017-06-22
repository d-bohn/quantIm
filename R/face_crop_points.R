#' Crop image around set of points
#'
#' @aliases Daniel N. Albohn
#'
#' @param coords
#' @param image
#' @param points
#' @param savename
#' @param scale
#' @param wh
#'
#' @return
#' @export
#'
#' @examples
face_crop_points <- function(coords, image, points = c(30,28), savename = NULL, scale = 1, wh = NULL){
  # #### FOR DEGUGGING NOT RUN
  # setwd('/Volumes/S E/R_Drive/Stim/Neutral_Pre.Post/Pre.Post.Re.2/')
  # image <- "S001.Post.Anger.jpg"
  # coords <- face_landmarks(image=image)$face_landmarks
  # scale = 1
  # wh = 300
  # points = c(30,28)
  # savename = NULL
  # ### END DEBUGGING NOT RUN

  # Check if it we have numeric values
  coords$x <- as.numeric(as.character(coords$x))
  coords$y <- as.numeric(as.character(coords$y))

  # Get center coordinates
  centerx <- round((coords$x[points[1]]))*scale
  centery <- round((coords$y[points[2]]))*scale

  # Load and rescale
  percent = scale*100
  img <- magick::image_read(image)
  img <- magick:::image_scale(img, paste0(percent,'%'))

  # Now crop, fool, crop!
  if (is.null(wh)==TRUE) {
    wh <- as.numeric(dim(EBImage::readImage(image))[1])/2
    message('The image cropping will default to half of it`s width')
  }

  wh2 <- paste(wh,wh,sep = 'x')
  w_h_x_y <- paste0(wh2,'+',(centerx-(as.numeric(wh)/2)),'+',(centery-(as.numeric(wh)/2)))
  im <- magick::image_crop(img, w_h_x_y)
  # magick::image_browse(im)

  if (is.null(savename)==TRUE) {
    image_sans <- tools::file_path_sans_ext(image)
    savename <- paste0(image_sans,'_crop_scale.png')
  }

  attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

  if (class(attempt1)=='try-error') {
    savename <- paste0(image_sans,'_crop_scale.jpg')
    attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
  }

  if (class(attempt2)=='try-error') message('Please provide an appropriate image type to be written.')

  }

