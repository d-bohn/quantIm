#' Crop an image to the face
#'
#' @author Daniel N. Albohn
#'
#' @param coords Face coordinates provided by \code{\link{face_coord}}
#' @param image Image name to be cropped
#' @param savename File name to save cropped image. Defaults to "cropped_\code{image}.png."
#' @param resize How much to increase the size of the bounding box by to capture more
#'  of the face in the image. Defaults to NULL.
#' @param scale Width and height to scale the image to. In the format of a character vector of widthxheight
#' (e.g., "512x512"). Defaults to NULL.
#'
#' @return Saves cropped image as a \code{.png} image file.
#' @export
#'
#' @examples
#' image <- system.file('data','faces.jpg',package = 'quantIm')
#' coords <- face_coord(image)
#' face_crop(coords,image)
#' ...
#' @importFrom magick image_read image_crop image_write
#' @importFrom tools file_path_sans_ext

face_crop <- function(bb, image, savename = NULL, resize = NULL, scale = NULL){

  # #### FOR DEGUGGING NOT RUN
  # image <- 'S032.Pre.Happy.jpg'
  # resize <- .5
  # scale <- "300x300"
  # bb <- face_landmarks(image=image)$bounding_box
  # ### END DEBUGGING NOT RUN

  img <- magick::image_read(image)

  if (is.null(resize) == FALSE){
    multy <- as.numeric(as.character(bb$y))*resize
    bb$y <- as.numeric(as.character(bb$y))-multy
    bb$height <- as.numeric(as.character(bb$height))+(multy+(multy/6))

    multx <- as.numeric(as.character(bb$x))*resize
    bb$x <- as.numeric(as.character(bb$x))-multx
    bb$width <- as.numeric(as.character(bb$width))+(multx+(multx/6))
  }

  for (i in 1:nrow(bb)){
    w_h_x_y <- paste0(bb[i,'width'],'x',bb[i,'height'],'+',bb[i,'x'],'+',bb[i,'y'])
    im <- magick::image_crop(img, w_h_x_y[i])
  }

  if (is.null(scale) == FALSE){
    im <- magick::image_scale(im, scale)
    #magick::image_browse(im_scale)
  }

  if (is.null(savename)==TRUE) {
    image_sans <- tools::file_path_sans_ext(image)
    savename <- paste0(image_sans,'_crop.png')
  }

  attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

  if (class(attempt1)=='try-error') {
    savename <- paste0(image_sans,'_crop.jpg')
    attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
  }

  if (class(attempt2)=='try-error') message('Please provide an appropriate image type to be written.')

  }
