#' Crop image to face
#'
#' @param coords Face coordinates provided by \code{\link{face_coord}}
#' @param image Image name to be cropped
#' @param savename File name to save cropped image. Defaults to "cropped_\code{image}.png."
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

# #### FOR DEGUGGING NOT RUN
# image <- 'data/faces.jpg'
# savename <- 'cropped_face'
# ### END DEBUGGING NOT RUN
face_crop <- function(coords, image, savename=NULL){

  img <- magick::image_read(image)

  w_h_x_y <- paste0(coords[i,'width'],'x',coords[i,'height'],'+',coords[i,'x'],'+',coords[i,'y'])
  im <- magick::image_crop(img, w_h_x_y[i])

  if (is.null(savename)==TRUE) {
    image_sans <- tools::file_path_sans_ext(image)
    savename <- paste0('cropped_',image_sans,'.png')
  }

  magick::image_write(im, path = savename, format = "png")
}
