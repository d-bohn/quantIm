#' Warp (affine transform) an image
#'
#' @param image1 Path to image to warp.
#' @param m Affine matrix.
#' @param convert Convert output to be R compatible?
#'
#' @return Warped image
#'
#' @importFrom reticulate import source_python
#' @importFrom EBImage readImage writeImage
#' @export
#'
#' @examples
#'

warp_image <- function(image, m, width, height, convert = FALSE){
  on.exit(
    unlink(c(temp1, temp2))
  )

  cv <- reticulate::import('cv2', convert = FALSE)

  temp1 <- tempfile(fileext = '.png')
  temp2 <- tempfile(fileext = '.png')

  if (class(image) %in% 'Image'){
    EBImage::writeImage(image, temp1)
    image1 <- temp1
  } else {
    image1 <- image
  }

  py_file <- system.file("python", "warp_image.py", package = "quantIm")

  if (convert == TRUE){
    reticulate::source_python(py_file)
    img <- warp_im(im = image1, M = m, width = width, height = height)
    return(img)
  }
  if (convert == FALSE){
    reticulate::source_python(py_file, convert = FALSE)
    img <- warp_im(im = image1, M = m, width = width, height = height)
    cv$imwrite(temp2, img)
    out_im <- EBImage::readImage(temp2)
    return(out_im)
  }


}

#' Match color between two images
#'
#' @param image1 Reference image
#' @param image2 Image to color correct
#'
#' @return
#'
#' @importFrom reticulate import source_python
#' @importFrom EBImage readImage writeImage
#' @export
#'
#' @examples
#'

color_correction <- function(image1, image2, blur = 0.6){
  on.exit(
    unlink(c(temp1, temp2))
  )

  cv <- reticulate::import('cv2', convert = FALSE)

  temp1 <- tempfile(fileext = '.png')
  temp2 <- tempfile(fileext = '.png')

  if (class(image2) %in% 'Image'){
    EBImage::writeImage(image2, temp1)
    image22 <- temp1
  } else {
    image22 <- image2
  }

  points <- get_landmarks(image22, convert = FALSE)

  py_file <- system.file("python", "color_correct.py", package = "quantIm")
  reticulate::source_python(py_file, convert = FALSE)

  img <- correct_colours(im1 = image1, im2 = image22, landmarks1 = points, blur = blur)

  cv$imwrite(temp2, img)

  out_im <- EBImage::readImage(temp2)

  return(out_im)
}

# NOT WORKING ----

#' Get a face mask
#'
#' @param image
#'
#' @return
#'
#' @importFrom EBImage readImage
#'
#' @examples
#'

get_face_mask <- function(image){
  on.exit(
    unlink(c(temp1, temp2))
  )

  cv <- reticulate::import('cv2', convert = FALSE)

  temp1 <- tempfile(fileext = '.png')
  temp2 <- tempfile(fileext = '.png')

  if (class(image) %in% 'Image'){
    EBImage::writeImage(image, temp1)
    image2 <- temp1
  } else {
    image2 <- image
  }

  lm <- get_landmarks(image2, convert = FALSE)
  im <- cv$imread(image2)

  py_file <- system.file("python", "get_face_mask.py", package = "quantIm")
  reticulate::source_python(py_file, convert = FALSE)

  mask <- get_face_mask(im = im, landmarks = lm)

  cv$imwrite(temp2, mask)

  out_im <- EBImage::readImage(temp2)
  return(out_im)
}

# END NOT WORKING ----
