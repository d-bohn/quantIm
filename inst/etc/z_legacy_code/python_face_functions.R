#' Get facial landmarks
#'
#' @param image Path to image
#' @param convert Convert output to be R compatible?
#'
#' @return Dataframe with 68 face points.
#'
#' @importFrom reticulate import source_python
#' @export
#'
#' @examples
#' img <- system.file("extdata", "obama.png", package = "quantIm")
#' get_landmarks(img)
#'
get_landmarks <- function(image, convert = TRUE){

  cv <- reticulate::import('cv2', convert = FALSE)

  img <- cv$imread(image)

  py_file <- system.file("python", "get_landmarks.py", package = "quantIm")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "quantIm")

  if (convert == TRUE){
    reticulate::source_python(py_file)
    x <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)
    landmarks <- data.frame(point = seq(1:68), x = x[,1], y = x[,2])
    return(landmarks)
  }
  if (convert == FALSE){
    reticulate::source_python(py_file, convert = FALSE)
    x <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)
    return(x)
  }

}

#' Affine transform face image points
#'
#' @param image1 Image to reference in transformation.
#' @param image2 Image to be transformed.
#' @param convert Convert output to be R compatible?
#'
#' @return Affine transformation matrix.
#'
#' @importFrom reticulate import source_python
#' @export
#'
#' @examples
#'

transform_points_py <- function(image1, image2, convert = TRUE){

  # Get points ----
  pts1 <- get_landmarks(image1, convert = FALSE)
  pts2 <- get_landmarks(image2, convert = FALSE)

  # Get transformation matrix ----
  if (convert == TRUE){
    py_file <- system.file("python", "transform_points.py", package = "quantIm")
    reticulate::source_python(py_file, convert = TRUE)

    matrix <- transformation_points(points1 = pts1, points2 = pts2)
    return(matrix)
  }
  if (convert == FALSE){
    py_file <- system.file("python", "transform_points.py", package = "quantIm")
    reticulate::source_python(py_file, convert = FALSE)

    matrix <- transformation_points(points1 = pts1, points2 = pts2)
    return(matrix)
  }
}

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
#' @export
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

#' Swap faces between two images
#'
#' @param reference Path to image with face to host swapped face from \code{target} image.
#' @param target Path to image with face to swap to \code{reference} image.
#' @param convert Logical. Convert returned image to be \code{R} compatible?
#' Defaults to \code{FALSE} and returns numpy matrix.
#'
#' @return Either a numpy matrix or EBImage object.
#'
#' @importFrom reticulate import source_python
#' @importFrom EBImage readImage
#' @export
#'
#' @examples
#'

face_swap <- function(reference, target, convert = FALSE){

  py_file <- system.file("python", "face_warp_full.py", package = "quantIm")
  reticulate::source_python(py_file, convert = FALSE)

  img <- out_image(reference = reference, target = target)

  if (convert == TRUE){
    on.exit(
      unlink(c(temp))
    )
    cv <- reticulate::import('cv2', convert = FALSE)

    temp <- tempfile(fileext = '.png')
    cv$imwrite(temp, img)

    out_im <- EBImage::readImage(temp)
    return(out_im)

  } else {
    return(img)
  }

}
