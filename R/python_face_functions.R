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
