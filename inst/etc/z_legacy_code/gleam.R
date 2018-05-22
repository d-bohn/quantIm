#' Gleam an Image
#'
#' @author Daniel N. Albohn
#'
#' @description
#' \code{gleam} will take an input image and "gleam" it. Gleam is one of
#' many algorithms to to a color image greyscale. The gleam algorithm has been
#' shown to be particularly effective in computer vision for detecting faces
#' and objects. See Details for more information.
#'
#' @details
#' This function uses \code{EBImage} to read in the image, extract the RGB colorspace,
#' and then compute the gray values from the gleam algorithm.
#'
#' The gleam algorithm can be formally stated as:
#'
#' \deqn{Gleam = 1/3(R' + G' + B')}
#'
#' Where R', G', and B' are the gamma-corrected color channels.
#'
#' @seealso
#' See \link{http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0029740}
#' for detailed information on the benefits of using certain grayscale algorithms over
#' others.
#'
#' @param image Full path to the input image to be gleamed.
#' @param tau Integer. The gamma-correction value. Each channel (RGB) in the \code{image}
#' will be raised to this value. Defaults to 0.4545455.
#' @param show Logical. Display the image after computing? Defaults to \code{FALSE}.
#'
#' @return Returns a a two-dimenstional array of gamma-corrected intensity
#' pixel values equal to the width and height of the original \code{image}.
#' @export
#'
#' @examples
#' image <- system.file('extdata', 'obama.png', package = 'quantIm')
#' gleam_image <- gleam(image, show=TRUE)
#' ...
#' @importFrom EBImage readImage imageData Image display
#'
gleam <- function(image, tau=(1/2.2), show=FALSE) {
  if (show == TRUE){
    on.exit({
      plot(0:1,0:1,type="n", ann=FALSE, axes=FALSE)
      rasterImage(im2,0,0,1,1)
    }
    )
  }

  im <- EBImage::readImage(image)
  if(length(dim(im)) == 3){
    red <- (EBImage::imageData(im)[,,1])^tau
    green <- (EBImage::imageData(im)[,,2])^tau
    blue <- (EBImage::imageData(im)[,,3])^tau
    im2 <- Image((1/3)*(red+green+blue))
  }
  else {
    im2 <- im
    message(image,' does not have 3 channels. Writting original file to disk.')
  }

  return(as.array(im2))
}
