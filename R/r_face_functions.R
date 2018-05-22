#' Create affine transformation
#'
#' @param landmarks1
#' @param landmarks2
#'
#' @return
#' @importFrom vegan procrustes
#' @export
#'
#' @examples
#'
transform_points <- function(landmarks1, landmarks2) {
  if (ncol(landmarks1) == 3) {
    landmarks1 <- landmarks1[2:3]
    }
  if (ncol(landmarks2) == 3) {
    landmarks2 <- landmarks2[2:3]
  }

  tmp <- vegan::procrustes(landmarks1, landmarks2)
  r <- unlist(tmp['rotation'])
  t <- unlist(tmp['translation'])

  affine <- matrix(c(r[[1]],r[[2]],t[[1]],r[[3]],r[[4]],t[[2]]), nrow=3)
  return(affine)
}

#' Warp image with affine transformation
#'
#' @param image
#' @param m
#'
#' @return
#' @importFrom EBImage affine readImage
#' @export
#'
#' @examples
#'
warp_face <- function(image, m) {
  tmp <- EBImage::affine(
    x = EBImage::readImage(image),
    m = m,
    bg.col = "#777777"
  )
  return(tmp)
}

#' Create an average template from multiple templates
#'
#' @param files
#'
#' @return
#' @importfrom abind abind
#' @export
#'
#' @examples
#'
create_average_template <- function(files) {
  mat3d <- NULL
  for (file in 1:length(files)) {
    landmarks <- get_landmarks(files[[file]])[2:3]
    mat3d <- abind::abind(mat3d, landmarks, along = 3)
  }

  average_temp <- apply(mat3d, 1:2, mean)
  return(average_temp)
}

