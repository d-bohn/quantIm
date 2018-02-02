#' Rotate a matrix 90 degrees
#'
#' @param x A matrix to rotate.
#'
#' @return
#'
#' @examples
rotate <- function(x){
  t(apply(x, 2, rev))
}

#' Create a matrix of face points
#'
#' @param image
#' @param points
#'
#' @return
#' @export
#'
#' @examples
create_matrix <- function(image, points, ntimes){

  points2 <- points %>%
    dplyr::select(., x,y,point) %>%
    dplyr::mutate(., x = as.numeric(as.character(x)),
           y = as.numeric(as.character(y)),
           point = as.numeric(as.character(point)))

  img <- try(jpeg::readJPEG(image))
  if (class(img) == 'try-error') img <- png::readPNG(image)
  if (class(img) == 'try-error') message('Please supply either a .jpg or .png image.')

  dims <- dim(img)
  a <- array(0L, dims)
  b <- as.matrix(points2[1:2])
  a[b] <- 1

  a <- rotate(a)

  points_rotated <- RSAGA::grid.to.xyz(a)

  points_rotated <- points_rotated[points_rotated$z != 0,]

}
