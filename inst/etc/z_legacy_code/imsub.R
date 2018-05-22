#' Subtract two images
#'
#' @param image1
#' @param image2
#' @param blur
#' @param save_image
#' @param write_data
#'
#' @return
#' @export
#' @importFrom EBImage writeImage readImage
#' @importFrom tools file_path_sans_ext
#' @importFrom readr parse_number
#' @importFrom spatstat blur as.im
#'
#' @examples

im_sub <- function(image1, image2, save_image = TRUE,
                   write_data = TRUE, blur) {

    # Vars
    path <- dirname(image1)
    im1_name <- basename(tools::file_path_sans_ext(image1))
    im2_name <- basename(tools::file_path_sans_ext(image2))
    save_name <- paste0(im1_name, '-', im2_name)

    im1 <- EBImage::readImage(image1)
    im2 <- EBImage::readImage(image2)

    # Check gray
    EBImage::colorMode(im1) <- 0
    EBImage::colorMode(im2) <- 0

    if (hasArg(blur)) {
      im1_blur <- spatstat::blur(spatstat::as.im(im1@.Data), blur)
      im1 <- EBImage::as.Image(im1_blur$v)

      im2_blur <- spatstat::blur(spatstat::as.im(im2@.Data), blur)
      im2 <- EBImage::as.Image(im2_blur$v)
    }

    out <- im1 - im2

    if (!(dir.exists((file.path(path, 'subtracted'))))) {
      dir.create(file.path(path, 'subtracted'))
    }

    if (write_data == TRUE) {
      df <- as.data.frame(out)
      data.table::fwrite(df, file = file.path(path, 'subtracted',
                                              paste0(save_name, '_subtracted.csv')))
      if (save_image == TRUE) {
        EBImage::writeImage(out, file.path(path, 'subtracted',
                                           paste0(save_name, '.png')))
      }
      }
      else {
        if (save_image == TRUE) {
          EBImage::writeImage(out, file.path(path, 'subtracted',
                                             paste0(save_name, '.png')))
      return(as.data.frame(out))
        }
        }
    }

#' Subtract batches of images
#'
#' @param files1
#' @param files2
#' @param save_image
#' @param write_data
#' @param blur
#'
#' @return
#'
#' @importFrom purrr map2 reduce
#' @importFrom abind abind
#' @export
#'
#' @examples
#'

batch_im_sub <- function(files1, files2, save_image,
                         write_data, ...) {

  list <- purrr::map2(files1, files2, quantIm:::im_sub,
              save_image = save_image,
              write_data = write_data, ...)

  if (!is.null(length(dim(list)))) {
    df <- purrr::reduce(list, abind::abind, along = 3)
    return(df)
  } else message ('Complete.')
}




