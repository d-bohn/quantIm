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
  if(length(dim(im)) == 3) {
    red <- (EBImage::imageData(im)[,,1])^tau
    green <- (EBImage::imageData(im)[,,2])^tau
    blue <- (EBImage::imageData(im)[,,3])^tau
    im2 <- Image((1/3)*(red+green+blue))
  } else if (length(dim(im)) == 2) {
    im2 <- (EBImage::imageData(im))^tau
  }
  else {
    stop(image,' not in the proper format.')
  }

  return(as.array(im2))
}

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
#' @param ... Additional arguments to be passed to \code{im_sub}.
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

#' Compute t-test on multiple images
#'
#' @param path File path to folder with subtracted image files and values.
#' @param pattern1 Pattern that the subtracted files were saved in. Defaults to '.csv'
#' @param name Name that the t-values file should be saved as.
#' @param write Should the file be saved (\code{TRUE}), or returned as a vector (\code{FALSE}).
#' Defaults to \code{TRUE}.
#'
#' @return
#'
#'
#' @export
#' @importFrom abind abind
#' @importFrom purrr map reduce
#' @importFrom data.table fwrite fread
#'
#' @examples

im_t_test <- function(path, pattern1 = '*.csv', name, write = TRUE){

  if(!hasArg(path)){
    stop('Please supply folder path...')
  }

  files <- list.files(path = path, pattern = pattern1, full.names = TRUE)

  list <- purrr::map(files, data.table::fread)
  zz <- purrr::reduce(list, abind::abind, along = 3)

  tt <- apply(zz, 1:2, function (u) t.test(u, mu = 0)$statistic)

  data <- as.data.frame(tt)

  if (write == TRUE) {
    data.table::fwrite(data, file = file.path(path,paste0(name,'_average_t_values.csv')), sep = ",",
                       append = FALSE, col.names = TRUE)
  } else {
    return(data)
  }

}

#' Compute z-test on multiple images
#'
#' @param path File path to folder with subtracted image files and values.
#' @param pattern1 Pattern that the subtracted files were saved in. Defaults to '.csv'
#' @param name Name that the t-values file should be saved as.
#' @param write Should the file be saved (\code{TRUE}), or returned as a vector (\code{FALSE}).
#' Defaults to \code{TRUE}.
#'
#' @return
#'
#' @export
#' @importFrom abind abind
#' @importFrom BSDA z.test
#' @importFrom purrr map reduce
#' @importFrom data.table fwrite fread
#'
#' @examples

im_z_test <- function(path, data, pattern1 = '*.csv', name, write = TRUE){

  if (!hasArg(path) && length(dim(data)) != 3) {
    stop('Please supply folder path,')
    stop('or 3D dataframe.')
  }

  if (hasArg(path)) {
    files <- list.files(path = path, pattern = pattern1, full.names = TRUE)

    list <- purrr::map(files, data.table::fread)
    zz <- purrr::reduce(list, abind::abind, along = 3)
  }

  if (hasArg(data) && length(dim(data)) == 3) {
    zz <- data
  }

  sd <- (mean(apply(zz, 1:2, function (u) sd(u))))/2
  tt <- apply(zz, 1:2, function (u) BSDA::z.test(u, mu = 0, sigma.x = sd)$statistic)

  # # Get p value for each pixel
  # pmap <- apply(zz, 1:2, function(x) t.test(x)$p.value)
  #
  # # Create Z-map
  # zmap <- sign(zz[1:2]) * abs(qnorm(pmap/2))

  data <- as.data.frame(tt)

  if (write == TRUE) {
    data.table::fwrite(data, file = file.path(path, paste0(name,'_average_z_values.csv')), sep = ",",
                       append = FALSE, col.names = TRUE)

  } else {
    return(data)
  }

}

#' Pad image to given size
#'
#' @param image
#' @param width
#' @param height
#' @param background
#' @param out
#' @param write
#'
#' @return
#' @importFrom EBImage channel
#' @importFrom magick image_border image_crop as_EBImage
#' @export
#'
#' @examples
im_pad <- function(image, width, height, background, out, write = FALSE) {

  if (!hasArg(background)==TRUE) background <- "#7F7F7F"

  if (!hasArg(out) && write == TRUE) {
    out <- paste0(gsub('.png','',image),'_padded.png')
    }

  raw <- magick::image_read(image)
  meta <- magick::image_info(raw)
  desired_dims <- paste(width,height,sep='x')

  if( width >= meta$width & height >= meta$height ) {

    pad_width <- (width - meta$width)/2
    # pad_width <- round((width - meta$width)/2,0)
    # if ((pad_width)*2+meta$width != width) {
    #   pad_width_final <- pad_width + ((width - ((pad_width*2)+meta$width)) / 2)
    # } else pad_width_final -> pad_width

    pad_height <- (height - meta$height)/2
    # pad_height <- round((height - meta$height)/2,0)
    # if ((pad_height)*2+meta$height != height) {
    #   pad_height_final <- pad_height + ((height - ((pad_height*2)+meta$height)) / 2)
    # } else pad_height_final -> pad_height

    dims <- paste(pad_width, pad_height, sep='x')

    padded <- magick::image_border(raw, background, dims)
    padded <- magick::image_crop(padded, desired_dims)

  } else {
    stop('Supplied width and height are not compatible with image \n
         Desired dimensions are likely too small to pad with')
  }

  if (hasArg(out) && write == TRUE) {
    magick::image_write(padded, path = out)
  } else if (write == FALSE) {
    out <- magick::as_EBImage(padded)
    out <- EBImage::channel(out,"gray")
    return(out@.Data)
  }

}

