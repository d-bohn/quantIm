#' Crop a face and rescale
#'
#' @aliases Daniel N. Albohn
#'
#' @param coords
#' @param image
#' @param points
#' @param savename
#' @param wh
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
face_crop_points <- function(coords, image, points = 'default', savename,
                             eye_dist = 125, rescale = TRUE, wh = 512, ...){

  on.exit(raster::removeTmpFiles(h=2))

  if (!hasArg(coords)) {
    coords <- quantIm::get_landmarks(image)
  }

  if (points == 'default'){
    points_list <- list(
      pupil_l = c(37,40,38,41),
      pupil_r = c(43,46,44,47),
      bridge = c(30,28),
      top_brows = 17:22,
      bottom_chin = 8L
    )
  }

  ## Check if it we have numeric values
  coords$x <- as.numeric(as.character(coords$x))
  coords$y <- as.numeric(as.character(coords$y))

  ## Function to estimate center of four points in a rectangle
  centerC <- function(point1,point2,point3,point4){

    point1x <- coords$x[coords$point==point1]; point1y <- coords$y[coords$point==point1]
    point2x <- coords$x[coords$point==point2]; point2y <- coords$y[coords$point==point2]
    point3x <- coords$x[coords$point==point3]; point3y <- coords$y[coords$point==point3]
    point4x <- coords$x[coords$point==point4]; point4y <- coords$y[coords$point==point4]

    mid1x <- (point1x+point2x) / 2
    mid2x <- (point3x+point4x) / 2
    mid1y <- (point1y+point2y) / 2
    mid2y <- (point3y+point4y) / 2

    centerx <- (mid1x + mid2x) / 2
    centery <- (mid1y + mid2y) / 2

    return(c(centerx,centery))

  }

  ## Estimate center of left pupil
  left_pupil <- centerC(points_list$pupil_l[[1]],points_list$pupil_l[[2]],
                        points_list$pupil_l[[3]],points_list$pupil_l[[4]])
  ## Estimate center of right pupil
  right_pupil <- centerC(points_list$pupil_r[[1]],points_list$pupil_r[[2]],
                         points_list$pupil_r[[3]],points_list$pupil_r[[4]])

  ## Get our values to crop by
  pupil_dist <- max(left_pupil[1],right_pupil[1]) - min(left_pupil[1],right_pupil[1])

  scale <- as.numeric(as.character(eye_dist)) / as.numeric(as.character(pupil_dist))
  wh_scale <- wh/scale

  centerx <- left_pupil[[1]]-(190/scale)
  centery <- left_pupil[[2]]-(256/scale)

  ## Now crop!
  image_sans <- tools::file_path_sans_ext(image)
  img <- magick::image_read(image)
  # wh2 <- paste(wh_scale,wh_scale,sep = 'x')
  w_h_x_y <- magick::geometry_area(wh_scale, wh_scale, centerx, centery)

  im <- magick::image_crop(img, w_h_x_y)
  # magick::image_browse(im)

  ## Load and rescale
  if (rescale==TRUE){
    percent = scale*100
    img <- magick::image_scale(im, paste0(percent,'%'))
    # magick::image_browse(img)

    if (hasArg(savename)) {
      savename <- paste0(image_sans,'_crop_scale.png')
    }

    attempt1 <- try( magick::image_write(img, path = savename, format = "png"), silent = TRUE)

    if (class(attempt1)=='try-error') {
      savename <- paste0(image_sans,'_crop_scale.jpg')
      attempt2 <- try( magick::image_write(img, path = savename, format = "jpg"), silent = TRUE )
      if (class(attempt2)=='try-error') message('Please provide an appropriate
                                                image type to be written.')
    }

    coords_new <- quantIm:::get_landmarks(image = savename)

    coords <- dplyr::bind_cols(coords, coords_new)

    return(coords)

  } else {
    if (!hasArg(savename)) {
      savename <- paste0(image_sans,'_crop.png')
    }

    attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

    if (class(attempt1)=='try-error') {
      savename <- paste0(image_sans,'_crop.jpg')
      attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
      if (class(attempt2)=='try-error') message('Please provide an appropriate
                                                image type to be written.')
    }
  }
  }

