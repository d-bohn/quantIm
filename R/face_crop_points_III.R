#' Crop image around set of points
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
face_crop_points <- function(coords=NULL, image, points = 'default', savename = NULL,
                             eye_dist = 125, rescale = TRUE, wh = 512, ...){

  on.exit(raster::removeTmpFiles(h=2))

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    base <- '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata'
    file <- 'defiant2.jpg'
    # base <- '~/Desktop'
    # file <- "test3.png"
    image <- file.path(base,file)
    condaenv = 'quantIm'
    coords <- quantIm::face_landmarks(image = image, condaenv = condaenv)$face_landmarks
    eye_dist = 125
    wh = 512
    points = 'default'
    savename = NULL
    ### END DEBUGGING NOT RUN
  }

  if (is.null(coords)==TRUE){
    if(hasArg(python)){
      coords <- face_landmarks(image=image, python = python)$face_landmarks
    } else if(hasArg(condaenv)){
      coords <- face_landmarks(image=image, condaenv = condaenv)$face_landmarks
    } else message('Cannot configure python to run, check you have supplied proper arguments.')
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

    if (is.null(savename)==TRUE) {
      image_sans <- tools::file_path_sans_ext(image)
      savename <- paste0(image_sans,'_crop_scale.png')
    }

    attempt1 <- try( magick::image_write(img, path = savename, format = "png"), silent = TRUE)

    if (class(attempt1)=='try-error') {
      savename <- paste0(image_sans,'_crop_scale.jpg')
      attempt2 <- try( magick::image_write(img, path = savename, format = "jpg"), silent = TRUE )
      if (class(attempt2)=='try-error') message('Please provide an appropriate
                                                image type to be written.')
    }

    coords_new <- quantIm::face_landmarks(image = savename, condaenv = condaenv)$face_landmarks

    coords <- dplyr::bind_cols(coords, coords_new)

    return(coords)

  } else {

    if (is.null(savename)==TRUE) {
      image_sans <- tools::file_path_sans_ext(image)
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

