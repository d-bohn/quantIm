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
#'
#' @examples
face_crop_points <- function(coords=NULL, image, points = 'default', savename = NULL,
                             dist = '120', wh = '512'){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    base <- '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata'
    file <- "nm0000100_rm46373120_1955-1-6_2003.jpg"
    image <- file.path(base,file)
    condaenv = 'quantIm'
    coords <- face_landmarks(image = image, condaenv = condaenv)$face_landmarks
    dist = 116
    wh = 512
    points = c(30,28)
    savename = NULL
    ### END DEBUGGING NOT RUN
  }

  if (is.null(coords)==TRUE){
    coords <- face_landmarks(image=image)$face_landmarks
  }

  if (points == 'default'){
    points_list <- list(
      pupil_l = c(37,40,38,41),
      pupil_r = c(43,46,44,47)
    )
  }

  ## Check if it we have numeric values
  coords$x <- as.numeric(as.character(coords$x))
  coords$y <- as.numeric(as.character(coords$y))

  ## Get a proportion to scale by
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

  ## See what we need to scale it by scale
  pupily <- (left_pupil[2]+right_pupil[2])
  pupil_dist <- max(left_pupil[1],right_pupil[1]) - min(left_pupil[1],right_pupil[1])

  scale <- as.numeric(as.character(dist)) / as.numeric(as.character(pupil_dist))

  ## Got to check if scale, wh, and dist are compatible
  dims <- as.numeric(dim(EBImage::readImage(image)))

  if( min(dims[1],dims[2])*scale <= wh){
     scale <- as.numeric(as.character(wh))/min(dims[1],dims[2])
   }

  ## Get center coordinates
  coords <- resize_points(coords=coords, width=wh, height=wh)
  centerx <- round((coords$x_new[points[1]]))*scale
  centery <- round((coords$y_new[points[2]]))*scale

  ## Load and rescale
  percent = scale*100
  img <- magick::image_read(image)
  img <- magick::image_scale(img, paste0(percent,'%'))

  ## Now crop, fool, crop!
  if (is.null(wh)==TRUE) {
    wh <- as.numeric(dim(EBImage::readImage(image))[1])/2
    message('The image cropping will default to half of its width')
  }

  wh2 <- paste(wh,wh,sep = 'x')
  w_h_x_y <- paste0(wh2,'+',(centerx-(as.numeric(wh)/2)),'+',(centery-(as.numeric(wh)/2)))

  im <- magick::image_crop(img, w_h_x_y)
  # magick::image_browse(im)

  if (is.null(savename)==TRUE) {
    image_sans <- tools::file_path_sans_ext(image)
    savename <- paste0(image_sans,'_crop_scale.png')
  }

  attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

  if (class(attempt1)=='try-error') {
    savename <- paste0(image_sans,'_crop_scale.jpg')
    attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
    if (class(attempt2)=='try-error') message('Please provide an appropriate
                                              image type to be written.')
  }
  }

