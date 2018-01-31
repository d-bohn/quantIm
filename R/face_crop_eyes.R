face_crop_eyes <- function(coords=NULL, image, points = c(36,45), savename = NULL,
                           dist = '116', wh = 512, w=235, h = 135){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    base <- '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata'
    file <- "nm0000114_rm129141504_1957-12-13_2009.jpg"
    file <- "nm0000100_rm46373120_1955-1-6_2003.jpg"
    image <- file.path(base,file)
    condaenv = 'quantIm'
    coords <- face_landmarks(image = image, condaenv = condaenv)$face_landmarks
    dist = 116
    wh = 512;w=235;h=135
    points = c(36,45)
    savename = NULL
    ### END DEBUGGING NOT RUN
  }

  if (is.null(coords)==TRUE){
    coords <- face_landmarks(image=image)$face_landmarks
  }

  # Check if it we have numeric values
  coords$x <- as.numeric(as.character(coords$x))
  coords$y <- as.numeric(as.character(coords$y))

  # Get a proportion to scale by
  point1=37;point2=41
  point3=38;point4=40
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
  left_pupil <- centerC(37,40,38,41)
  ## Estimate center of right pupil
  right_pupil <- centerC(43,46,44,47)

  # See what we need to scale it by scale
  pupily <- (left_pupil[2]+right_pupil[2])/2
  pupilx <- (left_pupil[1]+right_pupil[1])/2
  pupil_dist <- max(left_pupil[1],right_pupil[1]) - min(left_pupil[1],right_pupil[1])

  scale <- as.numeric(as.character(dist)) / as.numeric(as.character(pupil_dist))

  # Got to check if scale, wh, and dist are compatible
  dims <- as.numeric(dim(EBImage::readImage(image)))
  if( min(dims[1],dims[2])*scale <= wh){
    scale <- as.numeric(as.character(wh))/min(dims[1],dims[2])
  }

  # Get center coordinates
  # centerx <- round((coords$x[points[1]]))*scale
  # centery <- round((coords$y[points[2]]))*scale

  # Load and rescale
  percent <- scale*100
  img <- magick::image_read(image)
  img <- magick::image_scale(img, paste0(percent,'%'))

  # Now crop, fool, crop!
  if (is.null(wh)==TRUE) {
    wh <- as.numeric(dim(EBImage::readImage(image))[1])/2
    message('The image cropping will default to half of its width')
  }

  wh2 <- paste(w,h,sep = 'x')
  w_h_x_y <- paste0(wh2,'+',(pupilx-(as.numeric(wh)/2)),'+',(pupily-(as.numeric(wh)/2)))
  w_h_x_y <- paste0(wh2,'+',pupilx,'+',pupily)
  im <- magick::image_crop(img, w_h_x_y)
  magick::image_browse(im)

  if (is.null(savename)==TRUE) {
    image_sans <- tools::file_path_sans_ext(image)
    savename <- paste0(image_sans,'_crop_scale.png')
  }

  attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

  if (class(attempt1)=='try-error') {
    savename <- paste0(image_sans,'_crop_scale.jpg')
    attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
    if (class(attempt2)=='try-error') message('Please provide an appropriate image type to be written.')
  }
}

