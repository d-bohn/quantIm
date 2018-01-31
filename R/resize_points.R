resize_points <- function(coords, image, width, height){

    info_o <- magick::image_info(magick::image_read(coords$image[1]))

    if(!missing(image)){
      info_n <- magick::image_info(magick::image_read(image))
    } else if(!missing(width) && !missing(height)){
      info_n <- data.frame(width=width,height=height)
    } else{
      message('No image or width/height specifications provided. Defaulting to 512x512')
      info_n <- data.frame(width=512L, height=512L)
  }

  old_w = info_o$width
  old_h = info_o$height
  new_w = info_n$width
  new_h = info_n$height

  d = data.frame( x_new=rep(0, nrow(coords)), y_new=rep(0,nrow(coords)))

  for (i in 1:nrow(coords)){
    x <- (new_w*coords[i,'x'])/old_w
    y <- (new_h*coords[i,'y'])/old_h
    d[i,] <- c(x,y)
  }
  return(coords <- cbind(coords,d))
}

#coords <- resize_points(image = image, coords = coords)
