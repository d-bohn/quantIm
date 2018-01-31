pad_image <- function(image,width,height,background,out){

  if(exists('db')==TRUE){
    ## START DEBUG NOT RUN
    image <- '/Users/dalbohn/Desktop/male.jpg'
    width <- 512
    height <- 512
    # background <- "#7F7F7F"
    condaenv <- 'quantIm'
    ## END DEBUG NOT RUN ##
  }

  if (!hasArg(background)==TRUE) background <- "#7F7F7F"

  if (!hasArg(out)) out <- paste0(gsub('.png','',image),'_padded.png')

  raw <- magick::image_read(image)
  meta <- magick::image_info(raw)
  desired_dims <- paste(width,height,sep='x')

  if( width > meta$width & height > meta$height ){
    pad_width <- round((width - meta$width)/2,0)
    pad_height <- round((height - meta$height)/2,0)
    dims <- paste(pad_width,pad_height,sep='x')

    padded <- magick::image_border(raw, background, dims)
    padded <- magick::image_crop(padded, desired_dims)
    magick::image_write(padded, path = out)

  } else{
    stop('Supplied width and height are not compatible with image')
    stop("Desired dimensions are likely too small to pad with")
  }

}
