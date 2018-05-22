#' Pad image to give size
#'
#' @param image
#' @param width
#' @param height
#' @param background
#' @param out
#'
#' @return
#' @export
#'
#' @examples
pad_image <- function(image, width, height, background, out) {

  if(exists('db')==TRUE){
    ## START DEBUG NOT RUN
    image <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/S031.Pre.Anger.png'
    width <- 428
    height <- 480
    # background <- "#7F7F7F"
    condaenv <- 'quantIm'
    ## END DEBUG NOT RUN ##
  }

  if (!hasArg(background)==TRUE) background <- "#7F7F7F"

  if (!hasArg(out)) out <- paste0(gsub('.png','',image),'_padded.png')

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

    dims <- paste(pad_width,pad_height,sep='x')

    padded <- magick::image_border(raw, background, dims)
    padded <- magick::image_crop(padded, desired_dims)
    magick::image_write(padded, path = out)

  } else{
    stop('Supplied width and height are not compatible with image')
    stop("Desired dimensions are likely too small to pad with")
  }

}
