#' Pads an image to a certain width and height
#'
#' @param image
#' @param width
#' @param height
#' @param background
#'
#' @return
#'
#' @examples
pad_image <- function(image,width,height,background,out){

  if(exists('db')==TRUE){
    ## START DEBUG NOT RUN
    image <- '/Users/dalbohn/Desktop/test2.png'
    width <- 512
    height <- 512
    # background <- "'#7F7F7F'"
    condaenv <- 'quantIm'
    ## END DEBUG NOT RUN ##
  }

  if (hasArg(condaenv)) {
    quantIm::find_python(condaenv=condaenv)
  }

  if (hasArg(condaenv)) reticulate::use_python(quantIm::find_python(python=python), required = TRUE)

  if (is.null(system('convert -version'))==TRUE){

    message('Please install magick for your system.')

  } else{
    if (!hasArg(background)==TRUE) background <- "'#7F7F7F'"

    dims <- paste(width,height,sep = 'x')
    if (!hasArg(out)) out <- paste0(gsub('.png','',image),'_padded.png')
    command1 <- paste('/Users/dalbohn/anaconda/envs/quantIm/bin/convert',image,'-gravity center -background',background,'-extent',dims,out,sep=' ')
    #magick::image_convert(image, gravity=='center',)
    #command1 <- 'convert 004_o_m_f_a_crop_gleamed.png -gravity center -background white -extent 512x512 result.png'
    tryCatch(system(command1,intern=TRUE),
             error=function(e){
               message('Error. Please check that all options are correct by reviewing the documentation.')
               message('Also, make sure you supply the full path for the image!')
             })

    }
}
