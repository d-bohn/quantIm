#' Pads an image to a certain width and height
#'
#' @param image
#' @param width
#' @param height
#' @param background
#'
#' @return
#' @export
#'
#' @examples
pad_image <- function(image,width,height,background=NULL,out=NULL){

  if(exists('db')==TRUE){
    ## START DEBUG NOT RUN
    image <- here('images2/gleamed','004_o_m_f_a_crop_gleamed.png')
    width <- 512
    height <- 512
    background <- "'#7F7F7F'"
    ## END DEBUG NOT RUN ##
  }

  if (is.null(system('convert -version'))==TRUE){

    message('Please install magick for your system.')

  } else{
    if (is.null(background)==TRUE) background <- "'#7F7F7F'"

    dims <- paste(width,height,sep = 'x')
    if (is.null(out)==TRUE) out <- paste0(gsub('.png','',image),'_padded.png')
    command1 <- paste('convert',image,'-gravity center -background',background,'-extent',dims,out, sep=' ')
    #command1 <- 'convert 004_o_m_f_a_crop_gleamed.png -gravity center -background white -extent 512x512 result.png'
    tryCatch(system(command1,intern=TRUE),
             error=function(e){
               message('Error. Please check that all options are correct by reviewing the documentation.')
               message('Also, make sure you supply the full path for the image!')
             })

    }
}
