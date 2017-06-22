#' Face Coordinates
#'
#' @author Daniel N. Albohn
#'
#' @param python_location The location of your python install. Defaults to
#'default install location for MacOS (e.g., \code{'/usr/local/bin/python'}).
#' @param facedetector External python script location for obtaining bounding box.
#' By default, it looks for this in the package's \code{data/} folder, but can
#' also manually specify.
#' @param image A string specifying the input image to detect face(s) in.
#'
#' @return Returns number of faces by 4 dimension dataframe specifying the bounding box
#' coordinates of each face and the width and height of each bonding box
#' @export
#'
#' @examples
#' im <- system.file('extdata', 'faces.jpg', package = 'quantIm')
#' face_coord(image=im)
#'
#' ...
#' @importFrom stringr str_split

face_coord <- function(python_location='/usr/local/bin/python', facedetector=NULL, image){

  # ##### FOR DEBUGGING NOT RUN
  # python_location='/usr/local/bin/python'
  # facedetector='data/facedetector'
  # image=file_list[3]
  # image = file.path('images',file_list[2])
  # #### END DEBUGGING NOT RUN

  ## Try to get facedetector from src folder
  command1 <- paste(python_location, system.file("python", "facedetector", package = "quantIm"), image, sep = ' ')
  coord <- try(
    system(command1, intern = TRUE)
    )

  ## If not, manually specify
  if (class(coord) == 'try-error') {
    command2 <- paste(python_location, facedetector, image, sep = ' ')
    coord <- system(command2, intern = TRUE)
  }
  if (class(coord) == 'try-error') {
    print('Error. Please check: \n
          1) that python and opencv2 are installed, \n
          2) your image has a face in it')
  }
  if (!(class(coord) == 'try-error') & (length(coord) > 0)==TRUE) {
    coords <- coord %>% data.frame(do.call(rbind, stringr::str_split(., '\\s+')))
    names(coords) <- c('Face','x','y','width','height')
    coords$Face <- paste0('face_',seq(1,nrow(coords),1))
    coords$image <- image
    return(list(coords))
  } else {
    coords <- data.frame('Image'=image)
    return(list(coords))
  }
}

