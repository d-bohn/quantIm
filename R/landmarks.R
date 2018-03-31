#' Detect Face Landmarks and Bounding Box
#'
#' @author Daniel N. Albohn
#'
#' @description
#' A simple wrapper for dlib's face detector algorithm.
#'
#' @details
#' The following python modules must be installed and accessible to the version of python
#' specified in \code{python_location} (see parameters):
#' \itemize{
#'     \item{\code{imutils}}
#'     \item{\code{cv2}}
#'     \item{\code{dlib}}
#'     \item{\code{numpy}}
#' }
#' All of these can be installed from the command line with \code{pip install} on Unix systems
#' except for \code{openCV2} which needs to be downloaded. Additionally, the following
#' packages must be installed and up to date:
#'
#' \itemize{
#'     \item{\code{sudo apt-get install cmake}}
#'    \item{\code{sudo apt-get install libboost-all-dev}}
#'    \item{\code{sudo apt-get install libmagick++-dev}}
#'    \item{\code{sudo apt-get install libopencv-dev python-opencv}}
#'}
#'
#' @param image A string specifying the input image to detect face(s) in.
#' @param python
#'
#' @return Returns a list which contains dlib's 68 facial landmarks
#' as well as the bounding box that the face in the image exists.
#'
#' @seealso
#' dlib \link{http://dlib.net/face_landmark_detection.py.html}
#' OpenCV2 \link{http://opencv.org/platforms/}
#' imagemagick and magick \link{https://ropensci.org/blog/2016/08/23/z-magick-release}
#'
#' @export
#'
#' @examples
#' image = system.file("extdata", "defiant2.jpg", package = "quantIm")
#' face_landmarks(image=image, python=NULL)
#'----
#' @importFrom stringr str_split
#' @importFrom magrittr "%>%"
#'
face_landmarks <- function(image, python = NULL, condaenv = NULL){

  # base <- '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata'
  # file <- "nm0000100_rm46373120_1955-1-6_2003.jpg"
  # image <- file.path(base,file)
  # condaenv = 'quantIm'

  python_location <- quantIm::find_python(condaenv = 'quantIm')
  #invisible(reticulate::py_discover_config(required_module = 'cv2'))

  ## Try to get try to get the script form the inst/python folder
  script <- system.file("python", "facial_landmarks.py", package = "quantIm")
  dlib_shape <- system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "quantIm")
  command1 <- paste(python_location, script,'-p', dlib_shape, '-i', image, sep = ' ')

  landmarks <- try(
    system(command1, intern = TRUE),
    silent = TRUE
    )

  if (class(landmarks) == 'try-error') {
    python_location2 <- system('which python', intern = TRUE)
    command2 <- paste(python_location2, script,'-p', dlib_shape, '-i', image, sep = ' ')
    landmarks <- try(
      system(command2, intern=TRUE)
    )
  }

  if (class(landmarks) == 'try-error') {
    message('Error. Please check:')
    message('1) that python and the necessary packages are installed')
    message('2) your image has at least one face in it')
  }
  if (!(class(landmarks) == 'try-error') & (length(landmarks) > 0)==TRUE) {
    points <- landmarks[1:68]
    bb <- landmarks[69]

    points <- points %>% data.frame(do.call(rbind, stringr::str_split(., '\\s+'))) %>% .[2:3]
    names(points) <- c('x','y')
    points$point <- seq(0,(nrow(points)-1),1)
    #points$face <- paste0('face_',seq(1,nrow(points),1))
    points$image <- image

    bb <- bb %>% data.frame(do.call(rbind, stringr::str_split(., '\\s+')))
    names(bb) <- c('face','x','y','width','height')
    bb$image <- image
    bb$face <- paste0('face_',seq(1,nrow(bb),1))

    #meta <- dim(EBImage::readImage(image))

    return(list(face_landmarks = points, bounding_box = bb))
  } else {
    points <- data.frame('Image'=image)
    return(list(points))
  }
  }
