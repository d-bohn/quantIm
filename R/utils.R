#' Helper function to find python on user system
#'
#' @param python
#' @param condaenv
#'
#' @importFrom reticulate use_condaenv conda_list use_python
#'
find_python <- function(python, condaenv){
  if(hasArg(condaenv)){
    reticulate::use_condaenv(condaenv)
    condalist <- reticulate::conda_list()
    py_loc <- ifelse(condaenv %in% condalist$name,
                     condalist[condalist$name==condaenv,'python'],
                     NA)
    message('The python version this system is using is located at: ', py_loc)
  } else if(hasArg(python)){
    ## Try for homebrew first
    python_hb <- suppressWarnings( system('which python2', intern = TRUE) )
    ## If null...try default python
    if(!is.null(attr(python_hb,'status'))){
      py_locT <- system("which python", intern=TRUE)
      message('The python version this system is using is located at: ', py_loc)
      yn1 <- readline('Is this correct? (Y, N): ')
      ## If no, supply it
      if ((yn1 == 'n' | yn1 == 'N' | yn1 == 'NO' | yn1 == 'no' | yn1 == 'No') == TRUE) {
        py_locU <- readline('Please provide desired python location executable: ')
        py_loc <- py_locU
        reticulate::use_python(py_locU)
      } else {
        py_loc <- py_locT
        reticulate::use_python(py_locT)
      }
    } else{
      py_loc <- python_hb
      reticulate::use_python(python_hb)
    }
  } else {
    py_loc <- python
    reticulate::use_python(python)
  }
  return(py_loc)
}

#' Rotate a matrix 90 degrees
#'
#' @param x
#'
rotate <- function(x){
  t(apply(x, 2, rev))
}

#' Create a matrix of face points
#'
#' @param image
#' @param points
#' @param ntimes
#'
#' @importFrom dplyr select mutate
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom RSAGA grid.to.xyz

create_matrix <- function(image, points, ntimes){

  points2 <- points %>%
    dplyr::select(., x,y,point) %>%
    dplyr::mutate(., x = as.numeric(as.character(x)),
           y = as.numeric(as.character(y)),
           point = as.numeric(as.character(point)))

  img <- try(jpeg::readJPEG(image))
  if (class(img) == 'try-error') img <- png::readPNG(image)
  if (class(img) == 'try-error') message('Please supply either a .jpg or .png image.')

  dims <- dim(img)
  a <- array(0L, dims)
  b <- as.matrix(points2[1:2])
  a[b] <- 1

  a <- rotate(a)

  points_rotated <- RSAGA::grid.to.xyz(a)

  points_rotated <- points_rotated[points_rotated$z != 0,]
}

