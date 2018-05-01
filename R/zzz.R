cv <- NULL

#' @importFrom reticulate conda_list import conda_binary

.onLoad <- function(libname, pkgname){
  cv <<- reticulate::import('cv2', delay_load = TRUE)

  condaInst <- reticulate::conda_binary()

  if (length(condaInst) != 0){
    condaList <- reticulate::conda_list()
    if (('quantIm' %in% condaList$name)) {
      quantIm:::find_python(condaenv = 'quantIm')
    }
  } else {
    message('Please install dependencies with')
    message('"install_quantIm()" function before using.')
  }

}
