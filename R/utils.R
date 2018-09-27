#' @export
# quantIm helper functions ----
load_quantIm <- function() {
  if (is_quantIm()) {
      find_quantIm()
      cv <<- reticulate::import('cv2', delay_load = list(
        environment = "quantIm"
        ), convert = FALSE)
      dlib <<- reticulate::import('dlib', delay_load = list(
        environment = "quantIm"
      ), convert = FALSE)
    } else if (!is_quantIm()) {
      quantIm_not_found_message()
    }
}

find_quantIm <- function() {
  condaList <- reticulate::conda_list()
  if (('quantIm' %in% condaList$name)) {
    reticulate::use_condaenv('quantIm', required = TRUE)
  } else {
    quantIm_not_found_message()
  }
}

is_quantIm <- function() {
  condaInst <- reticulate::conda_binary()
  if (length(condaInst) != 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

quantIm_not_found_message <- function() {
  message('Please install quantIm with "install_quantIm()" function before using,')
  message('Or specify python environment with reticulate.')
}

# Rotate a matrix 90 degrees ----
rotate <- function(x) {
  t(apply(x, 2, rev))
}
