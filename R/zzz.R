cv <- NULL

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

# x <- reticulate::py_config()
#
# if (utils::compareVersion(x$version, "3.5") < 0) {
#   stop(
#     paste0(
#       c(
#         "Python 3.5+ is required. If this is installed please set RETICULATE_PYTHON ",
#         "to the path to the Python 3 binary on your system and try re-installing/",
#         "re-loading the package."
#       ),
#       collapse = ""
#     )
#   )
# }
#
# dsstore <- NULL
# os <- NULL
#
# .onLoad <- function(libname, pkgname) {
#   os <<- reticulate::import("cv2", delay_load = TRUE)
# }

