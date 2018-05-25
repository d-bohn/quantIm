cv <- NULL

.onLoad <- function(libname, pkgname) {
  if (is_quantIm()) {
    find_quantIm()
    cv <<- reticulate::import('cv2', delay_load = list(
      environment = "quantIm"
      ))
  } else if (!is_quantIm()) {
    quantIm_not_found_message()
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
# cv <- NULL
#
# .onLoad <- function(libname, pkgname) {
#   cv <<- reticulate::import("cv2", delay_load = TRUE)
# }

