#' Compute z-test on multiple images
#'
#' @param path File path to folder with subtracted image files and values.
#' @param pattern1 Pattern that the subtracted files were saved in. Defaults to '.csv'
#' @param name Name that the t-values file should be saved as.
#' @param write Should the file be saved (\code{TRUE}), or returned as a vector (\code{FALSE}).
#' Defaults to \code{TRUE}.
#'
#' @return
#'
#' @export
#' @importFrom abind abind
#' @importFrom here here
#' @importFrom data.table fwrite fread
#'
#' @examples

im_z_test <- function(path, data, pattern1 = '*.csv', name, write = TRUE){
  library(dplyr)

  if (!hasArg(path) && length(dim(data)) != 3) {
    stop('Please supply folder path,')
    stop('or 3D dataframe.')
  }

  if (hasArg(path)) {
    files <- list.files(path = path, pattern = pattern1, full.names = TRUE)

    list <- purrr::map(files, data.table::fread)
    zz <- purrr::reduce(list, abind::abind, along = 3)
  }

  if (hasArg(data) && length(dim(data)) == 3) {
    zz <- data
  }

  sd <- (mean(apply(zz, 1:2, function (u) sd(u))))/2
  tt <- apply(zz, 1:2, function (u) BSDA::z.test(u, mu = 0, sigma.x = sd)$statistic)

  # # Get p value for each pixel
  # pmap <- apply(zz, 1:2, function(x) t.test(x)$p.value)
  #
  # # Create Z-map
  # zmap <- sign(zz[1:2]) * abs(qnorm(pmap/2))

  data <- as.data.frame(tt)

  if (write == TRUE) {
    data.table::fwrite(data, file = file.path(path, paste0(name,'_average_z_values.csv')), sep = ",",
                       append = FALSE, col.names = TRUE)

  } else {
    return(data)
  }

}
