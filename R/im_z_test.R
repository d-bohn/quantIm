#' Compute t-test on multiple images
#'
#' @param path
#' @param pattern1
#' @param name
#'
#' @return
#' @export
#' @importFrom abind abind
#' @importFrom here here
#' @importFrom data.table fwrite fread
#'
#' @examples

im_z_test <- function(path = NULL, pattern1 = '*.csv', name, write = TRUE){
  library(dplyr)
  # wd <- getwd()
  # setwd(path)

  if(is.null(path)){
    stop('Please supply folder path...')
  }

  files <- list.files(path = path, pattern = pattern1, full.names = TRUE)

  list <- purrr::map(files, data.table::fread)
  zz <- purrr::reduce(list, abind::abind, along = 3)

  sd <- (mean(apply(zz, 1:2, function (u) sd(u))))/2
  tt <- apply(zz, 1:2, function (u) BSDA::z.test(u, mu = 0, sigma.x = sd)$statistic)

  # # Get p value for each pixel
  # pmap <- apply(zz, 1:2, function(x) t.test(x)$p.value)
  #
  # # Create Z-map
  # zmap <- sign(zz[1:2]) * abs(qnorm(pmap/2))

  data <- as.data.frame(tt)

  if (write == TRUE) {
    data.table::fwrite(data, file = file.path(path,paste0(name,'_average_t_values.csv')), sep = ",",
                       append = FALSE, col.names = TRUE)
  } else {
    return(data)
  }

}
