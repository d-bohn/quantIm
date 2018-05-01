#' Compute t-test on multiple images
#'
#' @param path File path to folder with subtracted image files and values.
#' @param pattern1 Pattern that the subtracted files were saved in. Defaults to '.csv'
#' @param name Name that the t-values file should be saved as.
#' @param write Should the file be saved (\code{TRUE}), or returned as a vector (\code{FALSE}).
#' Defaults to \code{TRUE}.
#'
#' @return
#'
#'
#' @export
#' @importFrom abind abind
#' @importFrom here here
#' @importFrom data.table fwrite fread
#'
#' @examples

im_t_test <- function(path, pattern1 = '*.csv', name, write = TRUE){
  library(dplyr)
  # wd <- getwd()
  # setwd(path)

  if(!hasArg(path)){
    stop('Please supply folder path...')
  }

  files <- list.files(path = path, pattern = pattern1, full.names = TRUE)

  list <- purrr::map(files, data.table::fread)
  zz <- purrr::reduce(list, abind::abind, along = 3)

  # setwd(path)
  # list <- list2env(
  #   lapply(setNames(files, make.names(gsub("*.csv$", "", files))),
  #          data.table::fread), envir = .GlobalEnv)
  # setwd(wd)

  # lst <- Filter(function(x) is(x, "data.frame"), mget(ls()))
  # lst <- lapply(ls(pattern=pattern2), function(x) get(x))
  # ar1 <- array(unlist(lst), dim = c(dim(lst[1]), length(lst)))
  # res2 <-  apply(aperm(ar1, c(3, 1, 2)), c(2,3), FUN = function(x) t.test(x, mu = 0)$p.value)
  # str(res2)

  # zz <- do.call(abind::abind, c(lst, along = 3))

  tt <- apply(zz, 1:2, function (u) t.test(u, mu = 0)$statistic)

  # apply(tt, 1, function(x) lapply(x, identity))

  # z <- matrix(tapply(unlist(lst, use.names = FALSE),
  #                    rep(gl(prod(dim(lst[[1]])), 1), length(lst)),
  #                    #FUN = function (u) t.test(u, mu = 0)$p.value),
  #                    FUN = function (u) t.test(u, mu = 0)$statistic),
  #             nrow = nrow(lst[[1]]))

  # data <- as.data.frame(z)
  # rm(list= ls()[!(ls() %in% c('tt','path','name'))])
  data <- as.data.frame(tt)

  if (write == TRUE) {
    data.table::fwrite(data, file = file.path(path,paste0(name,'_average_t_values.csv')), sep = ",",
                       append = FALSE, col.names = TRUE)
  } else {
    return(data)
  }

}
