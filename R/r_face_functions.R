#' Create affine transformation
#'
#' @param landmarks1
#' @param landmarks2
#'
#' @return
#' @importFrom vegan procrustes
#' @export
#'
#' @examples
#'
transform_points <- function(landmarks1, landmarks2) {
  if (ncol(landmarks1) == 3) {
    landmarks1 <- landmarks1[2:3]
    }
  if (ncol(landmarks2) == 3) {
    landmarks2 <- landmarks2[2:3]
  }

  tmp <- vegan::procrustes(landmarks1, landmarks2)
  r <- unlist(tmp['rotation'])
  t <- unlist(tmp['translation'])

  affine <- matrix(c(r[[1]],r[[2]],t[[1]],r[[3]],r[[4]],t[[2]]), nrow=3)
  return(affine)
}

#' Warp image with affine transformation
#'
#' @param image
#' @param m
#'
#' @return
#' @importFrom EBImage affine readImage
#' @export
#'
#' @examples
#'
warp_face <- function(image, m) {
  tmp <- EBImage::affine(
    x = EBImage::readImage(image),
    m = m,
    bg.col = "#777777"
  )
  return(tmp)
}

#' Create an average template from multiple templates
#'
#' @param files
#'
#' @return
#' @importfrom abind abind
#' @export
#'
#' @examples
#'
create_average_template <- function(files) {
  mat3d <- NULL
  for (file in 1:length(files)) {
    landmarks <- get_landmarks(files[[file]])[2:3]
    mat3d <- abind::abind(mat3d, landmarks, along = 3)
  }

  average_temp <- apply(mat3d, 1:2, mean)
  return(average_temp)
}

#' Converts landmarks file to json file
#'
#' @param landmarks Landmarks returned from \link{\code{get_landmarks}}
#' @param write_out If \code{TRUE} (default) write file to disk, else returns landmarks as json.
#' @param savename If supplied the name of the file to write out, else will derive directory from
#' landmarks file supplied.
#'
#' @return File or landmarks as type \code{.json}
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @examples
#' img <- system.file("extdata", "obama.png", package = "quantIm")
#' landmarks <- get_landmarks(img)
#' landmarks_to_json(landmarks)
#'
landmarks_to_json <- function(landmarks, write_out = TRUE, savename) {

  if (isFALSE(is.data.frame(landmarks)) & isFALSE(is.character(landmarks))) {
    message("Please supply landmark dataframe or path")
  } else if (isFALSE(is.data.frame(landmarks)) & isTRUE(is.character(landmarks))) {
    landmark_file <- landmarks
    landmarks <- read.csv(landmarks)
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (isFALSE(hasArg(savename))) {
      savename <- file.path(dirname(landmark_file),paste0(file,'__labels.json'))
    }
  } else if (isTRUE(is.data.frame(landmarks))) {
    landmarks <- landmarks
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (isFALSE(hasArg(savename))) {
      savename <- file.path(dirname(as.character(landmarks$image_path[[1]])),paste0(file,'__labels.json'))
    }
  }

  tem <- jsonlite::fromJSON(system.file("extdata", "dlib-landmark-mean__labels.json", package = "quantIm"))

  tem$image_filename <- landmarks$image_base[[1]]
  tem$labels$position$x <- landmarks$x
  tem$labels$position$y <- landmarks$y

  if (write_out == TRUE) {
    jsondf <- jsonlite::toJSON(tem, dataframe = 'row', raw = 'mongo')
    readr::write_lines(jsondf, savename)
  } else if (write_out == FALSE) {
    jsondf <- jsonlite::toJSON(tem, dataframe = 'row', raw = 'mongo')
    return(jsondf)
  }

}

#' Converts landmarks to JPsychomorph tem file
#'
#' @param landmarks Landmarks returned from \link{\code{get_landmarks}}
#' @param write_out If \code{TRUE} (default) write file to disk, else returns landmarks as json.
#' @param savename If supplied the name of the file to write out, else will derive directory from
#' landmarks file supplied.
#'
#' @return \code{.tem} file or data frame
#' @importFrom readr write_delim
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @examples
#'
landmarks_to_tem <- function(landmarks, write_out = TRUE, savename) {
  if (isFALSE(is.data.frame(landmarks)) & isFALSE(is.character(landmarks))) {
    message("Please supply landmark dataframe or path")
  } else if (isFALSE(is.data.frame(landmarks)) & isTRUE(is.character(landmarks))) {
    landmark_file <- landmarks
    landmarks <- read.csv(landmarks)
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (isFALSE(hasArg(savename))) {
      savename <- file.path(dirname(landmark_file),paste0(file,'__labels.json'))
    }
  } else if (isTRUE(is.data.frame(landmarks))) {
    landmarks <- landmarks
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (isFALSE(hasArg(savename))) {
      savename <- file.path(dirname(as.character(landmarks$image_path[[1]])),paste0(file,'__labels.json'))
    }
  }

  df <- data.frame(x = landmarks$x, y = landmarks$y)
  data <- rbind(c(68,NA),df,c(0,NA))

  if (write_out == TRUE) {
    readr::write_delim(data, savename, delim = ' ', na = '', col_names = FALSE)
  } else if (write_out == FALSE) {
    return(data)
  }
}
