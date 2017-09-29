#' Subtract two images
#'
#' @param image1
#' @param image2
#' @param folder1
#' @param folder2
#' @param path
#' @param resize
#' @param write.data
#' @param sub
#'
#' @return
#' @export
#' @importFrom EBImage writeImage readImage resize
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr "%>%"
#' @importFrom here here
#' @importFrom tools file_path_sans_ext
#' @importFrom readr parse_number
#'
#' @examples

im_sub <- function(image1, image2, folder1=NULL, folder2=NULL, path,
                   resize = FALSE, write.data = TRUE, sub='default'){
  # library(dplyr)
  wd <- getwd()
  # setwd(path)

  if(is.null(folder1)){
    im1 <- EBImage::readImage(here::here(path,image1))
    im2 <- EBImage::readImage(here::here(path,image2))
  } else{
    im1 <- EBImage::readImage(here::here(path,folder1,image1))
    im2 <- EBImage::readImage(here::here(path,folder2,image2))
  }

  out <- im1 - im2

  if (sub == 'default'){
    file1_sans <- tools::file_path_sans_ext(image1)
    file2_sans <- tools::file_path_sans_ext(image2)
    sub <- paste0(file1_sans,'-',file2_sans)
  } else{
    try({
      sub <- readr::parse_number(image1)
      if(is.na(sub)==TRUE){
        class(sub) <- 'try-error'
      }
    })

    if (class(sub) == 'try-error'){
      file1_sans <- tools::file_path_sans_ext(image1)
      file2_sans <- tools::file_path_sans_ext(image2)
      sub <- paste0(file1_sans,'-',file2_sans)
    }
  }

  if(!(dir.exists((here::here(path, 'subtracted'))))){
    dir.create(here::here(path, 'subtracted'))
  }

  if(write.data == TRUE){
    df <- as.data.frame(out)

    ## For appending:
    # write.table(df, file = paste(path,"Image_data/image_data.csv",sep=""), sep = ",",
    #             append = TRUE, col.names = FALSE, row.names = FALSE)

    ## For separate files:
    # write.table(df, file = file.path('data','image_data',paste0('subject_',sub,'_subtracted.csv')), sep = ",",
    #             append = FALSE, col.names = TRUE, row.names = FALSE)

    # data.table::fwrite(df, file = here::here('data','image_data',paste0('subject_',sub,'_subtracted.csv')))
    data.table::fwrite(df, file = here::here(path, 'subtracted', paste0(sub,'_subtracted.csv')))
  }

  if(resize == TRUE){
    EBImage::resize(out, w = 363, h = 478)
  }

  #image(EBImage::flip(out))
  EBImage::writeImage(out, here::here(path, 'subtracted', paste0(sub,'_subtracted.jpg')))

  #(imager::imshift(pt2)-pre2) %>% plot(frame=2,main="Difference betw. frames 2 and 1")
  #setwd(wd)

  rm(list = ls())
}
