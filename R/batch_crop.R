#' Title
#'
#' @param file_list
#' @param path
#' @param save_string
#' @param gleam
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom EBImage writeImage
#' @importFrom tools file_path_sans_ext
#' @import dplyr

# ### START DEBUGGING NOT RUN ##
# setwd('~/Desktop/test')
# file_list <- list.files('images/')
# path = 'images/';save_string = 'test_crop_'
# gleam=TRUE;i=3;
# ### END DEBUGGING NOT RUN ##

batch_crop <- function(file_list, path, save_string, gleam = FALSE, ...){
  wd <- getwd()
  setwd(path)
  for (i in 1:length(file_list)){
    coords <- face_coord(image = file_list[i]) %>% as.data.frame(.)
    if ((length(coords) > 1)==TRUE){
      image <- file_list[i]
      save <- paste0(save_string,tools::file_path_sans_ext(file_list[i]),'.png')
      face_crop(coords, image, savename = save)
      if (gleam==TRUE){
        image2 <- paste0(tools::file_path_sans_ext(save),'.png')
        im <- gleam(image = save)
        save2 <- paste0('gleamed_',image2)
        EBImage::writeImage(im, save2, type = "png")
      }
    } else {
      if (file.exists('errorlog_.csv')==FALSE){
        df <- data.frame('Image' = file_list[i])
        write.table(df, paste0('errorlog_','.csv'), sep = ',', row.names = FALSE)
      }
      image <- file_list[i]
      write.table(image, 'errorlog_.csv', sep=',', row.names = FALSE, col.names = FALSE, append = TRUE)
      next
      }
  }
  cat('Done. Check error log for errors.')
  setwd(wd)
  rm(list = ls())
}
