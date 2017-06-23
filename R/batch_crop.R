#' Crop Batches of Images from a Folder
#'
#' @param file_list
#' @param path
#' @param save_string
#' @param gleam
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom EBImage writeImage
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr "%>%"

batch_crop <- function(file_list, path, save_string, scale = NULL, gleam = FALSE){

  # ### START DEBUGGING NOT RUN ##
  # setwd('/Volumes/Seagate Expansion Drive/machine_learning/face_image_pixels/')
  # file_list <- list.files('images/')
  # path = 'images/'; save_string = '_cropped'
  # gleam=TRUE; i=1; resize = .4
  # ### END DEBUGGING NOT RUN ##

  wd <- getwd()
  setwd(path)

  for (i in 1:length(file_list)){

    landmarks <- face_landmarks(image = file_list[i])

    if ((length(landmarks) > 1)==TRUE){

      bb <- as.data.frame(landmarks$bounding_box)
      coords <- as.data.frame(landmarks$face_landmarks)
      image <- file_list[i]

      if (!dir.exists('cropped')){

        dir.create('cropped')

        }

      save <- paste0(tools::file_path_sans_ext(file_list[i]),save_string,'.png')
      face_crop_points(coords, image, points = c(31,29), savename = file.path('cropped', save),
                       scale = scale, wh = '512')

      if (gleam==TRUE){
        image2 <- paste0(tools::file_path_sans_ext(save),'.png')
        im <- gleam(image = file.path('cropped',save))
        save2 <- paste0(tools::file_path_sans_ext(save),'_gleamed.png')

        if (!dir.exists('gleamed')){
          dir.create('gleamed')

          }

        EBImage::writeImage(im, file.path('gleamed',save2), type = "png")

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

    message(paste0('File ',file_list[i],' is done.'))

    }

  cat('Done. Check error log for errors.')

  setwd(wd)
  rm(list = ls())
}
