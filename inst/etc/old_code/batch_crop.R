#' Crop Batches of Images from a Folder
#'
#' @param file_list
#' @param path
#' @param save_string
#' @param gleam
#' @param dist
#' @param pad
#'
#' @return
#'
#' @examples
#'
#' @importFrom EBImage writeImage
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr "%>%"
#' @importFrom here here

batch_crop <- function(file_list, path, save_string, dist, wh, points,
                       gleam = FALSE, pad = FALSE, ...){

  if(exists('db')==TRUE){
    ### START DEBUGGING NOT RUN ##
    path <- '/Volumes/ALBOHN/machine_learning/face_image_pixels/images2/'
    file_list <- list('004_o_m_a_a.jpg');i=1
    save_string = '_crop'
    points = c(30,28)
    dist = '116'
    wh='512'
    gleam=TRUE; pad=TRUE
    condenv='quantIm'
    ### END DEBUGGING NOT RUN ##
  }

  # wd <- getwd()
  # setwd(path)

  if (hasArg(condaenv)) reticulate::use_python(quantIm::find_python(condaenv), required = TRUE)
  if (hasArg(python)) reticulate::use_python(python)

  for (i in 1:length(file_list)){

    landmarks <- face_landmarks(image = file_list[i])

    if ((length(landmarks) > 1)==TRUE){

      bb <- as.data.frame(landmarks$bounding_box)
      coords <- as.data.frame(landmarks$face_landmarks)
      image <- file_list[[i]]

      if (!dir.exists('cropped')){

        dir.create('cropped')

        }

      save <- paste0(tools::file_path_sans_ext(file_list[i]),save_string,'.png')
      face_crop_points(coords, image, points = points, savename = file.path('cropped', save),
                       dist=dist, wh = wh)

      if (gleam==TRUE){
        image2 <- paste0(tools::file_path_sans_ext(save),'.png')
        im <- gleam(image = file.path('cropped',save))
        save2 <- paste0(tools::file_path_sans_ext(save),'_gleamed.png')

        if (!dir.exists('gleamed')){
          dir.create('gleamed')

          }

        EBImage::writeImage(im, file.path('gleamed',save2), type = "png")

      }

      if (pad==TRUE){

        if (!dir.exists('padded')){
          dir.create('padded')
        }

        out <- paste0('padded/',gsub('.png','',save2),'_padded.png')
        im <- pad_image(image = file.path('gleamed',save2),width=wh,height=wh,out=out)

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
