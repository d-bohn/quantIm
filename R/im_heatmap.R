#' Create heatmap image from t-values
#'
#' @param path
#' @param file
#' @param base_image
#' @param mask
#' @param thresh
#'
#' @return
#' @export
#' @import ggplot2 grid
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom here here
#' @importFrom RSAGA grid.to.xyz
#' @importFrom EBImage readImage
#' @importFrom data.table fread
#' @importFrom tools file_ext
#'
#' @examples

im_heatmap <- function(path, file = paste0(name,"_average_t_values.csv"), base_image, mask=NULL, thresh=1.96){

  ext <- tools::file_ext(base_image)
  if((ext == 'png') == TRUE){
    image <- png::readPNG(here::here(base_image))
    img <- rasterGrob(image, interpolate=TRUE)
  } else if ((ext=='jpg')==TRUE){
    image <- jpeg::readJPEG(here::here(base_image))
    img <- rasterGrob(image, interpolate=TRUE)
  } else {
    message ('Please supply either a .jpg or .png picture file.')
    break
  }

  mext <- tools::file_ext(mask)
  if (!is.null(mask)){
    if((mext == 'png') == TRUE){
      mask_im <- EBImage::readImage(mask)
    } else if ((mext=='jpg')==TRUE){
      mask_im <- EBImage::readImage(mask)
    } else {
      message ('Please supply either a .jpg or .png mask file.')
      break
    }
  }

  df <- data.table::fread(here::here(path,file))
  mat <- data.matrix(df)
  mat2 <- mat*mask_im

  rotate <- function(x) t(apply(x, 2, rev))
  mat3 <- rotate(rotate(mat2))
  # mat3 <- mat2


  mat_rotate <- RSAGA::grid.to.xyz(mat3)
  data <- as.data.frame(mat_rotate)

  data2 <- data

  data2$z2 <- ifelse(abs(data2$z) <= thresh, NA, data2$z)

  g <- rasterGrob(image)

  plot <- ggplot(data2, aes(x = y, y = x, fill = z2)) +
    # annotation_custom(rasterGrob(image, width=unit(1,"npc"), height=unit(1,"npc")),
    #                   -Inf, Inf, -Inf, Inf) +
    annotation_custom(g, xmin=min(data2$y), xmax=max(data2$y), ymin=min(data2$x), ymax=max(data2$x)) +
    #annotation_custom(img, xmin=0, xmax=236, ymin=0, ymax=236) +
    geom_tile(show.legend = FALSE, inherit.aes = FALSE, na.rm = TRUE,
              aes(x = y, y = x, fill = z2, alpha = 1/20)) +
    # geom_raster(show.legend = FALSE, inherit.aes = FALSE, na.rm = TRUE,
    #           aes(alpha = 1/15)) +
    scale_fill_gradientn(colours=rev(c("blue1",'lightblue', "white" ,'orangered', "red1")), na.value=NA) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA)
    )
  plot

  height = dim(image)[1]*2 + ((dim(image)[1]*2)-((dim(image)[1]*2)*.95))
  width = dim(image)[2]*2 + ((dim(image)[2]*2)-((dim(image)[2]*2)*.90))

  png(here::here(path,paste0(name,"_heatmap.png")),
      height = height, width = width, res = 300, units = "px")
  plot
  dev.off()

  # setwd(wd)
  rm(list = ls())

}