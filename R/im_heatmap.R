#' Create heatmap image from t-values
#'
#' @param tz_file Path to file of t-values.
#' @param base_image Relative path to base image file (in \code{.jpg} or \code{.png} format).
#' @param mask Relative path to mask image file (in \code{.jpg} or \code{.png} format). Defaults
#' to \code{NULL}.
#' @param thresh The cuttoff score for significance. Defaults to p = .05, or 1.96.
#' @param save Should the plot be saved? Defaults to \code{FALSE}
#'
#' @return Returns a \code{ggplot2} object that can be further manipulated using
#' \code{ggplot2} functions, or saved.
#'
#' @details If saving graph, make sure to save the image with the exact width and
#' height of the original images, else the heatmap values will not be placed correctly.
#' Specifying that the plot should be saved automatically extracts the width and height
#' of the original image.
#'
#' @export
#' @import ggplot2 grid
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom here here
#' @importFrom RSAGA grid.to.xyz
#' @importFrom EBImage readImage
#' @importFrom data.table fread
#' @importFrom tools file_ext
#' @importFrom grid rasterGrob
#'
#' @examples

im_heatmap <- function(tz_file, base_image, mask, thresh = 1.96){

  is_df <- length(dim(tz_file))

  if (hasArg(tz_file) && is.null(dim(is_df)) ){
    path <- dirname(tz_file)
    z <- basename(tools::file_path_sans_ext(tz_file))
    savename <- gsub('_average_[t|z]_values','', z)

    df <- data.table::fread(tz_file)
  } else if (hasArg(tz_file) && !is.null(is_df)) {
    path <- getwd()
    savename <- 'average_'
    df <- tz_file
  } else stop('Please supply valid file or name')

  if (hasArg(base_image)) ext <- tools::file_ext(base_image)

  if((hasArg(base_image) && ext == 'png') == TRUE) {
    image <- png::readPNG(base_image)
    img <- grid::rasterGrob(image, interpolate = TRUE)
  } else if ((hasArg(base_image) && ext=='jpg') == TRUE) {
    image <- jpeg::readJPEG(base_image)
    img <- grid::rasterGrob(image, interpolate = TRUE)
  } else {
    stop('Please supply either a .jpg or .png picture file.')
  }

  if(hasArg(mask)) mext <- tools::file_ext(mask)

  if (hasArg(mask)){
    if((mext == 'png') == TRUE){
      mask_im <- EBImage::readImage(mask)
    } else if ((mext=='jpg')==TRUE){
      mask_im <- EBImage::readImage(mask)
    } else {
      stop('Please supply either a .jpg or .png mask file.')
    }
  }

  mat <- data.matrix(df)

  if (hasArg(mask)) {
    mat2 <- mat*mask_im
  } else {
    mat2 <- mat
  }


  # rotate <- function(x) t(apply(x, 2, rev))
  # mat3 <- rotate(rotate(mat2))
  #mat3 <- Thermimage::rotate90.matrix(mat2)
  mat3 <- mat2

  mat_rotate <- RSAGA::grid.to.xyz(mat3)
  data <- as.data.frame(mat_rotate)

  data2 <- data

  data2$z2 <- ifelse(abs(data2$z) <= thresh, NA, data2$z)

  g <- grid::rasterGrob(image)

  plot <- ggplot2::ggplot(data2, ggplot2::aes(x = y, y = x, fill = z2)) +
    # annotation_custom(rasterGrob(image, width=unit(1,"npc"), height=unit(1,"npc")),
    #                   -Inf, Inf, -Inf, Inf) +
    # ggplot2::annotation_custom(g, xmin=min(data2$y), xmax=max(data2$y),
    #                            ymin=min(data2$x), ymax=max(data2$x)) +
    ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf,
                               ymin=-Inf, ymax=Inf) +
    # annotation_custom(img, xmin=0, xmax=236, ymin=0, ymax=236) +
    ggplot2::xlim(0,NA) +
    # ggplot2::ylim(0,NA) +
    ggplot2::geom_tile(show.legend = FALSE, inherit.aes = FALSE, na.rm = TRUE,
                       ggplot2::aes(x = y, y = x, fill = z2, alpha = 1/20)) +
    # geom_raster(show.legend = FALSE, inherit.aes = FALSE, na.rm = TRUE,
    #           aes(alpha = 1/15)) +
    # ggplot2::scale_fill_gradientn(colours=rev(c("blue1",'lightblue', "white" ,'orangered', "red1")),
    #                               na.value=NA) +
    ggplot2::scale_fill_gradientn(colours=c("blue1",'lightblue', "white" ,'orangered', "red1"),
                                  na.value=NA) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent",colour = NA)
    )
  # plot

  # height = dim(image)[1]*2 + ((dim(image)[1]*2)-((dim(image)[1]*2)*.95))
  # width = dim(image)[2]*2 + ((dim(image)[2]*2)-((dim(image)[2]*2)*.90))
  height = dim(image)[1]
  width = dim(image)[2]

  # if(save == TRUE){
  #   png(file.path(path,paste0(savename,"_heatmap.png")),
  #       height = height, width = width, res = 300, units = "px")
  #   plot
  #   dev.off()
  # }

  return(list(heatmap = plot, image = list(width = width, height = height)))
}

