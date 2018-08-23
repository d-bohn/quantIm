#' Create heatmap image from t-values
#'
#' @param tz_file Path to file of t-values.
#' @param base_image Relative path to base image file (in \code{.jpg} or \code{.png} format).
#' @param mask Relative path to mask image file (in \code{.jpg} or \code{.png} format). Defaults
#' to \code{NULL}.
#' @param absval Logical. Should the absolute value of the z/t scores be taken?
#' @param thresh The cuttoff score for significance. Defaults to p = .05, or 1.96.
#'
#' @return Returns a \code{ggplot2} object that can be further manipulated using
#' \code{ggplot2} functions, or to be saved.
#'
#' @details If saving graph, make sure to save the image with the exact width and
#' height of the original images, else the heatmap values will not be placed correctly.
#'
#' @export
#' @import ggplot2 grid
#' @importFrom RSAGA grid.to.xyz
#' @importFrom EBImage readImage
#' @importFrom data.table fread
#' @importFrom tools file_ext
#' @importFrom grid rasterGrob
#'
#' @examples

im_heatmap <- function(tz_file, base_image, mask, thresh = 1.96, absval = FALSE){

  is_df <- length(dim(tz_file))

  # Read in t/z file ----
  # If file supplied is NOT a data frame
  if (hasArg(tz_file) && is.null(dim(is_df))) {
    path <- dirname(tz_file)
    z <- basename(tools::file_path_sans_ext(tz_file))
    savename <- gsub('_average_[t|z]_values', '', z)
    # Make it a df
    df <- data.table::fread(tz_file)

    # If it is already, great!
  } else if (hasArg(tz_file) && !is.null(is_df)) {
    path <- getwd()
    savename <- 'average_'
    df <- tz_file
  } else {
    stop('Please supply valid file or name')
  }

  # Read in base image ----
  if (isTRUE(hasArg(base_image))) {
    image <- quantIm:::read_image(base_image)
    img <- grid::rasterGrob(image, interpolate = TRUE)
  } else {
    stop('Please supply either a .jpg or .png picture file.')
  }

  # Read in mask ----
  if (isTRUE(hasArg(mask))) {
    mask_im <- EBImage::readImage(mask)
    mask_mat <- EBImage::imageData(mask_im)
  } else if (isFALSE(hasArg(mask))) {
    mask_im <- 1
  } else {
    stop('Please supply either a .jpg or .png picture file.')
  }

  mat <- data.matrix(df)

  if (isTRUE(hasArg(mask)) & dim(mask_mat)[3] == 2 | is.na(dim(mask_mat)[3])) {
    mat2 <- mat*mask_mat
  } else if (isTRUE(hasArg(mask)) & dim(mask_mat)[3] == 3) {
    mask_mat2 <- mask_mat[,,1]
    mat2 <- mat*mask_mat2
  } else {
    mat2 <- mat
  }

  # Prepare for plotting ----
  mat_rotate <- RSAGA::grid.to.xyz(mat2)
  data <- as.data.frame(mat_rotate)

  data2 <- data

  data2$z2 <- ifelse(abs(data2$z) <= thresh, NA, data2$z)

  g <- grid::rasterGrob(image)

  # Plot and plot function ----
  ggheatmap <- function(data, absval) {
    if (!absval) {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = y, y = x, fill = z2)) +
        ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf,
                                   ymin=-Inf, ymax=Inf) +
        ggplot2::xlim(0,NA) +
        ggplot2::geom_tile(show.legend = FALSE, inherit.aes = FALSE, na.rm = TRUE,
                           ggplot2::aes(x = y, y = x, fill = z2, alpha = 1/20)) +
        ggplot2::scale_fill_gradientn(colours=c("blue1",'lightblue', "white" ,'orangered', "red1"),
                                      na.value=NA) +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
    } else if (absval) {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = y, y = x, fill = abs(z2))) +
        ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf,
                                   ymin=-Inf, ymax=Inf) +
        ggplot2::xlim(0,NA) +
        ggplot2::geom_tile(show.legend = FALSE, inherit.aes = FALSE, na.rm = TRUE,
                           ggplot2::aes(x = y, y = x, fill = z2, alpha = 1/20)) +
        ggplot2::scale_fill_gradientn(colours=c("white" ,'orangered', "red1"),
                                      na.value=NA) +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
    }

    return(p)
  }

  plot <- ggheatmap(data = data2, absval = absval)

  # plot
  height = dim(image)[1]
  width = dim(image)[2]

  return(list(heatmap = plot, image = list(width = width, height = height)))
}
