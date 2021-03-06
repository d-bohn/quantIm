#' Obtain Various Distance Metrics Between Two Images
#'
#' @param target Path to an image.
#' @param reference Path to an image to be used as a reference
#' @param save_id Regular expression to extract ID from file name. Defaults
#' to full path provided for file if \code{NULL}.
#' @param to_file Logical. Should results be written to file or printed in console?
#'
#' @return Returns dataframe with various distance metrics. See details for
#' which distance measures are includes.
#'
#' @export
#'
#' @importFrom tibble tribble
#' @import dtwclust
#' @importFrom dtwclust dtw_lb
#' @importFrom TSdist FourierDistance
#' @importFrom RNiftyReg similarity
#' @importFrom reticulate import
#' @importFrom EBImage readImage
#' @importFrom tools file_path_sans_ext
#'
#' @examples
distance_metrics <- function(target, reference, save_id = NULL, to_file = TRUE){

  # Read them images, gurl
  if (is.null(save_id)==TRUE){
    t_name <- tools::file_path_sans_ext(target)
    r_name <- tools::file_path_sans_ext(reference)
  } else{
    t_name <- gsub(save_id, '', target)
    r_name <- gsub(save_id, '', reference)
  }

  target <- EBImage::readImage(target)
  reference <- EBImage::readImage(reference)

  # Get measures from matrix images
  ## Dip into python for a tad
  ski <- reticulate::import('skimage.measure')
  np <- reticulate::import('numpy')
  scipy.euc <- reticulate::import('scipy.spatial.distance')
  # fastdtw <- reticulate::import('fastdtw')
  cv <- reticulate::import('cv2')

  nrmse <- ski$compare_nrmse(target@.Data,reference@.Data)
  psnr <- ski$compare_psnr(target@.Data,reference@.Data)
  ssim <- ski$compare_ssim(target@.Data,reference@.Data)

  # target_py <- reticulate::np_array(target@.Data, dtype = 'float32')
  # hw <- target_py$shape
  # cv$CreateMat(hw, cv$CV_32FC3)
  # vis2 = cv$cvtColor(target_py, cv$COLOR_GRAY2BGR)
  # reference_py <- reticulate::np_array(reference@.Data)
  # emd <- cv$EMD(target_py,reference_py, distType = cv$DIST_L1)

  ## Borrow from fMRI
  sim_reg <- RNiftyReg::similarity(target,reference)
  dtw <- dtw::dtw(target@.Data, reference@.Data, dist.method = "Manhattan")[['distance']]
  dtwN <- dtw::dtw(target@.Data, reference@.Data, dist.method = "Manhattan")[['normalizedDistance']]

  ## C-c-c'mon, Morty, Get measures from the long form data, Morty
  target_c <- c(target@.Data)
  reference_c <- c(reference@.Data)

  # dtw <- TSdist::DTWDistance(target_c, reference_c, sigma=30)
  #
  # if (!('dtwclust' %in% loadedNamespaces())){
  #   require(dtwclust)
  # }
  #
  # dtw <- dtwclust::dtw_basic(target_c, reference_c, window.size = 512L)
  # dtwlb <- dtwclust::dtw_lb(target_c, reference_c, window.size = 512L)
  #
  # fourier <- TSdist::FourierDistance(target_c,reference_c)

  data <- tibble::tribble(## Columns
    ~target, ~reference, ~nrmse, ~psnr, ~ssim, ~sim_reg, ~dtw,~dtwN,
    ## Values
    t_name, r_name, nrmse, psnr, ssim, sim_reg, dtw, dtwN)

  if (to_file==TRUE) {
    if(!file.exists(here::here('distance_metrics.csv'))) {
      write.table(data, 'distance_metrics.csv', row.names = FALSE, sep = ',')
      message('Distance metrics file create at: ', here::here())
      message(t_name,' written to file')
    } else {
      write.table(data, 'distance_metrics.csv', row.names = FALSE, col.names = FALSE,
                  sep = ',', append = TRUE)
      message(t_name,' written to file')
    }
  } else {
    return(data)
  }

}
