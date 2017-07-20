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
#' @details The following distance metrics are computed:
#' nrmse= Normalized root mean squred error from python \code{skimage.measure}
#' module (must be installed prior to use),
#' psnr = Peak Signal-to-Noise ration from python \code{skimage.measure} module (must be installed priot to use),
#' ssim = Structural similarity index from python \code{skimage.measure} module (must be installed prior to use),
#' sim_reg = Similarity metric from \code{\link{RNiftyReg::similarity}},
#' dtw = Dynamic Time Warping distance from \code{\link{dtwclust::dtw_lb}},
#' fdtw = NULL,
#' \link{https://pypi.python.org/pypi/fastdtw} (must be installed prior to use),
#' fourier = Fast Fourier Transform distance from \code{\link{TSdist::FourierDistance}},
#' emd = Earth Mover's Distance computed from
#' \link{https://github.com/garydoranjr/pyemd} (must be installed prior to use),
#' dissim = NULL.
#'
#'All of the required python packages used for this function can be installed by running the
#'setup function \code{quantIm::initialize_quantIm()}. Note that this setup function only works for
#'UNIX based operating systems (not Windows).
#'
#' @export
#'
#' @examples
#'
#' @importFrom magrittr "%>%"
#' @importFrom tibble tribble
#' @import dtwclust
#' @importFrom dtwclust dtw_lb
#' @importFrom TSdist FourierDistance
#' @importFrom RNiftyReg similarity
#' @importFrom reticulate import
#' @importFrom EBImage readImage
#' @importFrom tools file_path_sans_ext
#'
distance_metrics <- function(target, reference, save_id = NULL, to_file = TRUE){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    save_id <- '^.*extdata/|*.png*$'
    target = here::here('inst/extdata/subject_nr_1002.png')
    reference = here::here('inst/extdata/include_total.png')
    to_file = TRUE
    ### END DEBUGGING NOT RUN
  }

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
  fastdtw <- reticulate::import('fastdtw')
  EMD <- reticulate::import('emd')

  nrmse <- ski$compare_nrmse(target,reference)
  psnr <- ski$compare_psnr(target,reference)
  ssim <- ski$compare_ssim(target,reference)
  emd <- EMD$emd(target,reference)

  ## Borrow from fMRI
  sim_reg <- RNiftyReg::similarity(target,reference)

  # C-c-c'mon, Morty, Get measures from the long form data, Morty
  target_c <- c(target)
  reference_c <- c(reference)

  #fdtw <- unlist(fastdtw$fastdtw(target_c,reference_c, dist=scipy.euc$euclidean)[1])
  fdtw <- NA

  #dissim <- TSdist::DissimDistance(target_c,reference_c)
  dissim <- NA
  #dtw <- dtw::dtw(target_c,reference_c)
  #dtw2 <- rucrdtw::ucrdtw_vv(target_c, reference_c, .05)
  #dtw <- TSdist::DTWDistance(target_c, reference_c, sigma=30)
  if (require(dtwclust)==FALSE){
    require(dtwclust)
  }
  dtw <- dtwclust::dtw_lb(target_c,reference_c, window.size = 1000L)

  fourier <- TSdist::FourierDistance(target_c,reference_c)

  data <- tibble::tribble(~target, ~reference, ~nrmse, ~psnr, ~ssim, ~sim_reg, ~dtw, ~fdtw, ~dissim, ~fourier, ~emd,
                          t_name, r_name, nrmse, psnr, ssim, sim_reg, dtw, fdtw, dissim, fourier, emd)

  #data <- as.data.frame(data)

  if (to_file==TRUE){
    if(!file.exists(here::here('distance_metrics.csv'))){
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
