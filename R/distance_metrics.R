distance_metrics <- function(target, reference){

  # ##### FOR DEBUGGING NOT RUN
  # target =
  # reference =
  # #### END DEBUGGING NOT RUN

  # Read them images, gurl
  target <- EBImage::readImage(target)
  reference <- EBImage::readImage(reference)

  # Get measures from matrix images
  ## Dip into python for a tad
  ski <- reticulate::import('skimage.measure')
  nrmse <- ski$compare_nrmse(target,reference)
  psnr <- ski$compare_psnr(target,reference)
  ssim <- ski$compare_ssim(target,reference)

  ## Borrow from fMRI
  sim_reg <- RNiftyReg::similarity(target,reference)

  # C-c-c'mon, Morty, Get measures from the long form data, Morty
  target_c <- c(target)
  reference_c <- c(reference)

  dissim <- TSdist::DissimDistance(target_c,reference_c)
  dtw <- TSdist::DTWDistance(target_c,reference_c)
  fourier <- TSdist::FourierDistance(target_c,reference_c)

}
