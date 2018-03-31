#' Align two images
#'
#' @param target Path to target image
#' @param reference Path to reference image (what to alignt to)
#' @param scope Perform a 'linear' or 'nonlinear' transformation (see \link[RNiftyReg]{niftyreg})
#' @param save Logical indicating if the aligned image should be saved. Defaults to \code{TRUE}
#' @param ... Optional arguments to pass to \link[RNiftyReg]{niftyreg}
#'
#' @return
#' @export
#'
#' @importFrom RNiftyReg niftyreg
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom EBImage readImage as.Image writeImage
#'
#' @examples
face_align <- function(target, reference, scope = c('linear', 'nonlinear'), save = TRUE, ...){

  ext <- tools::file_ext(target)
  output <- paste0(tools::file_path_sans_ext(target), '_aligned.', ext)

  target_im <- EBImage::readImage(target)@.Data
  reference_im <- EBImage::readImage(target)@.Data

  result <- RNiftyReg::niftyreg(reference_im, target_im, scope = 'affine', ...)
  if (scope == 'linear') {
    out <- result
  } else if (scope == 'nonlinear') {
    result_nl <- RNiftyReg::niftyreg(reference_im, target_im, scope = 'nonlinear',
                                     init = result$affine, ...)
    out <- result_nl
  }

  out_im <- EBImage::as.Image(out$image)

  if (save==TRUE){
    EBImage::writeImage(out_im, output)
  } else {
    return(out_im)
  }

}
