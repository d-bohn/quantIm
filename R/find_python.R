#' Helper function to find python on user system
#'
#' @param python
#'
#' @return
#' @export
#'
#' @examples
find_python <- function(python){
  if(is.null(python)){
    python_hb <- suppressWarnings( system('which python2', intern = TRUE) )
    if(!is.null(attr(python_hb,'status'))){
      py_locT <- system("which python", intern=TRUE)
      message('The python version this system is using is located at: ', py_loc)
      yn1 <- readline('Is this correct? (Y, N): ')
      if ((yn1 == 'n' | yn1 == 'N' | yn1 == 'NO' | yn1 == 'no' | yn1 == 'No') == TRUE) {
        py_locU <- readline('Please provide desired python location executable: ')
        py_loc <- py_locU
        reticulate::use_python(py_locU)
      } else {
        py_loc <- py_locT
        reticulate::use_python(py_locT)
      }
    } else{
      py_loc <- python_hb
      reticulate::use_python(python_hb)
    }
  } else {
    py_loc <- python
    reticulate::use_python(python)
  }
  return(py_loc)
}
