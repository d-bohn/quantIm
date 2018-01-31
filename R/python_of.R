#' Compute optical flow between two images
#'
#' @param reference
#' @param target
#' @param output
#' @param python
#'
#' @return
#'
#' @examples
python_of <- function(reference, target, output, save, python=NULL){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    target = here::here('inst/extdata/S001.Post.Anger.jpg')
    reference = here::here('inst/extdata/S001.Pre.Anger.jpg')
    output = here::here('test')
    save = stringr::str_extract(target, 'S\\d{3}')
    python = NULL
    ### END DEBUGGING NOT RUN
  }

  py_loc <- quantIm::find_python(python)

  ## Try to get try to get the script form the inst/python folder
  script <- system.file("python", "python_of.py", package = "quantIm")
  command1 <- paste(py_loc, script,
                    '-r', reference,
                    '-t', target,
                    '-o', output,
                    '-s', save,
                    '-v',
                    sep = ' ')

  try(
    system(command1, intern = TRUE),
    silent = TRUE
  )


    }
