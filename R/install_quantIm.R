#' Install quantIm package dependencies
#'
#' @return
#' @export
#'
#' @importFrom reticulate conda_list conda_binary conda_create conda_install
#'
#' @examples

install_quantIm <- function(){

  paks <- c('python=3','imutils', 'numpy', 'scipy', 'matplotlib', 'dlib',' scikit-image',
            'opencv', 'Pillow', 'boost')

  paks2 <- c('git+https://github.com/garydoranjr/pyemd.git#egg=pyemd',
             'git+https://github.com/pathak22/pyflow.git#egg=pyflow')

  condaInst <- reticulate::conda_binary()

  if (length(condaInst) != 0){
    condaList <- reticulate::conda_list()
    if(!('quantIm' %in% condaList$name)){

      reticulate::conda_create(envname = 'quantIm',
                               packages = paks,
                               conda = condaInst)

      reticulate::conda_install('quantIm',
                                'opencv',
                                forge = TRUE)

      reticulate::conda_install('quantIm',
                                paks2,
                                pip=TRUE)

    }

    else stop('quantIm already installed.')

  } else{

    stop('You must have a valid instalation of conda in order to')
    stop('install quantIm on this system.')
}

}
