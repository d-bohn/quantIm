#' Install quantIm package dependencies
#'
#' @return
#' @export
#'
#' @examples

install_quantIm <- function(){

  paks <- c('python=3','imutils', 'numpy', 'scipy', 'matplotlib', 'dlib',' scikit-image',
            'opencv', 'Pillow', 'boost')

  paks2 <- c('git+https://github.com/garydoranjr/pyemd.git#egg=pyemd',
             'git+https://github.com/pathak22/pyflow.git#egg=pyflow')

  condaInst <- conda_binary()

  if (length(condaInst) != 0){
    condaList <- conda_list()
    if(!('quantIm' %in% condaList$name)){

      conda_create(envname = 'quantIm', packages = paks,
                    conda = condaInst)

      conda_install('quantIm', paks2, pip=TRUE)

    }

    else stop('quantIm already installed.')

  } else{

    stop('You must have a valid instalation of conda in order to')
    stop('install quantIm on this system.')
}

}
