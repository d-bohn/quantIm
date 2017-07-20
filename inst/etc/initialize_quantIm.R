initialize_quantIm <- function(){

  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }

  os <- get_os()

  #session <- devtools::session_info()$platform$system
  #password <- readlines('Please ender administrator password: ')

  py_loc <- system("which python", intern=TRUE)
  message('The python version this system is using is located at ', py_loc)
  yn1 <- readline('Is this correct? (Y, N): ')

  if ((yn1 == 'n' | yn1 == 'N' | yn1 == 'NO' | yn1 == 'no' | yn1 == 'No') == TRUE) {
    py_loc <- readlines('Please provide desired python location executable: ')
    reticulate::use_python(py_loc)
  } else {
    reticulate::use_python(py_loc)
  }

  message('The following packages will attempt to install: ',
    'python-pip, cmake, imutils, libboost, dlib, fastdtw, pyemd, scikit-image, numpy, scipy, matplotlib'
  )

  if ((os=='osx')==TRUE){
    system('brew install pip')
    system('brew install cmake')
    system('brew install boost --with-python')
    system('brew install boost-python')
  }
  if ((os=='linux')==TRUE){
    system('apt-get install python-pip')
    system('apt-get install libboost-all-dev')
    system('apt-get install cmake')
  }

  system('pip install imutils')
  system('pip install dlib')
  system('pip install fastdtw')
  system('pip install -e git+https://github.com/garydoranjr/pyemd.git#egg=pyemd')
  system('pip install scikit-image')
  system('pip install --user numpy scipy matplotlib')

}



