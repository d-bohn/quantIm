initialize_quantIm <- function(){
  #password <- readLines('Please ender administrator password: ')

  installs <- c(
    'sudo apt-get install python-pip',
    'sudo apt-get install cmake',
    'sudo pip install imutils',
    'sudo apt-get install libboost-all-dev',
    'sudo pip install dlib'
  )

  session <- devtools::session_info()$platform$system

  if (session != 'x86_64, minw32') {
    for (i in seq_along(installs)){
      system(installs[i])
    }
  } else message('Please follow instructions for installing the required
               packages for Windows in the documentation.')
}



