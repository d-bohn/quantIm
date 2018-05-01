face_align_python <- function(target) {

  quantIm::find_python(condaenv = 'quantIm')

  dlib_shape <- system.file('extdata','shape_predictor_68_face_landmarks.dat', package = 'quantIm')

  script <- system.file('python','face_align.py', package = 'quantIm')

  command <- sprintf('python %s --shape-predictor %s --image %s', script, dlib_shape, target)

  np_array <- system(command, intern = TRUE)

  reticulate::py_to_r(np_array)

}
