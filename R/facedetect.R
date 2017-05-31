library(reticulate)

# reticulate::use_python("/usr/local/Cellar/python/2.7.13/bin/python", required=TRUE)
# reticulate::use_python("/usr/local/bin/python", required=TRUE)

cv2 <- import('cv2', convert = FALSE)
dlib <- import('dlib', convert = FALSE)
face_utils <- import('imutils.face_utils')
imutils <- import('imutils')
np <- import('numpy', convert=FALSE)
scipy <- import('scipy.misc')
main <- import_main()
py <- import_builtins()

## DEBUG START ##
shape_predictor = 'data/shape_predictor_68_face_landmarks.dat'
img = 'data/defiant2.jpg'
## DEBUG END ##

face_detect <- function(img,
                        shape_predictor = system.file('data', 'shape_predictor_68_face_landmarks.dat',
                                                      package = 'quantIm')){

  # initialize dlib's face detector (HOG-based) and then create
  # the facial landmark predictor
  detector = dlib$get_frontal_face_detector()
  predictor = dlib$shape_predictor(shape_predictor)

  # load the input image, resize it, and convert it to grayscale
  # image = cv2$imread(img,cv2$IMREAD_GRAYSCALE)
  image = cv2$imread(img)
  gray = cv2$cvtColor(image, cv2$COLOR_BGR2GRAY)
  # width=dim(image)[1]
  # height=dim(image)[2]
  # channels=1
  # dims = r_to_py(c(width,height))
  # im = r_to_py(image)
  # im_array <- r_to_py(array(image))
  # im$shape
  # reticulate::py_set_attr(im, 'shape', list(1300,1053))
  # reticulate::py_list_attributes(im)

  # detect faces in the grayscale image
  reticulate::py_set_attr(detector,'upscale',1.2)
  rects = detector$run(gray)

  # determine the facial landmarks for the face region, then
  # convert the facial landmark (x, y)-coordinates to a NumPy
  # array
  zz = py$enumerate(rects)
  qq = iterate(rects, simplify = FALSE)

  shape = predictor$save(gray, rects)
  shape = face_utils$shape_to_np(shape)

  # convert dlib's rectangle to a OpenCV-style bounding box
  # [i.e., (x, y, w, h)], then draw the face bounding box
  xywh <- c('x', 'y', 'w', 'h')
  xywh_py <- reticulate::r_to_py(xywh)
  xywh_py = face_utils$rect_to_bb(rects)

}
