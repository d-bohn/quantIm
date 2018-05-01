library(reticulate)

cv2 <- import('cv2', convert = FALSE)
dlib <- import('dlib', convert = FALSE)
face_utils <- import('imutils.face_utils')
imutils <- import('imutils')
np <- import('numpy')
math <- import('math')
main <- import_main()
py <- import_builtins()

## DEBUG START ##
shape_predictor = 'data/shape_predictor_68_face_landmarks.dat'
img = 'data/defiant2.jpg'
## DEBUG END ##

face_detect <- function(img){
  # initialize dlib's face detector (HOG-based) and then create
  # the facial landmark predictor
  detector = dlib$get_frontal_face_detector()
  predictor = dlib$shape_predictor(shape_predictor)

  # load the input image, resize it, and convert it to grayscale
  image = cv2$imread(img)
  #image = imutils.resize(image, width=500)
  gray = cv2$cvtColor(image, cv2$COLOR_BGR2GRAY)

  # detect faces in the grayscale image
  rects = detector$run(gray)

  py$enumerate(rects)
  # loop over the face detections
  for (i, rect) in enumerate(rects):
    # determine the facial landmarks for the face region, then
    # convert the facial landmark (x, y)-coordinates to a NumPy
    # array
    shape = predictor$save(gray, rects)
    shape = face_utils.shape_to_np(shape)

  # convert dlib's rectangle to a OpenCV-style bounding box
  # [i.e., (x, y, w, h)], then draw the face bounding box
  bb = face_utils.rect_to_bb(rect)


  rect = shape[1]
  for rect in shape:
    print("{} {}".format(*rect))
  print("{} {} {} {}".format(*bb))

  }

