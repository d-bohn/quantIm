from imutils import face_utils
import numpy as np
import argparse
import imutils
import dlib
import cv2
import rpy2.robjects.numpy2ri

## DEBUG START ##
#shape_predictor = '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata/shape_predictor_68_face_landmarks.dat'
#img = '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata/nm0000100_rm46373120_1955-1-6_2003.jpg'
## DEBUG END ##

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-p", "--shape-predictor", required=True,
	help="path to facial landmark predictor")
ap.add_argument("-i", "--image", required=True,
	help="path to input image")
args = vars(ap.parse_args())

# initialize dlib's face detector (HOG-based) and then create
# the facial landmark predictor
detector = dlib.get_frontal_face_detector()
predictor = dlib.shape_predictor(args["shape_predictor"])
#predictor = dlib.shape_predictor(shape_predictor)

# load the input image, resize it, and convert it to grayscale
image = cv2.imread(args["image"])
#image = cv2.imread(img)
#image = imutils.resize(image, width=500)
gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

# detect faces in the grayscale image
rects = detector(gray, 1)

# loop over the face detections
for (i, rect) in enumerate(rects):
	# determine the facial landmarks for the face region, then
	# convert the facial landmark (x, y)-coordinates to a NumPy
	# array
	shape = predictor(gray, rect)
	shape = face_utils.shape_to_np(shape)

	# convert dlib's rectangle to a OpenCV-style bounding box
	# [i.e., (x, y, w, h)], then draw the face bounding box
	bb = face_utils.rect_to_bb(rect)

import rpy2.robjects as ro
import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

nr,nc = shape.shape
#br,bc = bb.shape

shapeR = ro.r.matrix(shape, nrow=nr, ncol=nc)

ro.r.assign("shapeR", shapeR)
#rect = shape[1]
rect = shapeR[1]
for rect in shape:
	print("{} {}".format(*rect))
#print(ro.r["shapeR"])
print("{} {} {} {}".format(*bb))
