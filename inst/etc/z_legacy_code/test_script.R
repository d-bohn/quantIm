
# ### START DEBUGGING NOT RUN ##
# setwd('/Volumes/Seagate Expansion Drive/machine_learning/face_image_pixels/')
# file_list <- list.files('images/')
# path = 'images/'; save_string = '_cropped'
# gleam=TRUE; i=4; resize = .4
# ### END DEBUGGING NOT RUN ##

library(quantIm)
setwd('/Volumes/Seagate Expansion Drive/machine_learning/face_image_pixels/')
file_list <- list.files('images/')
batch_crop(file_list, path = 'images/', save_string = '_crop', scale = .4, gleam=TRUE)
