
library(quantIm)

coords <- face_coord(image = 'data/defiant2.jpg')
coords <- as.data.frame(coords)
image <- 'data/defiant2.jpg'
face_crop(coords, image, savename = 'data/old_woman_cropped.png')
image_gleamed <- gleam(image = 'data/old_woman_cropped.png', tau = .6, show = FALSE)

library(quantIm)
setwd('~/Desktop/test')
files <- list.files('images/')
batch_crop(files, path = 'images/', save_string = 'test_crop_', gleam=TRUE)
