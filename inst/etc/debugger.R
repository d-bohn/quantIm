#library(quantIm)

# ---- Start debug face_align ----

target <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/S001.Post.Anger2.png'
reference <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/S001.Pre.Anger2.png'
scope <- 'nonlinear'

# End Debug face_align

# ---- Start debug im_t_test ----
path <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/subtracted'
pattern1 <- '*.csv'
name <- 'post-pre'

# End debug im_t_test

# ---- Start debug im_heatmap ----
name <- 'post-pre'
file <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/subtracted/post-pre_average_t_values.csv'
base_image <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/average.png'
mask <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/average_mask.png'
thresh <- 2.5
# thresh = .001

# End Debug im_heatmap
