#library(quantIm)

# ---- Start debug im_sub ----

image1 <- files1[[1]]
image2 <- files2[[1]]
save_image <- FALSE
write_data <- FALSE
blur <- 0.3

# End Debug im_sub

# ---- Start debug batch_im_sub ----

files2 <- list.files('/Volumes/UNTITLED/emotion_residue/MI_M/MI_M_Pics/test_imgs',
                     pattern = '*_pre.png', full.names = TRUE)
files1 <- list.files('/Volumes/UNTITLED/emotion_residue/MI_M/MI_M_Pics/test_imgs',
                     pattern = '*_pt.png', full.names = TRUE)
blur <- 0.3

z_df <- batch_im_sub(files1 = files2, files2 = files1,
                      save_image = TRUE,
                      write_data = TRUE,
                      blur = blur)

# End Debug batch_im_sub

# ---- Start debug face_align ----

target <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/S001.Post.Anger2.png'
reference <- '/Volumes/UNTITLED/emotion_residue/PrPt_Exp/pics/S001.Pre.Anger2.png'
scope <- 'nonlinear'

# End Debug face_align

# ---- Start debug im_t/z_test ----

# data <- z_df
folder_path <- '/Volumes/UNTITLED/emotion_residue/MI_M/MI_M_Pics/test_imgs/subtracted/'
write <- TRUE
pattern1 <- '*.csv'
name <- 'post-pre'

z_vals <- im_z_test(path = folder_path, write = write, pattern1 = pattern1, name = name)

# End debug im_t_test

# ---- Start debug im_heatmap ----
base <- '/Users/dalbohn/Documents/zblack_hole/emtion_residue_images/prptex/anger/'
gleam_folder <- file.path(base, 'gleamed')

p1 <- im_heatmap(
   tz_file = file.path(gleam_folder,'subtracted/post-pre_average_z_values.csv'),
   base_image = file.path(base,'prptex_anger_average.png'),
   mask = file.path(base,'prptex_anger_mask.png'),
   thresh = 3.090,
   absval = TRUE
)
p1$heatmap

# End Debug im_heatmap
