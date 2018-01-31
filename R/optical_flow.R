optical_flow <- function(target, reference, options = 'default', vis=TRUE){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    target = here::here('inst/extdata/S001.Post.Anger.jpg')
    reference = here::here('inst/extdata/S001.Pre.Anger.jpg')
    output = here::here('test')
    save = stringr::str_extract(target, 'S\\d{3}')
    python = NULL
    options = 'default'
    ### END DEBUGGING NOT RUN
  }

  py_loc <- reticulate::use_python(quantIm::find_python(python), required = TRUE)
  reticulate::py_discover_config(required_module = 'pyflow')

  main <- reticulate::import_main()
  bi <- reticulate::import_builtins()
  np <- reticulate::import('numpy')
  pil <- reticulate::import('PIL.Image')
  pd <- reticulate::import('pandas')
  pyflow <- reticulate::import('pyflow', convert = FALSE)

  im1 = np$array(pil$open(reference))
  if( length(dim(im1)) != 3 ){
    im1 = np$expand_dims(im1, axis = 2L)
  }

  im2 = np$array(pil$open(target))
  if( length(dim(im2)) != 3 ){
    im2 = np$expand_dims(im2, axis = 2L)
  }

  im1 = im1 / 255L
  im2 = im2 / 255L

  im1_py <- reticulate::r_to_py(im1)
  im2_py <- reticulate::r_to_py(im2)

  im1_py = im1_py$copy(order='C')
  im2_py = im2_py$copy(order='C')
  # reticulate::array_reshape(im1_py, c(480,428), order = 'C')

  default_options <- list(
    # Flow Options:
    alpha = 0.012,
    ratio = 0.75,
    minWidth = 20L,
    nOuterFPIterations = 7L,
    nInnerFPIterations = 1L,
    nSORIterations = 30L,
    colType = 1L  # 0 or default:RGB, 1:GRAY (but pass gray image with shape (h,w,1))
  )

  option_names <- c('alpha','ratio','minWidth','nOuterFPIterations','nInnerFPIterations',
                    'nSORIterations','colType')

  if (class(options) != 'list' && options == 'default'){

    invisible(
      lapply(seq_along(default_options),
           function(x) {
             assign(option_names[x], default_options[[x]], envir=.GlobalEnv)
           }
           )
      )

  } else{

    for (i in seq_along(option_names)){
      if (option_names[[i]] %in% names(options)){
        default_options[[option_names[[i]]]] <- options[[option_names[[i]]]]
      } else{
        default_options[[option_names[[i]]]] <- default_options[[option_names[[i]]]]
      }
    }
    invisible(
      lapply(seq_along(default_options),
             function(x) {
               assign(option_names[x], default_options[[x]], envir=.GlobalEnv)
             }
      )
    )
  }

  pyflowPy <- pyflow$coarse2fine_flow(
    im1_py, im2_py, alpha, ratio, minWidth,
    nOuterFPIterations, nInnerFPIterations,
    nSORIterations, colType)

  pyflowR <- reticulate::py_to_r(pyflowPy)
  flowR <- abind::abind(pyflowR[[1]],pyflowR[[2]], along = 3)
  im2W <- pyflowR[[3]]
  # rotate <- function(x) t(apply(x, 2, rev)

  np$save(paste0(output,'/',save,'.npy'),
          reticulate::r_to_py(flowR))

  if ( viz == TRUE){

    script <- system.file("python", "vis_of.py", package = "quantIm")
    flow <-  paste0(output,'/',save,'.npy')
    command1 <- paste(py_loc, script,
                      '-r', reference,
                      '-s', save,
                      '-o', output,
                      '-f', flow,
                      sep = ' ')

    try(
      system(command1, intern = TRUE),
      silent = TRUE
    )

  }

  flow_list <- list(flow=pyflowR, flowR = florR, warped_image = im2W)
  return(flow_list)

}




# show_of <- function(flow,im2w){
#   cv2 <- reticulate::import('cv2')
#   hsv = np$zeros(im1_py$shape, dtype=np$uint8)
#   hsv2 <- r_to_py(hsv)
#   hsv[, , 1] = 255
#   flowR_Py <- reticulate::r_to_py(flowR)
#   mag_ang = cv2$cartToPolar(flowR[,1,], flowR[,2,])
#   hsv[,,1] = mag_ang[[2]] * 180 / np$pi / 2
#
#   cv2$imwrite(paste0(output,'/',save,'_flow.png'), hsv2)
# }

