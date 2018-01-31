# quantIm
An R package for the **quant**itative assessment of **im**ages.

## Quick Start
In order to use this package, a valid installation of the package manager software [conda](https://conda.io/) must be installed. `quantIm` sets up a specific enviornment for the package (much like RStudio's [`tensorflow`](https://github.com/rstudio/tensorflow), and will install all of the necessary packages for the various functions (mostly python packages).

    install.packages('devtools')
    devtools::install_github('d-bohn/quantIm')
    
    library(quantIm)
    install_quantIm()

## Package Motivation
The motivation to compile this package are entirely selfish. The `R` ecosystem has a number of excellent image manipulation packages (e.g., `imager`, `EBImage` and `magick`), however, they all do different things. This leaves the user to navigate different packages for specific tasks, running the risk of incompatible data structures and non-reproducibility.

`quantIm` attempts to provide convenient wrappers around some common functions data scientists and computer vision researchers might utilize often when manipulating/assessing images. Additionally, with the implementation of RStudio's wonderful [`reticulate`](https://github.com/rstudio/reticulate) package, `quantIm` provides wrappers around some common computer vision functions written in python.

*Caveat*: Some of the functions in this package can be applied to all types of images, but the main focus of these functions is on manipulation of facial images. Thus, there are quite a few specialized functions for faces.

## Package Structure
The functions in `quantIm` can largely be broken into the following categories:

 - Image manipulation
 
 - Image analysis
 
 ### Image Manipulation
 
 ### Image Analysis
