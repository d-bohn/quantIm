# quantIm
An R package for the **quant**itative assessment of **im**ages.

***DISCLAIMER***
This package is currently in pre-pre-beta. It is a host of personal functions I wrote for specific projects that I find myself reusing over and over again. It is very much a work in progress. I have only tested it on UNIX systems.

## Quick Start
In order to use this package, a valid installation of the package manager software [conda](https://conda.io/) must be installed. `quantIm` sets up a specific enviornment for the package (much like RStudio's [`tensorflow`](https://github.com/rstudio/tensorflow), and will install all of the necessary packages for the various functions (mostly python packages).

    install.packages('devtools')
    devtools::install_github('d-bohn/quantIm')
    
    library(quantIm)
    install_quantIm()

## Package Motivation
The motivation to compile this package are entirely selfish. The `R` ecosystem has a number of excellent image manipulation packages (e.g., `imager`, `EBImage` and `magick`), however, they all do different things. This leaves the user to navigate different packages for specific tasks, running the risk of incompatible data structures and non-reproducibility. I wanted all of the functions I use in a single space for easier data analysis traversing.

`quantIm` attempts to provide convenient wrappers around some common functions data scientists and computer vision researchers might utilize often when manipulating/assessing images. Additionally, with the implementation of RStudio's wonderful [`reticulate`](https://github.com/rstudio/reticulate) package, `quantIm` attempts to provide wrappers around some common computer vision and image functions written in python.

## Package Structure
The functions in `quantIm` can largely be broken into the following categories:

 - Image manipulation
    
    1. Padding images
    
    2. Subtracting two images
    
    3. "gleam"ing and image
 
 - Image analysis
 
    1. t/z-test between images
    
    2. Plot heat map of values
    
    3. Distance metrics between images
