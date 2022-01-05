library(raster)
library(stringr)
getwd()
setwd(dir="C:/Users/````/Documents/GIS DataBase/RTest/leaves")
resolution <- 400
pixel_size <- 2.54/resolution  ###pixel_size in cm
## 2.54 is one inch to centimeter ##

ls("package:base") ## to confirm the source of list.files function ##
ls("package:stringr")

#listing all files
all_imgs <- list.files(pattern="*.tif$",full.names = F)
print(all_imgs) ## The dollar sign means it should stop at imgs with .tiff ##


leaf_area_calculator <- function(file){
  #This function takes only one argument: the path to the file
  img <- raster(file) #reads the first band (R) of the image. R, G, B are similar and have 255 where there are leaves
  vec <- c(-1, 0, 1, 0, 1, 0) #creates a vector classes
  mat <- matrix(vec, ncol=3, byrow=TRUE) #transforms the previous vector to a matrix for reclassification
  img_recl <- reclassify(img, mat) #creates a reclassified image based on the previous matrix
  n_pixel_leaves <- length(img_recl[img_recl==1])
  n_pixel_blank <- length(img_recl[img_recl==0])
  leaf_area <- n_pixel_leaves*pixel_size^2
  blank_area <- n_pixel_blank*pixel_size^2
  return(c(n_pixel_leaves, n_pixel_blank, leaf_area, blank_area))
  #plot(img_recl)
  }
