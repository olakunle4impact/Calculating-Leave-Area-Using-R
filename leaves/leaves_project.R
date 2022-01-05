#install.packages("raster")
#install.packages("wvtool")
#install.packages("stringr")
library(raster)
library(wvtool)
library(stringr)
setwd("D:/personal/Joshua/scan_of_leaves") #sets the working directory
resolution <- 400 #dpi
pixel_size <- 2.54/resolution #pixel_size in cm
#listing all files
all_imgs <- list.files(pattern="*.tif$",full.names = F)

leaf_area_calculator <- function(file){
  #file=all_imgs[3]
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

#creating the dataframe to store the output:
output <- data.frame(id=character(),
                     sample_number=numeric(),
                     year=numeric(),
                     replicate=numeric(),
                     n_pixel_leaves=numeric(),
                     n_pixel_blank=numeric(),
                     leaf_area=numeric(),
                     blank_area=numeric()
          )
output$id <- as.character(output$id)

for(i in all_imgs){
  print(i)
  #i=all_imgs[1]
  i_short <- str_sub(i, 1, -5)
  i_short_split <- unlist(strsplit(i_short, "_"))
  result <- leaf_area_calculator(i)
  
  output <- rbind(output,setNames(as.list(c(i_short,i_short_split[1],i_short_split[2], i_short_split[3],
                                             result[1], result[2], result[3], result[4])),
                                    names(output)), stringsAsFactors=F)
  
}
num_columns <- c(4:8)
output[num_columns] <- sapply(output[num_columns], as.numeric) #converting fields to numeric
str(output)

output$total_area <- NA

output$total_area <- output$leaf_area + output$blank_area

write.table(output, "output.csv", sep=",", row.names = F)
