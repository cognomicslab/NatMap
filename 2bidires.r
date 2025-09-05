# purpose: calculate the indicators of BDR and combine all the indicators together
library(BiDimRegression)
library(dplyr)

# set working directory
setwd("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/scripts")

# laosheng_s1
data <- read.csv("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s1/bidi_raw.csv")
results <- data.frame()

data$depV1 <- data$xloc
data$depV2 <- data$yloc

observations <- 22 #change the number according to your data
participants <- nrow(data)/observations

# Loop through each participant's data set, run it through the bidimensional regression,
# Save the data to the results. 
for (i in 1:nrow(data)){
  if (i%%observations==0){
    # Select the data to be analyzed
    tempData <- data[c((i-(observations-1)):i),]
    id <- tempData[1,'Name']
    results[i/observations,'Name'] <- as.character(id)
    
    tempData <- tempData[,c('depV1','depV2','xorig','yorig')]
    
    # If any of the data are null / not recorded, exclude that participant's data.
    if (any(is.na(tempData))) {next}
    
    tempData$indepV1 <- tempData$xorig
    tempData$indepV2 <- tempData$yorig
    
    tempData <- tempData[,c('depV1','depV2','indepV1','indepV2')]
    
    temp_results <- BiDimRegression(tempData)
    
    results[i/observations,'r'] <- temp_results$euclidean.r
    results[i/observations,'rsquared'] <- temp_results$euclidean.rsqr
    results[i/observations,'x_scale'] <- temp_results$euclidean.scaleFactorX
    results[i/observations,'y_scale'] <- temp_results$euclidean.scaleFactorY
    results[i/observations,'angle'] <- temp_results$euclidean.angleDEG
    results[i/observations,'alpha1'] <- temp_results$euclidean.alpha1.coeff
    results[i/observations,'alpha2'] <- temp_results$affine.alpha2.coeff
    results[i/observations,'distortion_index'] <- temp_results$euclidean.diABSqr
    
  }
}
write.csv(results,"/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s1/BidiOut.csv", row.names = FALSE)

# summary(results[ , -1])

# change of indexes
results$Alpha1 <- abs(results$alpha1)
results$Alpha2 <- abs(results$alpha2)
results$Scale <- abs(1 - results$x_scale)
results$Angle <- cos(results$angle)
summary(results[ , -1])

# merge data
D <- read.csv("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s1/cal_dist.csv")
index_all <- merge(D, results, by = "Name", all = TRUE)
write.csv(index_all,"/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s1/index_all.csv", row.names = FALSE)


# laosheng_s2
data <- read.csv("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s2/bidi_raw.csv")
results <- data.frame()

data$depV1 <- data$xloc
data$depV2 <- data$yloc

observations <- 22 #change the number according to your data
participants <- nrow(data)/observations

# Loop through each participant's data set, run it through the bidimensional regression,
# Save the data to the results. 
for (i in 1:nrow(data)){
  if (i%%observations==0){
    # Select the data to be analyzed
    tempData <- data[c((i-(observations-1)):i),]
    id <- tempData[1,'Name']
    results[i/observations,'Name'] <- as.character(id)
    
    tempData <- tempData[,c('depV1','depV2','xorig','yorig')]
    
    # If any of the data are null / not recorded, exclude that participant's data.
    if (any(is.na(tempData))) {next}
    
    tempData$indepV1 <- tempData$xorig
    tempData$indepV2 <- tempData$yorig
    
    tempData <- tempData[,c('depV1','depV2','indepV1','indepV2')]
    
    temp_results <- BiDimRegression(tempData)
    
    results[i/observations,'r'] <- temp_results$euclidean.r
    results[i/observations,'rsquared'] <- temp_results$euclidean.rsqr
    results[i/observations,'x_scale'] <- temp_results$euclidean.scaleFactorX
    results[i/observations,'y_scale'] <- temp_results$euclidean.scaleFactorY
    results[i/observations,'angle'] <- temp_results$euclidean.angleDEG
    results[i/observations,'alpha1'] <- temp_results$euclidean.alpha1.coeff
    results[i/observations,'alpha2'] <- temp_results$affine.alpha2.coeff
    results[i/observations,'distortion_index'] <- temp_results$euclidean.diABSqr
    
  }
}
write.csv(results,"/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s2/BidiOut.csv", row.names = FALSE)

# summary(results[ , -1])

# change of indexes
results$Alpha1 <- abs(results$alpha1)
results$Alpha2 <- abs(results$alpha2)
results$Scale <- abs(1 - results$x_scale)
results$Angle <- cos(results$angle)
summary(results[ , -1])

# merge data
D <- read.csv("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s2/cal_dist.csv")
index_all <- merge(D, results, by = "Name", all = TRUE)
write.csv(index_all,"/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/laosheng_s2/index_all.csv", row.names = FALSE)

# freshman_s1
data <- read.csv("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/freshman/bidi_raw.csv")
results <- data.frame()

data$depV1 <- data$xloc
data$depV2 <- data$yloc

observations <- 22 #change the number according to your data
participants <- nrow(data)/observations

# Loop through each participant's data set, run it through the bidimensional regression,
# Save the data to the results. 
for (i in 1:nrow(data)){
  if (i%%observations==0){
    # Select the data to be analyzed
    tempData <- data[c((i-(observations-1)):i),]
    id <- tempData[1,'Name']
    results[i/observations,'Name'] <- as.character(id)
    
    tempData <- tempData[,c('depV1','depV2','xorig','yorig')]
    
    # If any of the data are null / not recorded, exclude that participant's data.
    if (any(is.na(tempData))) {next}
    
    tempData$indepV1 <- tempData$xorig
    tempData$indepV2 <- tempData$yorig
    
    tempData <- tempData[,c('depV1','depV2','indepV1','indepV2')]
    
    temp_results <- BiDimRegression(tempData)
    
    results[i/observations,'r'] <- temp_results$euclidean.r
    results[i/observations,'rsquared'] <- temp_results$euclidean.rsqr
    results[i/observations,'x_scale'] <- temp_results$euclidean.scaleFactorX
    results[i/observations,'y_scale'] <- temp_results$euclidean.scaleFactorY
    results[i/observations,'angle'] <- temp_results$euclidean.angleDEG
    results[i/observations,'alpha1'] <- temp_results$euclidean.alpha1.coeff
    results[i/observations,'alpha2'] <- temp_results$affine.alpha2.coeff
    results[i/observations,'distortion_index'] <- temp_results$euclidean.diABSqr
    
  }
}
write.csv(results,"/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/freshman/BidiOut.csv", row.names = FALSE)

# summary(results[ , -1])

# change of indexes
results$Alpha1 <- abs(results$alpha1)
results$Alpha2 <- abs(results$alpha2)
results$Scale <- abs(1 - results$x_scale)
results$Angle <- cos(results$angle)
summary(results[ , -1])

# merge data
D <- read.csv("/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/freshman/cal_dist.csv")
index_all <- merge(D, results, by = "Name", all = TRUE)
write.csv(index_all,"/bigvault/Projects/sea_hero/LOCdata/article/Remap_article/results/data/freshman/index_all.csv", row.names = FALSE)
