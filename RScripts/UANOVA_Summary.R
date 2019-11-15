
#*****************************************************************************************************************
# Step 1.1: Prepare data
#*****************************************************************************************************************

soilhealthData <- read.csv('outputs/SoilHealth_Indicator_Sum.csv', header = T)
attach(soilhealthData)

head(soilhealthData)

var_ID <- sort(unique(soilhealthData$ID))

for (i in 2){
# for (i in var_ID){
  subdata <- soilhealthData[soilhealthData$ID == i,]
  layer1Perc <- length(subdata[!is.na(subdata$mean_Climate),1])/336*100
  layer2Perc <- length(subdata[!is.na(subdata$mean_GrainCrop),1])/336*100
  layer3Perc <- length(subdata[!is.na(subdata$mean_CoverCrop),1])/336*100
  layer4Perc <- length(subdata[!is.na(subdata$mean_Texture),1])/336*100
  
  allsum <- cbind(layer1Perc,layer2Perc,layer3Perc,layer4Perc)
}


for (i in var_ID[-1]){
  subdata <- soilhealthData[soilhealthData$ID == i,]
  layer1Perc <- length(subdata[!is.na(subdata$mean_Climate),1])/336*100
  layer2Perc <- length(subdata[!is.na(subdata$mean_GrainCrop),1])/336*100
  layer3Perc <- length(subdata[!is.na(subdata$mean_CoverCrop),1])/336*100
  layer4Perc <- length(subdata[!is.na(subdata$mean_Texture),1])/336*100
  
  ithsum <- cbind(layer1Perc,layer2Perc,layer3Perc,layer4Perc)
  allsum <- rbind(allsum, ithsum)
  
}
