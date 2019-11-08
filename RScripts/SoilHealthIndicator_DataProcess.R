
#*****************************************************************************************************************
# Step 1: Data prapration
#*****************************************************************************************************************


SoilHealth_sum <- read.csv('outputs/SoilHealth_Indicator_Sum.csv', header = T)

head(SoilHealth_sum)

# Add a final mean column: when na, use above hierarchy represent
# use for loop

SoilHealth_sum$Mean_R <- NA


for (i in 1:length(SoilHealth_sum$ID)) {
  # i = 1
  SoilHealth_sum[i,]$Mean_R <- if (!is.na(SoilHealth_sum[i,]$mean_Texture)) {SoilHealth_sum[i,]$mean_Texture}
  else if (is.na(SoilHealth_sum[i,]$mean_Texture) & !is.na(SoilHealth_sum[i,]$mean_CoverCrop)) {SoilHealth_sum[i,]$mean_CoverCrop}
  else if (is.na(SoilHealth_sum[i,]$mean_Texture) & is.na(SoilHealth_sum[i,]$mean_CoverCrop) & !is.na(SoilHealth_sum[i,]$mean_GrainCrop)){
    SoilHealth_sum[i,]$mean_GrainCrop
  }
  else if (is.na(SoilHealth_sum[i,]$mean_Texture) & is.na(SoilHealth_sum[i,]$mean_CoverCrop) & is.na(SoilHealth_sum[i,]$mean_GrainCrop)
           & !is.na(SoilHealth_sum[i,]$mean_Climate)){SoilHealth_sum[i,]$mean_Climate}
  
  else if (is.na(SoilHealth_sum[i,]$mean_Texture) & is.na(SoilHealth_sum[i,]$mean_CoverCrop) & is.na(SoilHealth_sum[i,]$mean_GrainCrop)
           & is.na(SoilHealth_sum[i,]$mean_Climate) & !is.na(SoilHealth_sum[i,]$mean_all)) {SoilHealth_sum[i,]$mean_all}
 
  print(paste("******",i))
}


# check NA results
length(SoilHealth_sum[is.na(SoilHealth_sum$Mean_R),1] )

head(SoilHealth_sum)


# whenever n <= 3, use average replace

SoilHealth_sum[(SoilHealth_sum$obs_Climate<=3 & SoilHealth_sum$obs_Climate>0)
               |(SoilHealth_sum$obs_GrainCrop<=3 & SoilHealth_sum$obs_GrainCrop>0)
               |(SoilHealth_sum$obs_CoverCrop<=3 & SoilHealth_sum$obs_CoverCrop>0)
               |(SoilHealth_sum$obs_Texture<=3 & SoilHealth_sum$obs_Texture>0),]$Mean_R <- 
  ( (SoilHealth_sum[(SoilHealth_sum$obs_Climate<=3 & SoilHealth_sum$obs_Climate>0)
                 |(SoilHealth_sum$obs_GrainCrop<=3 & SoilHealth_sum$obs_GrainCrop>0)
                 |(SoilHealth_sum$obs_CoverCrop<=3 & SoilHealth_sum$obs_CoverCrop>0)
                 |(SoilHealth_sum$obs_Texture<=3 & SoilHealth_sum$obs_Texture>0),]$mean_all) +
  (SoilHealth_sum[(SoilHealth_sum$obs_Climate<=3 & SoilHealth_sum$obs_Climate>0)
                  |(SoilHealth_sum$obs_GrainCrop<=3 & SoilHealth_sum$obs_GrainCrop>0)
                  |(SoilHealth_sum$obs_CoverCrop<=3 & SoilHealth_sum$obs_CoverCrop>0)
                  |(SoilHealth_sum$obs_Texture<=3 & SoilHealth_sum$obs_Texture>0),]$Mean_R) )/2
  


SoilHealth_sum[(SoilHealth_sum$Mean_R)>400,]$Mean_R <- SoilHealth_sum[(SoilHealth_sum$Mean_R)>400,]$mean_all

write.csv(SoilHealth_sum,"outputs/SoilHealth_Indicator_Sum_Process.csv", row.names = F)


#*****************************************************************************************************************
# pivot
#*****************************************************************************************************************

SoilHealth_sum <- read.csv("9.SoilHealth_Indicator_Sum_Process.csv")
  
colnames(SoilHealth_sum)
class(SoilHealth_sum$ID)

var_ID <- sort(unique(SoilHealth_sum$ID))
length(var_ID) * 448

# do first ID first
for (i in 1) {
  Reshape_data <- SoilHealth_sum[SoilHealth_sum$ID == var_ID[i],c(2,22,24)]
  var_mean <- paste(i, "-", unique(Reshape_data$Response), "_Mean", sep = "")
  
  colnames(Reshape_data) <- c("Response", "Code", var_mean)
  Reshape_data <- Reshape_data[,-1]
  Reshape_data <- Reshape_data[order(Reshape_data$Code),]
}


for (i in 2:length(var_ID)) {
  subdata <- SoilHealth_sum[SoilHealth_sum$ID == var_ID[i],c(2,22,24)]
  var_mean <- paste(i, "-", unique(subdata$Response), "_Mean", sep = "")
  
  colnames(subdata) <- c("Response", "Code", var_mean)
  subdata <- subdata[order(subdata$Code),]
  subdata <- subdata[3]
  subdata <- as.data.frame(subdata)
  colnames(subdata) <- var_mean
  
  Reshape_data <- cbind(Reshape_data, subdata)
}

head(Reshape_data)

write.csv(Reshape_data,"10.SoilHealth_Indicator_Sum_Reshape.csv", row.names = F)


#*****************************************************************************************************************
# END
#*****************************************************************************************************************


