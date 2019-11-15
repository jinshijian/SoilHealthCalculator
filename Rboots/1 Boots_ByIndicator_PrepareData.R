
#*****************************************************************************************************************
# OC need some calculation before data process: Use both % and concentration data: only two study, so done in excel
# Step 1: Data prapration
#*****************************************************************************************************************

source("functions.R")
library(car)
# install.packages('agricolae')
library(agricolae)
# install.packages('multcompView')
library(multcompView)
library(bootstrap)
library("metafor")
library("meta" )

# aggregate function

SoilHealthCC <- read.csv('data/SoilHealthDB_V0.csv', header = T)
attach(SoilHealthCC)

# remove AFS, Orchard, and pasture
SoilHealthCC <- SoilHealthCC[SoilHealthCC$Conservation_Type != "AFS",]
SoilHealthCC$GrainCropGroup <- as.character(SoilHealthCC$GrainCropGroup)
unique(SoilHealthCC$GrainCropGroup)
SoilHealthCC <- SoilHealthCC[SoilHealthCC$GrainCropGroup != "Pasture", ]
SoilHealthCC <- SoilHealthCC[SoilHealthCC$GrainCropGroup != "Orchard", ]

head(SoilHealthCC)

SoilHealthCC[SoilHealthCC$StudyID==100,]


#*****************************************************************************************************************
# get columns number hold indicators
#*****************************************************************************************************************


# last meta infor column
which(colnames(SoilHealthCC) == 'CCTermination') # 63

colnames(SoilHealthCC[,c(1:which(colnames(SoilHealthCC) == 'CCTermination'))]) # all background information

# get all response columns

# first response infor column
which(colnames(SoilHealthCC) == 'BiomassCash_C')

# OC
which(colnames(SoilHealthCC) == 'OC_C')

# N
which(colnames(SoilHealthCC) == 'N_C')

# last useful parameter
which(colnames(SoilHealthCC) == 'MBN_C')

respcol <- c(seq(which(colnames(SoilHealthCC) == 'BiomassCash_C'),which(colnames(SoilHealthCC) == 'OC_C'),5)
             ,seq(which(colnames(SoilHealthCC) == 'N_C'),which(colnames(SoilHealthCC) == 'MBN_C'),5)) # all response columns


#*****************************************************************************************************************
# STEP 2 bootstrapping
#***************************************************************************************************************** 

mat <- matrix(NA, ncol=8, nrow=0)
mat <- as.data.frame (mat)

colnames (mat) <- c("ID", "Response", "n_study", "n_obs","boot_mean", "boot_CI1", "boot_CI2", "All" )


# first loop do CI for all data and all indicators **********************************
k = 1000

for (i in c(1:length(respcol)) ) {
# for (i in 24) {
  
  # i = 2
  subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                              , respcol[i], respcol[i]+1
                              )]
  
  subdata <- subdata[!is.na(subdata[,4]), ]
  
  subdata$yi <- log(subdata[,5])-log(subdata[,4])
  
  colnames(subdata)
  
  if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
      | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
  {
    # is.finite(x) value change to min of yi
    subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                    , min(subdata[is.finite(subdata$yi),]$yi))
  }
  
  subdata <- subdata[!is.na(subdata$yi),]
  
  # head(subdata, 10)
  # hist(subdata$yi, col= "gray", breaks = 20)
  
  # boot-test get nean and CI *********************************************************************
  
  mysamples = replicate(k, sample(subdata$yi, replace=T))
  mymeans = apply(mysamples, 2, mean)
  mean(mysamples)
  
  # quantile
  BootsTest <- quantile(mymeans,c(0.025, 0.50, 0.975))
  t_CI1 <- BootsTest[[1]]
  t_CI1<- (exp(t_CI1)-1)*100
  
  t_mean <- BootsTest[[2]]
  t_mean<- (exp(t_mean)-1)*100
  
  t_CI2 <- BootsTest[[3]]
  t_CI2<- (exp(t_CI2)-1)*100
  
  
  # other information
  response <- colnames(subdata)[4]
  n_obs <- length(subdata[,1])
  n_study <- length(unique(subdata$StudyID))
  
  
  submat <- cbind (i, response, n_study, n_obs, t_mean, t_CI1, t_CI2, "All data")
  colnames (submat) <- colnames (mat)
  mat <- rbind(mat, submat)
  
  print(paste("**********", i, response, "**********"))
  
}


write.csv(mat,"outputs/2.SoilHealthIdicator_t_raw.csv", row.names = F)


#*****************************************************************************************************************
# END
#*****************************************************************************************************************


