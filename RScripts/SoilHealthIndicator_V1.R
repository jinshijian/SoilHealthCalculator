
#*****************************************************************************************************************
# Step 1: Data prapration
#*****************************************************************************************************************

source("G:/My Drive/RSourceCode/basicFunctions.R")
library(car)
# install.packages('agricolae')
library(agricolae)
# install.packages('multcompView')
library(multcompView)
library(bootstrap)

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
# (1) Climate region information
# at first need change latitude and longitude to .25 and .75
# https://stackoverflow.com/questions/38985186/how-to-round-to-0-25-or-0-75-but-not-0-00-or-0-50-in-r

SoilHealthCC$Latitude
SoilHealthCC$Longitude

# round(SoilHealthCC$Latitude/0.5)*0.5
# as.data.frame(cbind(SoilHealthCC$Latitude, round((SoilHealthCC$Latitude-0.25)/0.5)*0.5)+0.25) # not work
# cbind(SoilHealthCC$Latitude, round(SoilHealthCC$Latitude/0.5)*0.5, SoilHealthCC$Latitude - round(SoilHealthCC$Latitude/0.5)*0.5 ) #not work

# SoilHealthCC$Latitude - round(SoilHealthCC$Latitude/0.5)*0.5 # need change this to 0.25
# ifelse ((SoilHealthCC$Latitude - round(SoilHealthCC$Latitude/0.5)*0.5)>0, 0.25, -0.25 )
# cbind(SoilHealthCC$Latitude, round(SoilHealthCC$Latitude/0.5)*0.5,  round(SoilHealthCC$Latitude/0.5)*0.5 
#       + ifelse ((SoilHealthCC$Latitude - round(SoilHealthCC$Latitude/0.5)*0.5)>0, 0.25, -0.25 ))

SoilHealthCC$Latitude_mod <- round(SoilHealthCC$Latitude/0.5)*0.5 + ifelse ((SoilHealthCC$Latitude - round(SoilHealthCC$Latitude/0.5)*0.5)>0, 0.25, -0.25 )
SoilHealthCC[, c(which(colnames(SoilHealthCC)=="Latitude"), which(colnames(SoilHealthCC)=="Latitude_mod"))]

SoilHealthCC$Longitude_mod <- round(SoilHealthCC$Longitude/0.5)*0.5 + ifelse ((SoilHealthCC$Longitude - round(SoilHealthCC$Longitude/0.5)*0.5)>0, 0.25, -0.25 )
SoilHealthCC[, c(which(colnames(SoilHealthCC)=="Longitude"), which(colnames(SoilHealthCC)=="Longitude_mod"))]

head(SoilHealthCC)
# load climate coppen data

climate_koeppon <- read.table("data/Koeppen-Geiger-ASCII.txt", header = T)
head(climate_koeppon)
colnames(climate_koeppon) <- c("Latitude_mod", "Longitude_mod", "Climate_Coeppon")

# left Join
SoilHealthCC <- merge(x = SoilHealthCC, y = climate_koeppon, by = c("Latitude_mod", "Longitude_mod"), all.x = TRUE)

SoilHealthCC[is.na(SoilHealthCC$Climate_Coeppon),]$Latitude_mod
SoilHealthCC[is.na(SoilHealthCC$Climate_Coeppon),]$Longitude_mod
SoilHealthCC[is.na(SoilHealthCC$Climate_Coeppon),]$StudyID

# Update top_climate
sort(unique (SoilHealthCC$Climate_Coeppon))

SoilHealthCC$Top_Climate <- NA
SoilHealthCC$Top_Climate <- as.character(SoilHealthCC$Top_Climate)

SoilHealthCC[SoilHealthCC$Climate_Coeppon == "Af"|SoilHealthCC$Climate_Coeppon == "Am"|SoilHealthCC$Climate_Coeppon == "As"|SoilHealthCC$Climate_Coeppon == "Aw",]$Top_Climate <- "Tropical"
SoilHealthCC[SoilHealthCC$Climate_Coeppon == "BSk"|SoilHealthCC$Climate_Coeppon == "BSh"|SoilHealthCC$Climate_Coeppon == "BWh",]$Top_Climate <- "Arid"
SoilHealthCC[SoilHealthCC$Climate_Coeppon == "Cfa"|SoilHealthCC$Climate_Coeppon == "Cfb"|SoilHealthCC$Climate_Coeppon == "Csa"
             |SoilHealthCC$Climate_Coeppon == "Csb"|SoilHealthCC$Climate_Coeppon == "Cwa"|SoilHealthCC$Climate_Coeppon == "Cwb",]$Top_Climate <- "Temperate"
SoilHealthCC[SoilHealthCC$Climate_Coeppon == "Dfa"|SoilHealthCC$Climate_Coeppon == "Dfb"|SoilHealthCC$Climate_Coeppon == "Dfc"
             |SoilHealthCC$Climate_Coeppon == "Dwa",]$Top_Climate <- "Boreal"

# check result
SoilHealthCC[is.na(SoilHealthCC$Top_Climate),]$Climate_Coeppon
unique(SoilHealthCC$Top_Climate)

SoilHealthCC[is.na(SoilHealthCC$Top_Climate),]$Climate_Coeppon

#*****************************************************************************************************************
# (2) Tillage information
which(colnames(SoilHealthCC) == 'Tillage_Top_T') 
unique(SoilHealthCC$Tillage_Top_T)
SoilHealthCC$Tillage_Top_T <- as.character(SoilHealthCC$Tillage_Top_T) 
unique(SoilHealthCC$Tillage_Top_T)
SoilHealthCC[SoilHealthCC$Tillage_Top_T=="",]$Tillage_Top_T <- "NotAvailable"


#*****************************************************************************************************************
# (3) Soil texture information: seperate into 4 groups
SoilHealthCC$Texture <- as.character(SoilHealthCC$Texture)

sort(unique(SoilHealthCC$Texture))

SoilHealthCC[which(SoilHealthCC$Texture==""),]$Texture <- "NotAvailable"

length(SoilHealthCC[which(SoilHealthCC$Texture==""), 1])

SoilHealthCC$TextureGroup <- NA

# coarse group
SoilHealthCC[SoilHealthCC$Texture=="TU"|SoilHealthCC$Texture=="TH"
             |SoilHealthCC$Texture=="EH"|SoilHealthCC$Texture=="Loamy sand"|SoilHealthCC$Texture=="Loam sand"
             |SoilHealthCC$Texture=="Sandy loam"|SoilHealthCC$Texture=="Sand loam"|SoilHealthCC$Texture=="Sand"
             |SoilHealthCC$Texture=="Sandy", ]$TextureGroup <- "Coarse"

# Medium group
SoilHealthCC[SoilHealthCC$Texture=="Loam"|SoilHealthCC$Texture=="Silt loam"|SoilHealthCC$Texture=="Silty loam "
             |SoilHealthCC$Texture=="Silty loam"|SoilHealthCC$Texture=="Silt", ]$TextureGroup <- "Medium"

# Fine group
SoilHealthCC[SoilHealthCC$Texture=="EHI"|SoilHealthCC$Texture=="TA"
             |SoilHealthCC$Texture=="Clay"|SoilHealthCC$Texture=="Silty clay"|SoilHealthCC$Texture=="Silt clay"
             |SoilHealthCC$Texture=="Silty clay loam"|SoilHealthCC$Texture=="Silt clay loam"|SoilHealthCC$Texture=="Clay loam"
             |SoilHealthCC$Texture=="Sandy clay"|SoilHealthCC$Texture=="Sandy clay loam"
             |SoilHealthCC$Texture=="Clay Loam", ]$TextureGroup <- "Fine"

# NotAvailable
SoilHealthCC[SoilHealthCC$Texture=="NotAvailable", ]$TextureGroup <- "NotAvailable"

# test null 
unique(SoilHealthCC[which(is.na(SoilHealthCC$TextureGroup)),]$Texture)


#*****************************************************************************************************************
# (4) Grain crop group information: seperate into 6 groups

colnames(SoilHealthCC)

sort(unique(SoilHealthCC$GrainCropGroup))

SoilHealthCC$TopGrainCrop <- NA



#1 Monoculture
# 1.1 Monoculture: Corn
SoilHealthCC[SoilHealthCC$GrainCropGroup=="Corn"|SoilHealthCC$GrainCropGroup=="Maize", ]$TopGrainCrop <- "Corn"

# 1.2 Monoculture: Soybean
SoilHealthCC[SoilHealthCC$GrainCropGroup=="Soybean", ]$TopGrainCrop <- "Soybean"

# 1.3 Monoculture: Wheat
SoilHealthCC[SoilHealthCC$GrainCropGroup=="Wheat", ]$TopGrainCrop <- "Wheat"


#2 Rotation of two or average of two 
# 2.1 Corn-soybean
SoilHealthCC[SoilHealthCC$GrainCropGroup=="CS", ]$TopGrainCrop <- "CS"

#3 Rotation of more than three or average of more than three
# 3.1 Corn-Soybean-wheat
SoilHealthCC[SoilHealthCC$GrainCropGroup=="CSW", ]$TopGrainCrop <- "CSW"

#4 Vegetable 
SoilHealthCC[SoilHealthCC$GrainCropGroup=="Vegetable", ]$TopGrainCrop <- "Vegetable"

# Other 
SoilHealthCC[SoilHealthCC$GrainCropGroup=="Arable"|SoilHealthCC$GrainCropGroup=="Sorghum", ]$TopGrainCrop <- "Other"
# Other two rotation
SoilHealthCC[SoilHealthCC$GrainCropGroup=="CO"|SoilHealthCC$GrainCropGroup=="CV"|SoilHealthCC$GrainCropGroup=="MOT"
             |SoilHealthCC$GrainCropGroup=="CW"|SoilHealthCC$GrainCropGroup=="WO"|SoilHealthCC$GrainCropGroup=="WV", ]$TopGrainCrop <- "Other"

# 3.2 Other three rotation
SoilHealthCC[SoilHealthCC$GrainCropGroup=="AVG"|SoilHealthCC$GrainCropGroup=="CSO"
             |SoilHealthCC$GrainCropGroup=="CVO"|SoilHealthCC$GrainCropGroup=="CWO"|SoilHealthCC$GrainCropGroup=="MTT", ]$TopGrainCrop <- "Other"

#4 Orchard : removed
# SoilHealthCC[SoilHealthCC$GrainCropGroup=="Orchard", ]$TopGrainCrop <- "Orchard"

#5 Pasture : removed
# SoilHealthCC[SoilHealthCC$GrainCropGroup=="Pasture", ]$TopGrainCrop <- "Pasture"



# test null 
unique(SoilHealthCC[which(is.na(SoilHealthCC$TopGrainCrop)),]$GrainCropGroup)
unique(SoilHealthCC$TopGrainCrop)

#*****************************************************************************************************************
# (5) Cover crop group information: seperate into 7 groups



SoilHealthCC$TopCoverCrop <- NA

sort(unique(SoilHealthCC$CoverCropGroup))

#1 Not available
SoilHealthCC[SoilHealthCC$CoverCropGroup == ""|SoilHealthCC$CoverCropGroup=="Not_available"
             |SoilHealthCC$CoverCropGroup=="No",]$TopCoverCrop <- "OtherCC"

#2 Brassica
SoilHealthCC[SoilHealthCC$CoverCropGroup == "Brassica",]$TopCoverCrop <- "OtherCC"

#3 Broadleaf
SoilHealthCC[SoilHealthCC$CoverCropGroup == "Broadleaf"|SoilHealthCC$CoverCropGroup=="Broadleaf/Brassica"
             |SoilHealthCC$CoverCropGroup=="BroadleafTree",]$TopCoverCrop <- "OtherCC"

#4 Grass & Rye
SoilHealthCC[SoilHealthCC$CoverCropGroup == "Grass"|SoilHealthCC$CoverCropGroup=="Weeds"|SoilHealthCC$CoverCropGroup == "Rye",]$TopCoverCrop <- "Grass"

#5 Rye


#6 Legume
SoilHealthCC[SoilHealthCC$CoverCropGroup == "Legume"|SoilHealthCC$CoverCropGroup=="Legume_Tree",]$TopCoverCrop <- "Legume"


#7 MOT
SoilHealthCC[SoilHealthCC$CoverCropGroup == "BG"|SoilHealthCC$CoverCropGroup=="GG"
             |SoilHealthCC$CoverCropGroup=="LB"|SoilHealthCC$CoverCropGroup=="LG"|SoilHealthCC$CoverCropGroup=="LL"
             |SoilHealthCC$CoverCropGroup=="MOT"|SoilHealthCC$CoverCropGroup=="MMT",]$TopCoverCrop <- "MTT"

#8 MTT
SoilHealthCC[SoilHealthCC$CoverCropGroup == "MTT"|SoilHealthCC$CoverCropGroup=="Mixed"
             |SoilHealthCC$CoverCropGroup=="AVG",]$TopCoverCrop <- "MTT"


# test null 
unique(SoilHealthCC[which(is.na(SoilHealthCC$TopCoverCrop)),]$CoverCropGroup)
unique(SoilHealthCC[SoilHealthCC$TopCoverCrop=="",]$CoverCropGroup)
unique(SoilHealthCC$TopCoverCrop)



#*****************************************************************************************************************
# OC need some calculation before data process: Use both % and concentration data: only two study, so done in excel
#*****************************************************************************************************************

# first need create model to predict BD base on OC

# subdata <- SoilHealthCC[!is.na(SoilHealthCC$OC_C) | !is.na(SoilHealthCC$OC_Con_C), ]
# subdata <- SoilHealthCC[!is.na(SoilHealthCC$OC_C), ] # same result
# plot(log(subdata$OC_C) ~ subdata$BD_C)
# abline(lm(log(subdata$OC_C) ~ subdata$BD_C), lwd=2, col="red")
# 
# plot(lm(log(subdata$OC_C) ~ subdata$BD_C))
# 
# summary(lm(log(subdata$OC_C) ~ subdata$BD_C))
# 
# 
# plot(subdata$OC_Con_C , subdata$BD_C)
# abline(lm(subdata$BD_C ~ subdata$OC_Con_C))
# summary(lm(subdata$BD_C ~ subdata$OC_Con_C))


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
# STEP 2.1 Unbalanced ANOVA for All and boots mean and by groups
#***************************************************************************************************************** 

# creat a matrix hold all statistic values

var_climate <- c("Tropical", "Arid", "Temperate", "Boreal")
var_GrainCrop <- c("Corn","Soybean","Wheat", "Vegetable", "CS", "CSW", "Other")
var_CoverCrop <- c("Legume", "Grass", "MTT", "OtherCC")
var_Texture <- c("Coarse", "Medium", "Fine", "NotAvailable")
# var_tillage <- c("CT", "RT", "NT", "NotAvailable")

4*7*4*4

unique(SoilHealthCC$Top_Climate)
unique(SoilHealthCC$TopGrainCrop)
unique(SoilHealthCC$TopCoverCrop)
unique(SoilHealthCC$TextureGroup)
unique(SoilHealthCC$Tillage_Top_T)




mat <- matrix(NA, ncol=23, nrow=0)
mat <- as.data.frame (mat)
colnames (mat) <- paste(c(1:23),"I", sep = "")


k = 10000

# first hiarchy type: Grain:CC:Texture **********************************

for (i in c(17,18)){
  
  subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'SamplingYear')
                              , respcol[i], respcol[i]+1
                              , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                      |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
  
  subdata <- subdata[!is.na(subdata[,4]), ]
  
  subdata$yi <- log(subdata[,5])-log(subdata[,4])
  
  if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
      | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
  {
    # is.finite(x) value change to min of yi
    subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                    , min(subdata[is.finite(subdata$yi),]$yi))
    # this line is not necessary anymore because NA has been removed
    # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
  }
  
  subdata <- subdata[!is.na(subdata$yi), ]
  val_1_mean_all <- summary(quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975)) )[4] # get mean value
  val_1_mean_all <- (exp(val_1_mean_all)-1)*100
  
  # 1. climate
  for (c in 1:length(var_climate)) {
    sub_c <- subdata[subdata$Top_Climate == var_climate[c], ]
    
    # (1.1) Unbalaced ANOVA test climate *********************************************************************
    # Unbalaced ANOVA
    try_anova1 <- try (Anova(lm(yi~Top_Climate,data = subdata), type="III") )
    if(inherits(try_anova1, "try-error"))  {
      val_2_p_UB_anova_climate <- NA
    }
    else {
      val_2_p_UB_anova_climate <- round(Anova(lm(yi~Top_Climate,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # (1.2) Boots mean ***************************************************************************************
     
    if (length(sub_c$yi) == 0) {
      val_3_mean_climate <- NA
      }
    else {
      val_3_mean_climate <- summary(quantile(bootstrap(sub_c$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
      val_3_mean_climate <- (exp(val_3_mean_climate)-1)*100
    }
    
    # 2.1 Grain crop *******************************************************************************************
    for (g in 1:length(var_GrainCrop)) {
      sub_c_g <- sub_c[sub_c$TopGrainCrop == var_GrainCrop[g], ]
      # 2.1 Unbalaced ANOVA test climate ********************************************************************* 
      try_anova2 <- try (Anova(lm(yi~TopGrainCrop,data = sub_c), type="III") )
      if(inherits(try_anova2, "try-error"))  {
        val_4_p_UB_anova_GrainCrop <- NA
      }
      else {
        val_4_p_UB_anova_GrainCrop <- round(Anova(lm(yi~TopGrainCrop,data = sub_c), type="III")$`Pr(>F)`[2], 4)
      }
      
      # (2.2) Boots mean ***************************************************************************************
      
      if (length(sub_c_g$yi) == 0) {
        val_5_mean_c_g <- NA
      }
      else {
        val_5_mean_c_g <- summary(quantile(bootstrap(sub_c_g$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val_5_mean_c_g <- (exp(val_5_mean_c_g)-1)*100
      }
      
      # 3. Cover crop
      for (cc in 1:length(var_CoverCrop)) {
        sub_c_g_cc <- sub_c_g[sub_c_g$TopCoverCrop == var_CoverCrop[cc], ]
        
        # 3.1 Unbalaced ANOVA test climate ********************************************************************* 
        try_anova3 <- try (Anova(lm(yi~TopCoverCrop,data = sub_c_g), type="III") )
        if(inherits(try_anova3, "try-error"))  {
          val_6_p_UB_anova_cc <- NA
        }
        else {
          val_6_p_UB_anova_cc <- round(Anova(lm(yi~TopCoverCrop,data = sub_c_g), type="III")$`Pr(>F)`[2], 4)
        }
        
        # (3.2) Boots mean ***************************************************************************************
        
        if (length(sub_c_g_cc$yi) == 0) {
          val_7_mean_c_g_cc <- NA
        }
        else {
          val_7_mean_c_g_cc <- summary(quantile(bootstrap(sub_c_g_cc$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
          val_7_mean_c_g_cc <- (exp(val_7_mean_c_g_cc)-1)*100
        }
        
        # 4. Texture
        for (t in 1:length(var_Texture)) {
          sub_c_g_cc_t <- sub_c_g_cc[sub_c_g_cc$TextureGroup == var_Texture[t], ]
          
          # 4.1 Unbalaced ANOVA test climate ********************************************************************* 
          try_anova4 <- try (Anova(lm(yi~TextureGroup,data = sub_c_g_cc), type="III") )
          if(inherits(try_anova4, "try-error"))  {
            val_8_p_UB_anova_cc_t <- NA
          }
          else {
            val_8_p_UB_anova_cc_t <- round(Anova(lm(yi~TextureGroup,data = sub_c_g_cc), type="III")$`Pr(>F)`[2], 4)
          }
          
          # 4.2 boots ********************************************************************************************
          if (length(sub_c_g_cc_t[,1]) == 0) {
            val_9_mean_c_g_cc_t <- NA
            }
          else {
            Boots <- bootstrap(sub_c_g_cc_t$yi,k,mean)
            val_9_mean_c_g_cc_t <- summary(quantile(Boots$thetastar,c(0.025,0.975)))[4] # get mean value
            val_9_mean_c_g_cc_t <- (exp(val_9_mean_c_g_cc_t)-1)*100
          }
          
          var_10_code <- paste(var_climate[c],var_GrainCrop[g],var_CoverCrop[cc],var_Texture[t], sep = "_")
          
          # Combind
          submat <- cbind (i, colnames(subdata)[4], "All", length(subdata$yi), val_1_mean_all
                           , var_climate[c], length(sub_c$yi), val_2_p_UB_anova_climate, val_3_mean_climate
                           , var_GrainCrop[g], length(sub_c_g$yi), val_4_p_UB_anova_GrainCrop, val_5_mean_c_g
                           , var_CoverCrop[cc], length(sub_c_g_cc$yi), val_6_p_UB_anova_cc, val_7_mean_c_g_cc
                           , var_Texture[t], length(sub_c_g_cc_t$yi), val_8_p_UB_anova_cc_t, val_9_mean_c_g_cc_t
                           , var_10_code, "G-CC-T"
                           )
          colnames(submat) <- colnames (mat)
          mat <- rbind(mat, submat)
          
          print(paste(colnames(subdata)[4],"**********", i, c, g, cc, t, "**********"))
          
        }
      }
    }
  }
}




# write.csv(mat,"8.SoilHealth_indicators_sum.csv", row.names = F)




#*****************************************************************************************************************
# Second hiarchy type: CC:Grain:Texture 
#*****************************************************************************************************************

for (i in c(2,16,23)){
  # i =2
  subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'SamplingYear')
                              , respcol[i], respcol[i]+1
                              , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                      |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
  
  subdata <- subdata[!is.na(subdata[,4]), ]
  
  subdata$yi <- log(subdata[,5])-log(subdata[,4])
  
  if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
      | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
  {
    # is.finite(x) value change to min of yi
    subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                    , min(subdata[is.finite(subdata$yi),]$yi))
    # this line is not necessary anymore because NA has been removed
    # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
  }
  
  subdata <- subdata[!is.na(subdata$yi), ]
  val_1_mean_all <- summary(quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975)) )[4] # get mean value
  val_1_mean_all <- (exp(val_1_mean_all)-1)*100
  
  # 1. climate
  for (c in 1:length(var_climate)) {
    # c = 1
    sub_c <- subdata[subdata$Top_Climate == var_climate[c], ]
    
    # (1.1) Unbalaced ANOVA test climate *********************************************************************
    # Unbalaced ANOVA
    try_anova1 <- try (Anova(lm(yi~Top_Climate,data = subdata), type="III") )
    if(inherits(try_anova1, "try-error"))  {
      val_2_p_UB_anova_climate <- NA
    }
    else {
      val_2_p_UB_anova_climate <- round(Anova(lm(yi~Top_Climate,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # (1.2) Boots mean ***************************************************************************************
    
    if (length(sub_c$yi) == 0) {
      val_3_mean_climate <- NA
    }
    else {
      val_3_mean_climate <- summary(quantile(bootstrap(sub_c$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
      val_3_mean_climate <- (exp(val_3_mean_climate)-1)*100
    }
    
    # 2.1 Cover crop *******************************************************************************************
    for (l2 in 1:length(var_CoverCrop)) {
      # l2 = 1
      sub_c_L2 <- sub_c[sub_c$TopCoverCrop == var_CoverCrop[l2], ]
      # 2.1 Unbalaced ANOVA test climate ********************************************************************* 
      try_anova2 <- try (Anova(lm(yi~TopCoverCrop,data = sub_c), type="III") )
      if(inherits(try_anova2, "try-error"))  {
        val_4_p_UB_anova_L2 <- NA
      }
      else {
        val_4_p_UB_anova_L2 <- round(Anova(lm(yi~TopCoverCrop,data = sub_c), type="III")$`Pr(>F)`[2], 4)
      }
      
      # (2.2) Boots mean ***************************************************************************************
      
      if (length(sub_c_L2$yi) == 0) {
        val_5_mean_c_L2 <- NA
      }
      else {
        val_5_mean_c_L2 <- summary(quantile(bootstrap(sub_c_L2$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val_5_mean_c_L2 <- (exp(val_5_mean_c_L2)-1)*100
      }
      
      # 3. Grain crop
      for (l3 in 1:length(var_GrainCrop)) {
        # l3 = 1
        sub_c_L3 <- sub_c_L2[sub_c_L2$TopGrainCrop == var_GrainCrop[l3], ]
        
        # 3.1 Unbalaced ANOVA test climate ********************************************************************* 
        try_anova3 <- try (Anova(lm(yi~TopGrainCrop,data = sub_c_L2), type="III") )
        if(inherits(try_anova3, "try-error"))  {
          val_6_p_UB_anova_L3 <- NA
        }
        else {
          val_6_p_UB_anova_L3 <- round(Anova(lm(yi~TopGrainCrop,data = sub_c_L2), type="III")$`Pr(>F)`[2], 4)
        }
        
        # (3.2) Boots mean ***************************************************************************************
        
        if (length(sub_c_L3$yi) == 0) {
          val_7_mean_c_L3 <- NA
        }
        else {
          val_7_mean_c_L3 <- summary(quantile(bootstrap(sub_c_L3$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
          val_7_mean_c_L3 <- (exp(val_7_mean_c_L3)-1)*100
        }
        
        # 4. Texture
        for (l4 in 1:length(var_Texture)) {
          # l4 = 1
          sub_c_L4 <- sub_c_L3[sub_c_L3$TextureGroup == var_Texture[l4], ]
          
          # 4.1 Unbalaced ANOVA test climate ********************************************************************* 
          try_anova4 <- try (Anova(lm(yi~TextureGroup,data = sub_c_L3), type="III") )
          if(inherits(try_anova4, "try-error"))  {
            val_8_p_UB_anova_L4 <- NA
          }
          else {
            val_8_p_UB_anova_L4 <- round(Anova(lm(yi~TextureGroup,data = sub_c_L3), type="III")$`Pr(>F)`[2], 4)
          }
          
          # 4.2 boots ********************************************************************************************
          if (length(sub_c_L4[,1]) == 0) {
            val_9_mean_c_L4 <- NA
          }
          else {
            Boots <- bootstrap(sub_c_L4$yi,k,mean)
            val_9_mean_c_L4 <- summary(quantile(Boots$thetastar,c(0.025,0.975)))[4] # get mean value
            val_9_mean_c_L4 <- (exp(val_9_mean_c_L4)-1)*100
          }
          
          var_10_code <- paste(var_climate[c],var_GrainCrop[l3],var_CoverCrop[l2],var_Texture[l4], sep = "_")
          
          # Combind
          submat <- cbind (i, colnames(subdata)[4], "All", length(subdata$yi), val_1_mean_all
                           , var_climate[c], length(sub_c$yi), val_2_p_UB_anova_climate, val_3_mean_climate
                           , var_GrainCrop[l3], length(sub_c_L2$yi), val_4_p_UB_anova_L2, val_5_mean_c_L2
                           , var_CoverCrop[l2], length(sub_c_L3$yi), val_6_p_UB_anova_L3, val_7_mean_c_L3
                           , var_Texture[l4], length(sub_c_L4$yi), val_8_p_UB_anova_L4, val_9_mean_c_L4
                           , var_10_code, "CC-G-T"
          )
          colnames(submat) <- colnames (mat)
          mat <- rbind(mat, submat)
          
          print(paste(colnames(subdata)[4],"**********", i, c, l2, l3, l4, "**********"))
          
        }
      }
    }
  }
}


#*****************************************************************************************************************
# Third hiarchy type: Texture:CC:Grain
#*****************************************************************************************************************

for (i in c(3,12,19)){
  
  subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'SamplingYear')
                              , respcol[i], respcol[i]+1
                              , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                      |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
  
  subdata <- subdata[!is.na(subdata[,4]), ]
  
  subdata$yi <- log(subdata[,5])-log(subdata[,4])
  
  if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
      | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
  {
    # is.finite(x) value change to min of yi
    subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                    , min(subdata[is.finite(subdata$yi),]$yi))
    # this line is not necessary anymore because NA has been removed
    # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
  }
  
  subdata <- subdata[!is.na(subdata$yi), ]
  val_1_mean_all <- summary(quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975)) )[4] # get mean value
  val_1_mean_all <- (exp(val_1_mean_all)-1)*100
  
  # 1. climate
  for (c in 1:length(var_climate)) {
    sub_c <- subdata[subdata$Top_Climate == var_climate[c], ]
    
    # (1.1) Unbalaced ANOVA test climate *********************************************************************
    # Unbalaced ANOVA
    try_anova1 <- try (Anova(lm(yi~Top_Climate,data = subdata), type="III") )
    if(inherits(try_anova1, "try-error"))  {
      val_2_p_UB_anova_climate <- NA
    }
    else {
      val_2_p_UB_anova_climate <- round(Anova(lm(yi~Top_Climate,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # (1.2) Boots mean ***************************************************************************************
    
    if (length(sub_c$yi) == 0) {
      val_3_mean_climate <- NA
    }
    else {
      val_3_mean_climate <- summary(quantile(bootstrap(sub_c$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
      val_3_mean_climate <- (exp(val_3_mean_climate)-1)*100
    }
    
    # 2.1 Texture *******************************************************************************************
    for (l2 in 1:length(var_Texture)) {
      sub_c_L2 <- sub_c[sub_c$TextureGroup == var_Texture[l2], ]
      # 2.1 Unbalaced ANOVA test climate ********************************************************************* 
      try_anova2 <- try (Anova(lm(yi~TextureGroup,data = sub_c), type="III") )
      if(inherits(try_anova2, "try-error"))  {
        val_4_p_UB_anova_L2 <- NA
      }
      else {
        val_4_p_UB_anova_L2 <- round(Anova(lm(yi~TextureGroup,data = sub_c), type="III")$`Pr(>F)`[2], 4)
      }
      
      # (2.2) Boots mean ***************************************************************************************
      
      if (length(sub_c_L2$yi) == 0) {
        val_5_mean_c_L2 <- NA
      }
      else {
        val_5_mean_c_L2 <- summary(quantile(bootstrap(sub_c_L2$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val_5_mean_c_L2 <- (exp(val_5_mean_c_L2)-1)*100
      }
      
      # 3. Cover crop
      for (l3 in 1:length(var_CoverCrop)) {
        sub_c_L3 <- sub_c_L2[sub_c_L2$TopCoverCrop == var_CoverCrop[l3], ]
        
        # 3.1 Unbalaced ANOVA test climate ********************************************************************* 
        try_anova3 <- try (Anova(lm(yi~TopCoverCrop,data = sub_c_L2), type="III") )
        if(inherits(try_anova3, "try-error"))  {
          val_6_p_UB_anova_L3 <- NA
        }
        else {
          val_6_p_UB_anova_L3 <- round(Anova(lm(yi~TopCoverCrop,data = sub_c_L2), type="III")$`Pr(>F)`[2], 4)
        }
        
        # (3.2) Boots mean ***************************************************************************************
        
        if (length(sub_c_L3$yi) == 0) {
          val_7_mean_c_L3 <- NA
        }
        else {
          val_7_mean_c_L3 <- summary(quantile(bootstrap(sub_c_L3$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
          val_7_mean_c_L3 <- (exp(val_7_mean_c_L3)-1)*100
        }
        
        # 4. GrainCrop
        for (l4 in 1:length(var_GrainCrop)) {
          sub_c_L4 <- sub_c_L3[sub_c_L3$TopGrainCrop == var_GrainCrop[l4], ]
          
          # 4.1 Unbalaced ANOVA test climate ********************************************************************* 
          try_anova4 <- try (Anova(lm(yi~TopGrainCrop,data = sub_c_L3), type="III") )
          if(inherits(try_anova4, "try-error"))  {
            val_8_p_UB_anova_L4 <- NA
          }
          else {
            val_8_p_UB_anova_L4 <- round(Anova(lm(yi~TopGrainCrop,data = sub_c_L3), type="III")$`Pr(>F)`[2], 4)
          }
          
          # 4.2 boots ********************************************************************************************
          if (length(sub_c_L4[,1]) == 0) {
            val_9_mean_c_L4 <- NA
          }
          else {
            Boots <- bootstrap(sub_c_L4$yi,k,mean)
            val_9_mean_c_L4 <- summary(quantile(Boots$thetastar,c(0.025,0.975)))[4] # get mean value
            val_9_mean_c_L4 <- (exp(val_9_mean_c_L4)-1)*100
          }
          
          var_10_code <- paste(var_climate[c],var_GrainCrop[l4],var_CoverCrop[l3],var_Texture[l2], sep = "_")
          
          # Combind
          submat <- cbind (i, colnames(subdata)[4], "All", length(subdata$yi), val_1_mean_all
                           , var_climate[c], length(sub_c$yi), val_2_p_UB_anova_climate, val_3_mean_climate
                           , var_GrainCrop[l4], length(sub_c_L2$yi), val_4_p_UB_anova_L2, val_5_mean_c_L2
                           , var_CoverCrop[l3], length(sub_c_L3$yi), val_6_p_UB_anova_L3, val_7_mean_c_L3
                           , var_Texture[l2], length(sub_c_L4$yi), val_8_p_UB_anova_L4, val_9_mean_c_L4
                           , var_10_code, "T-CC-G"
          )
          
          
          colnames(submat) <- colnames (mat)
          mat <- rbind(mat, submat)
          
          print(paste(colnames(subdata)[4],"**********", i, c, l2, l3, l4, "**********"))
          
        }
      }
    }
  }
}


#*****************************************************************************************************************
# Fourth hiarchy type: CC:Texture:Grai 
#*****************************************************************************************************************

for (i in c(4,5,15,37,38)){
  
  subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'SamplingYear')
                              , respcol[i], respcol[i]+1
                              , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                      |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
  
  subdata <- subdata[!is.na(subdata[,4]), ]
  
  subdata$yi <- log(subdata[,5])-log(subdata[,4])
  
  if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
      | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
  {
    # is.finite(x) value change to min of yi
    subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                    , min(subdata[is.finite(subdata$yi),]$yi))
    # this line is not necessary anymore because NA has been removed
    # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
  }
  
  subdata <- subdata[!is.na(subdata$yi), ]
  val_1_mean_all <- summary(quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975)) )[4] # get mean value
  val_1_mean_all <- (exp(val_1_mean_all)-1)*100
  
  # 1. climate
  for (c in 1:length(var_climate)) {
    sub_c <- subdata[subdata$Top_Climate == var_climate[c], ]
    
    # (1.1) Unbalaced ANOVA test climate *********************************************************************
    # Unbalaced ANOVA
    try_anova1 <- try (Anova(lm(yi~Top_Climate,data = subdata), type="III") )
    if(inherits(try_anova1, "try-error"))  {
      val_2_p_UB_anova_climate <- NA
    }
    else {
      val_2_p_UB_anova_climate <- round(Anova(lm(yi~Top_Climate,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # (1.2) Boots mean ***************************************************************************************
    
    if (length(sub_c$yi) == 0) {
      val_3_mean_climate <- NA
    }
    else {
      val_3_mean_climate <- summary(quantile(bootstrap(sub_c$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
      val_3_mean_climate <- (exp(val_3_mean_climate)-1)*100
    }
    
    # 2.1 Cover crop *******************************************************************************************
    for (l2 in 1:length(var_CoverCrop)) {
      sub_c_L2 <- sub_c[sub_c$TopCoverCrop == var_CoverCrop[l2], ]
      # 2.1 Unbalaced ANOVA test climate ********************************************************************* 
      try_anova2 <- try (Anova(lm(yi~TopCoverCrop,data = sub_c), type="III") )
      if(inherits(try_anova2, "try-error"))  {
        val_4_p_UB_anova_L2 <- NA
      }
      else {
        val_4_p_UB_anova_L2 <- round(Anova(lm(yi~TopCoverCrop,data = sub_c), type="III")$`Pr(>F)`[2], 4)
      }
      
      # (2.2) Boots mean ***************************************************************************************
      
      if (length(sub_c_L2$yi) == 0) {
        val_5_mean_c_L2 <- NA
      }
      else {
        val_5_mean_c_L2 <- summary(quantile(bootstrap(sub_c_L2$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val_5_mean_c_L2 <- (exp(val_5_mean_c_L2)-1)*100
      }
      
      # 3. Texture
      for (l3 in 1:length(var_Texture)) {
        sub_c_L3 <- sub_c_L2[sub_c_L2$TextureGroup == var_Texture[l3], ]
        
        # 3.1 Unbalaced ANOVA test climate ********************************************************************* 
        try_anova3 <- try (Anova(lm(yi~TextureGroup,data = sub_c_L2), type="III") )
        if(inherits(try_anova3, "try-error"))  {
          val_6_p_UB_anova_L3 <- NA
        }
        else {
          val_6_p_UB_anova_L3 <- round(Anova(lm(yi~TextureGroup,data = sub_c_L2), type="III")$`Pr(>F)`[2], 4)
        }
        
        # (3.2) Boots mean ***************************************************************************************
        
        if (length(sub_c_L3$yi) == 0) {
          val_7_mean_c_L3 <- NA
        }
        else {
          val_7_mean_c_L3 <- summary(quantile(bootstrap(sub_c_L3$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
          val_7_mean_c_L3 <- (exp(val_7_mean_c_L3)-1)*100
        }
        
        # 4. Texture
        for (l4 in 1:length(var_GrainCrop)) {
          sub_c_L4 <- sub_c_L3[sub_c_L3$TopGrainCrop == var_GrainCrop[l4], ]
          
          # 4.1 Unbalaced ANOVA test climate ********************************************************************* 
          try_anova4 <- try (Anova(lm(yi~TopGrainCrop,data = sub_c_L3), type="III") )
          if(inherits(try_anova4, "try-error"))  {
            val_8_p_UB_anova_L4 <- NA
          }
          else {
            val_8_p_UB_anova_L4 <- round(Anova(lm(yi~TopGrainCrop,data = sub_c_L3), type="III")$`Pr(>F)`[2], 4)
          }
          
          # 4.2 boots ********************************************************************************************
          if (length(sub_c_L4[,1]) == 0) {
            val_9_mean_c_L4 <- NA
          }
          else {
            Boots <- bootstrap(sub_c_L4$yi,k,mean)
            val_9_mean_c_L4 <- summary(quantile(Boots$thetastar,c(0.025,0.975)))[4] # get mean value
            val_9_mean_c_L4 <- (exp(val_9_mean_c_L4)-1)*100
          }
          
          var_10_code <- paste(var_climate[c],var_GrainCrop[l4],var_CoverCrop[l2],var_Texture[l3], sep = "_")
          
          # Combind
          submat <- cbind (i, colnames(subdata)[4], "All", length(subdata$yi), val_1_mean_all
                           , var_climate[c], length(sub_c$yi), val_2_p_UB_anova_climate, val_3_mean_climate
                           , var_GrainCrop[l4], length(sub_c_L2$yi), val_4_p_UB_anova_L2, val_5_mean_c_L2
                           , var_CoverCrop[l2], length(sub_c_L3$yi), val_6_p_UB_anova_L3, val_7_mean_c_L3
                           , var_Texture[l3], length(sub_c_L4$yi), val_8_p_UB_anova_L4, val_9_mean_c_L4
                           , var_10_code, "CC-T-G"
          )
          
          
          colnames(submat) <- colnames (mat)
          mat <- rbind(mat, submat)
          
          print(paste(colnames(subdata)[4],"**********", i, c, l2, l3, l4, "**********"))
          
        }
      }
    }
  }
}

colnames (mat) <- c("ID", "Response", "All","obs_all", "mean_all"
                    , "Climate", "obs_Climate", "P_Climate", "mean_Climate"
                    , "GrainCrop", "obs_GrainCrop", "P_GrainCrop", "mean_GrainCrop"
                    , "CoverCrop", "obs_CoverCrop", "P_CoverCrop", "mean_CoverCrop"
                    , "Texture", "obs_Texture", "P_Texture", "mean_Texture", "Code", "Hierarchy"
)


write.csv(mat,"outputs/SoilHealth_Indicator_Sum.csv", row.names = F)

#*****************************************************************************************************************
# END
#*****************************************************************************************************************


