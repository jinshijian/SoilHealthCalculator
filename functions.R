
#*****************************************************************************************************************
# function 1
#*****************************************************************************************************************

plot_sites_dist <- function () {
  
  # Step 1.1: Prepare data  # remove AFS, Orchard, and pasture
  
  # aggregate
  siteInfor <- summarySE (data=SoilHealthCC, measurevar='YearPublication', groupvars=c("Latitude","Longitude"))
  
  siteInfor <- siteInfor[, c(1:3)]
  siteInfor <- siteInfor[which(!is.na(siteInfor$Latitude)),]
  
  # siteInfor[siteInfor$N >= 50,3] <- 50
  siteInfor$var_size <- mean(siteInfor$N)*0.15 + (siteInfor$N)*0.05
  x_cc <- rep(-170, 4)
  size <- c(9, 6, 3, 1)
  # sizeN <- c(>91, 61-90, 31-60, <30)
  y <- c(0, -15, -30, -45)
  
  cc_legend <- cbind (x_cc, y, size)
  cc_legend <- as.data.frame(cc_legend)
  
  # Step 2: Plot
  # global map
  counties <- map_data("world", region = ".", exact = FALSE)
  sort(unique(counties$region))
  
  globMap <- ggplot(data = counties) + 
    geom_polygon(aes(x = long, y = lat , group = group), color = "white", fill = 'gray') + 
    guides(fill=FALSE)+
    theme(legend.position="none")
  # ylim(-57, 90)
  globMap
  
  sitemap1 <- globMap + 
    geom_point(data = siteInfor, aes(x=Longitude, y=Latitude)
               , shape=1, col = "red" 
               , size = siteInfor$var_size, alpha = 7/10
               , show.legend = TRUE)+
    theme(axis.text.y   = element_text(size=12),
          axis.text.x   = element_text(size=12),
          axis.title.y   = element_text(size=15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x   = element_text(size=15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          # axis.title.y  = element_blank(),
          # axis.title.x  = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
    scale_x_continuous(name="Longitude", breaks=seq(-180,180, 30),labels = seq(-180,180, 30))+
    scale_y_continuous(limits = c(-60, 90),name="Latitude", breaks=seq(-60,90,20),labels = seq(-60,90,20))
  
  # ?scale_y_continuous()
  
  sitemap1 <- sitemap1 + 
    geom_point(data = cc_legend, aes(x=cc_legend$x_cc, y=cc_legend$y), color = "red" 
               , shape=1, size = cc_legend$size) +
    annotate("text", x = -152, y = 0, label = "( > 91 )", size = 4.5, adj = 0)+
    annotate("text", x = -152, y = -15, label = "( 61-90 )", size = 4.5, adj = 0)+
    annotate("text", x = -152, y = -30, label = "( 31-60 )", size = 4.5, adj = 0)+
    annotate("text", x = -152, y = -45, label = "( < 30 )", size = 4.5, adj = 0) +
    annotate("text", x = -172, y = 30, label = "Legend", size = 5, adj = 0) + 
    annotate("text", x = -175, y = 15, label = "CC  Obs ( n )", size = 4.5, adj = 0)+
    annotate("text", x = 0, y = -55, label = "All sites (4024/269)", size = 4.5)
  
  sitemap1 
}

#*****************************************************************************************************************
# function 2
#*****************************************************************************************************************

sum_shi_t <- function() {
  
  # get columns number hold indicators
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
  # STEP 2 Meta-analysis
  
  mat <- matrix(NA, ncol=9, nrow=0)
  mat <- as.data.frame (mat)
  colnames (mat) <- c("ID", "Response", "n_study", "n_obs","t_mean", "t_CI1", "t_CI2", "t_p", "All" )
  
  # first loop do CI for all data and all indicators **********************************
  
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
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # t-test get nean and CI *********************************************************************
    t_test <- t.test(subdata$yi, mu=0)
    # get t-test statistics
    t_mean <- t_test$estimate
    t_mean<- (exp(t_mean)-1)*100
    
    t_CI1 <- t_test$conf.int[1]
    t_CI1<- (exp(t_CI1)-1)*100
    
    t_CI2 <- t_test$conf.int[2]
    t_CI2<- (exp(t_CI2)-1)*100
    
    t_p <- t_test$p.value
    
    # other information
    response <- colnames(subdata)[4]
    n_obs <- length(subdata[,1])
    n_study <- length(unique(subdata$StudyID))
    
    submat <- cbind (i, response, n_study, n_obs, t_mean, t_CI1, t_CI2, t_p, "All data")
    colnames (submat) <- colnames (mat)
    mat <- rbind(mat, submat)
    
    print(paste("**********", i, response, "**********"))
  }
  return (mat)
}



#*****************************************************************************************************************
# function 3
#*****************************************************************************************************************
# plot meta forest 
meta_forest <- function(){
  
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
       , omi = c(0.5, 1, 0.1, 0.1)  # by inches, outer margin 
       , mgp = c(0, 0.3, 0) # set distance of axis
       , tcl = 0.4
       # , cex.axis = 1.0
       , mfrow=c(1,1))
  
  # meta_II with predicted SD
  x_min <- min(sum_data$t_CI1, na.rm = T)
  # x_max <- max(sum_data$t_CI2, na.rm = T)
  x_max <- 160
  
  sum_data$col <- as.character(sum_data$col)
  
  plot(sum_data$ID ~ sum_data$t_mean, lwd=2
       # ,xlim=c(0,21),ylim=c(1,16)
       , las=1
       , xaxt='n', yaxt='n'
       , xlim = c(x_min, x_max)
       # , ylim = c(0.5, 4.5)
       , xlab = '', ylab='', main=''
       , col = sum_data$col
       , pch = sum_data$pch
       # , cex = 0.75
       , cex = 1.5 
       # , xaxs="i"
  )
  
  arrows(sum_data$t_CI1, sum_data$ID, sum_data$t_CI2, sum_data$ID
         ,code=3,length=0.05,angle=90,sum_data$col,lwd=2)
  
  abline(v=0, col="red", lty=2, lwd=2)
  
  
  axis (side = 1, at = seq(-80, 160, 40), labels = seq(-80, 160, 40)
        , cex=1
        , las = 1)
  
  axis (side = 2, at = sum_data$ID, labels = paste(sum_data$Response, sep = "")
        , cex=1
        , las = 2)
  
  text(ifelse(sum_data$t_CI2 > 120, sum_data$t_CI1-20, sum_data$t_CI2+2.5), sum_data$ID
       , paste(sum_data$n_obs,"/",sum_data$n_study, sep = ""), cex=0.85, adj=0)
  
  text(110,42.75,"(a) Physical", cex=1, adj=0)
  text(110,34.25,"(b) Chemical", cex=1, adj=0)
  text(110,25.25,"(c) Biological", cex=1, adj=0)
  text(110,11.25,"(d) Environmental", cex=1, adj=0)
  text(110,2.25,"(e) Agronomic", cex=1, adj=0)
  
  abline(h=c(3, 12, 26, 35), col="black", lty=3, lwd=2)
  
  mtext(side = 1, text = paste("Soil health indicator (% changes)", sep=" "), line = 1, cex=1.05, outer = T, adj = 0.7)
}


#*****************************************************************************************************************
# function 4 - UANOVA
#*****************************************************************************************************************
shi_UANOVA <- function () {
  
  
  #*****************************************************************************************************************
  # (1) Climate region information
  # at first need change latitude and longitude to .25 and .75
  # https://stackoverflow.com/questions/38985186/how-to-round-to-0-25-or-0-75-but-not-0-00-or-0-50-in-r
  

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
  # STEP 2.1 Unbalanced ANOVA for All
  #***************************************************************************************************************** 
  
  # creat a matrix hold all statistic values
  
  var_climate <- c("Tropical", "Arid", "Temperate", "Boreal")
  var_GrainCrop <- c("Corn","Soybean","Wheat", "Vegetable", "CS", "CSW", "Other")
  var_CoverCrop <- c("Legume", "Grass", "MTT", "OtherCC")
  var_Texture <- c("Coarse", "Medium", "Fine")
  var_tillage <- c("CT", "RT", "NT", "NotAvailable")
  
  
  # unique(SoilHealthCC$TopGrainCrop)
  
  mat <- matrix(NA, ncol=9, nrow=0)
  mat <- as.data.frame (mat)
  
  colnames (mat) <- c("ID", "Response", "obs", "Mean", "Low", "High",  "p_UB_Anova", "SubGroup", "TopGroup" )
  
  k = 10000
  
  # first loop do CI for all data and all indicators **********************************
  
  for (i in 1:length(respcol) ) {
    # for (i in 24) {
    
    # i = 2
    subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                                , respcol[i], respcol[i]+1
                                , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                        |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
    
    subdata <- subdata[!is.na(subdata[,4]), ]
    
    # ============================================ values
    val1_response <- colnames(subdata)[4]
    val2_n_total <- length(subdata[,1])
    
    subdata$yi <- log(subdata[,5])-log(subdata[,4])
    
    
    colnames(subdata)
    
    if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
        | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
    {
      # is.finite(x) value change to min of yi
      subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                      , min(subdata[is.finite(subdata$yi),]$yi))
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # (1) unbalanced ANOVA for ALL data *********************************************************************
    # boots and get 95% CI for all
    # set.seed(123)
    
    val3_mean_all <- summary(quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975)) )[4] # get mean value
    val4_all_ci_low <- quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975))[1]
    val5_all_ci_high <- quantile(bootstrap(subdata$yi,k,mean)$thetastar,c(0.025,0.975))[2]
    
    
    
    submat <- cbind (i, val1_response, val2_n_total, val3_mean_all, val4_all_ci_low, val5_all_ci_high, NA, "All data", "All data")
    colnames (submat) <- colnames (mat)
    mat <- rbind(mat, submat)
    
    print(paste("**********", i, val1_response, "**********"))
    
  }
  
  #*****************************************************************************************************************
  # STEP 2.2 Unbalanced ANOVA for Climate
  #***************************************************************************************************************** 
  
  
  for (i in 1:length(respcol) ) {
    # for (i in 2) {
    
    # i = 2
    subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                                , respcol[i], respcol[i]+1
                                , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                        |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
    
    subdata <- subdata[!is.na(subdata[,4]), ]
    
    # ============================================ values
    val1_response <- colnames(subdata)[4]
    val2_n_total <- length(subdata[,1])
    
    subdata$yi <- log(subdata[,5])-log(subdata[,4])
    
    
    colnames(subdata)
    
    if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
        | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
    {
      # is.finite(x) value change to min of yi
      subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                      , min(subdata[is.finite(subdata$yi),]$yi))
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # (2) Unbalaced ANOVA test climate *********************************************************************
    # Unbalaced ANOVA
    try_anova1 <- try (Anova(lm(yi~Top_Climate,data = subdata), type="III") )
    if(inherits(try_anova1, "try-error"))  {
      val6_p_UB_anova_climate <- NA
    }
    else {
      val6_p_UB_anova_climate <- round(Anova(lm(yi~Top_Climate,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # boots and get 95% CI by climate using for loop
    # set.seed(123)
    
    # (3) second for loop test climate *********************************************************************
    
    for (j in 1:length(var_climate)) {
      # j = 1
      sub_climate <- subdata[subdata$Top_Climate == var_climate[j],]
      val7_n_climate <- length(sub_climate$yi)
      if (val7_n_climate == 0) {
        val8_mean_climate <- NA
        val9_low_climate<- NA
        val10_high_climate <- NA }
      else {
        val8_mean_climate <- summary(quantile(bootstrap(sub_climate$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val9_low_climate <- quantile(bootstrap(sub_climate$yi,k,mean)$thetastar,c(0.025,0.975))[1]
        val10_high_climate <- quantile(bootstrap(sub_climate$yi,k,mean)$thetastar,c(0.025,0.975))[2]
      }
      
      mat_climate <- cbind (i, val1_response, val7_n_climate, val8_mean_climate, val9_low_climate, val10_high_climate
                            , val6_p_UB_anova_climate, var_climate[j], "Climate")
      
      colnames(mat_climate) <- colnames (mat)
      mat <- rbind(mat, mat_climate)
      
      print(paste("**********", i, val1_response, "**********", j, var_climate[j], "climate **********"))
      
    }
    
  }
  
  # mat <- mat[1:38,]
  
  #*****************************************************************************************************************
  # STEP 2.3 Unbalanced ANOVA for Texture
  #***************************************************************************************************************** 
  
  
  for (i in 1:length(respcol) ) {
    # for (i in 2) {
    
    # i = 2
    subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                                , respcol[i], respcol[i]+1
                                , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                        |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
    
    subdata <- subdata[!is.na(subdata[,4]), ]
    subdata <- subdata %>% filter(TextureGroup != "NotAvailable")
    
    # ============================================ values
    val1_response <- colnames(subdata)[4]
    val2_n_total <- length(subdata[,1])
    
    subdata$yi <- log(subdata[,5])-log(subdata[,4])
    
    colnames(subdata)
    
    if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
        | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
    {
      # is.finite(x) value change to min of yi
      subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                      , min(subdata[is.finite(subdata$yi),]$yi))
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # (2) Unbalaced ANOVA test Texture *********************************************************************
    # Unbalaced ANOVA
    try_anova_var <- try (Anova(lm(yi~TextureGroup,data = subdata), type="III") )
    if(inherits(try_anova_var, "try-error"))  {
      val11_p_UB_anova_var <- NA
    }
    else {
      val11_p_UB_anova_var <- round(Anova(lm(yi~TextureGroup,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # boots and get 95% CI by Texture using for loop
    # set.seed(123)
    
    # (3) second for loop test Texture *********************************************************************
    
    for (j in 1:length(var_Texture)) {
      # j = 1
      sub_var <- subdata[subdata$TextureGroup == var_Texture[j],]
      val12_n_var <- length(sub_var$yi)
      if (val12_n_var == 0) {
        val13_mean_var <- NA
        val14_low_var <- NA
        val15_high_var <- NA }
      else {
        val13_mean_var <- summary(quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val14_low_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[1]
        val15_high_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[2]
      }
      
      mat_var <- cbind (i, val1_response, val12_n_var, val13_mean_var, val14_low_var, val15_high_var
                        , val11_p_UB_anova_var, var_Texture[j], "Texture")
      
      colnames(mat_var) <- colnames (mat)
      mat <- rbind(mat, mat_var)
      
      print(paste("**********", i, val1_response, "**********", j, var_Texture[j], "Texture **********"))
      
    }
    
  }
  
  #*****************************************************************************************************************
  # STEP 2.4 Unbalanced ANOVA for Tillage
  #***************************************************************************************************************** 
  
  for (i in 1:length(respcol) ) {
    # for (i in 2) {
    
    # i = 2
    subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                                , respcol[i], respcol[i]+1
                                , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                        |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
    
    subdata <- subdata[!is.na(subdata[,4]), ]
    
    # ============================================ values
    val1_response <- colnames(subdata)[4]
    val2_n_total <- length(subdata[,1])
    
    subdata$yi <- log(subdata[,5])-log(subdata[,4])
    
    colnames(subdata)
    
    if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
        | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
    {
      # is.finite(x) value change to min of yi
      subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                      , min(subdata[is.finite(subdata$yi),]$yi))
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # (2) Unbalaced ANOVA test Tillage *********************************************************************
    # Unbalaced ANOVA
    try_anova_var <- try (Anova(lm(yi~Tillage_Top_T, data = subdata), type="III") )
    if(inherits(try_anova_var, "try-error"))  {
      val11_p_UB_anova_var <- NA
    }
    else {
      val11_p_UB_anova_var <- round(Anova(lm(yi~Tillage_Top_T,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # boots and get 95% CI by Texture using for loop
    # set.seed(123)
    
    # (3) usecond for loop test Tillage *********************************************************************
    
    for (j in 1:length(var_tillage)) {
      # j = 1
      sub_var <- subdata[subdata$Tillage_Top_T == var_tillage[j],]
      val12_n_var <- length(sub_var$yi)
      if (val12_n_var == 0) {
        val13_mean_var <- NA
        val14_low_var <- NA
        val15_high_var <- NA }
      else {
        val13_mean_var <- summary(quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val14_low_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[1]
        val15_high_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[2]
      }
      
      mat_var <- cbind (i, val1_response, val12_n_var, val13_mean_var, val14_low_var, val15_high_var
                        , val11_p_UB_anova_var, var_tillage[j], "Tillage")
      
      colnames(mat_var) <- colnames (mat)
      mat <- rbind(mat, mat_var)
      
      print(paste("**********", i, val1_response, "**********", j, var_tillage[j], "Tillage **********"))
      
    }
  }
  
  #*****************************************************************************************************************
  # STEP 2.5 Unbalanced ANOVA for Cover crop
  #***************************************************************************************************************** 
  
  for (i in 1:length(respcol) ) {
    # for (i in 2) {
    
    # i = 2
    subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                                , respcol[i], respcol[i]+1
                                , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                        |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
    
    subdata <- subdata[!is.na(subdata[,4]), ]
    
    # ============================================ values
    val1_response <- colnames(subdata)[4]
    val2_n_total <- length(subdata[,1])
    
    subdata$yi <- log(subdata[,5])-log(subdata[,4])
    
    colnames(subdata)
    
    if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
        | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
    {
      # is.finite(x) value change to min of yi
      subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                      , min(subdata[is.finite(subdata$yi),]$yi))
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # (2) Unbalaced ANOVA test Cover crop *********************************************************************
    # Unbalaced ANOVA
    try_anova_var <- try (Anova(lm(yi~TopCoverCrop, data = subdata), type="III") )
    if(inherits(try_anova_var, "try-error"))  {
      val11_p_UB_anova_var <- NA
    }
    else {
      val11_p_UB_anova_var <- round(Anova(lm(yi~TopCoverCrop,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # boots and get 95% CI by Texture using for loop
    # set.seed(123)
    
    # (3) usecond for loop test Tillage *********************************************************************
    
    for (j in 1:length(var_CoverCrop)) {
      # j = 1
      sub_var <- subdata[subdata$TopCoverCrop == var_CoverCrop[j],]
      val12_n_var <- length(sub_var$yi)
      if (val12_n_var == 0) {
        val13_mean_var <- NA
        val14_low_var <- NA
        val15_high_var <- NA }
      else {
        val13_mean_var <- summary(quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val14_low_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[1]
        val15_high_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[2]
      }
      
      mat_var <- cbind (i, val1_response, val12_n_var, val13_mean_var, val14_low_var, val15_high_var
                        , val11_p_UB_anova_var, var_CoverCrop[j], "Cover crop")
      
      colnames(mat_var) <- colnames (mat)
      mat <- rbind(mat, mat_var)
      
      print(paste("**********", i, val1_response, "**********", j, var_CoverCrop[j], "Cover crop **********"))
      
    }
  }
  
  # mat <- mat[mat$TopGroup!="Cover crop",]
  
  #*****************************************************************************************************************
  # STEP 2.6 Unbalanced ANOVA for Grain crop
  #***************************************************************************************************************** 
  
  for (i in 1:length(respcol) ) {
    # for (i in 2) {
    
    # i = 2
    subdata <- SoilHealthCC[, c(which(colnames(SoilHealthCC) == 'StudyID'|colnames(SoilHealthCC) == 'ExperimentID'|colnames(SoilHealthCC) == 'Tillage_Top_T')
                                , respcol[i], respcol[i]+1
                                , which(colnames(SoilHealthCC) == 'Top_Climate'|colnames(SoilHealthCC) == 'TextureGroup'
                                        |colnames(SoilHealthCC) == 'TopCoverCrop'|colnames(SoilHealthCC) == 'TopGrainCrop') )]
    
    subdata <- subdata[!is.na(subdata[,4]), ]
    
    # ============================================ values
    val1_response <- colnames(subdata)[4]
    val2_n_total <- length(subdata[,1])
    
    subdata$yi <- log(subdata[,5])-log(subdata[,4])
    
    
    colnames(subdata)
    
    if (colnames (subdata)[4] == "Erosion_C" |colnames (subdata)[4] == "Runoff_C"| colnames (subdata)[4] == "Diseases_C"
        | colnames (subdata)[4] == "Leaching_C"| colnames (subdata)[4] == "Weed_C"| colnames (subdata)[4] == "Pests_C")
    {
      # is.finite(x) value change to min of yi
      subdata[is.infinite(subdata$yi),]$yi <- ifelse (subdata[is.infinite(subdata$yi),]$yi>0, max(subdata[is.finite(subdata$yi),]$yi)
                                                      , min(subdata[is.finite(subdata$yi),]$yi))
      # this line is not necessary anymore because NA has been removed
      # if (length(subdata[is.na(subdata$yi),]$yi) > 0) {subdata[is.na(subdata$yi),]$yi <- log(1)}
    }
    
    subdata <- subdata[!is.na(subdata$yi),]
    
    # head(subdata, 10)
    # hist(subdata$yi, col= "gray", breaks = 20)
    
    # (2) Unbalaced ANOVA test Cover crop *********************************************************************
    # Unbalaced ANOVA
    try_anova_var <- try (Anova(lm(yi~TopGrainCrop, data = subdata), type="III") )
    if(inherits(try_anova_var, "try-error"))  {
      val11_p_UB_anova_var <- NA
    }
    else {
      val11_p_UB_anova_var <- round(Anova(lm(yi~TopGrainCrop,data = subdata), type="III")$`Pr(>F)`[2], 4)
    }
    
    # boots and get 95% CI by Texture using for loop
    # set.seed(123)
    
    # (3) usecond for loop test Tillage *********************************************************************
    
    for (j in 1:length(var_GrainCrop)) {
      # j = 1
      sub_var <- subdata[subdata$TopGrainCrop == var_GrainCrop[j],]
      val12_n_var <- length(sub_var$yi)
      if (val12_n_var == 0) {
        val13_mean_var <- NA
        val14_low_var <- NA
        val15_high_var <- NA }
      else {
        val13_mean_var <- summary(quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975)))[4] # get mean value
        val14_low_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[1]
        val15_high_var <- quantile(bootstrap(sub_var$yi,k,mean)$thetastar,c(0.025,0.975))[2]
      }
      
      mat_var <- cbind (i, val1_response, val12_n_var, val13_mean_var, val14_low_var, val15_high_var
                        , val11_p_UB_anova_var, var_GrainCrop[j], "Grain crop")
      
      colnames(mat_var) <- colnames (mat)
      mat <- rbind(mat, mat_var)
      
      print(paste("**********", i, val1_response, "**********", j, var_GrainCrop[j], "***** Grain crop **********"))
      
    }
  }
  return (mat)
}

