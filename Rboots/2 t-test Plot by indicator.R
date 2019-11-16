
#*****************************************************************************************************************
# Step 1.1: prepare data
#*****************************************************************************************************************


sum_data <- read.csv("outputs/2.SoilHealthIdicator_boot.csv")

head(sum_data)

class(sum_data$ID)

# sum_data <- sum_data[order(sum_data$ID, decreasing = T),]

unique (sum_data$All)


#*****************************************************************************************************************
# Step 2 Plot
#*****************************************************************************************************************

#1.1 plot meta forest 

# tiff("outputs/2 boot-test by indicator.tiff", width = 8, height = 10, pointsize = 1/300, units = 'in', res = 300)
# pdf("Manuscript/Datainbrief/Figures/Figure3.pdf", width = 8, height = 10)
png("Manuscript/Datainbrief/Figures/Figure3.png", width = 8, height = 10)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,1))

# meta_II with predicted SD
x_min <- min(sum_data$boot_CI1, na.rm = T)
# x_max <- max(sum_data$boot_CI2, na.rm = T)
x_max <- 160

sum_data$col <- as.character(sum_data$col)

plot(sum_data$ID ~ sum_data$boot_mean, lwd=2
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


arrows(sum_data$boot_CI1, sum_data$ID, sum_data$boot_CI2, sum_data$ID
       ,code=3,length=0.05,angle=90,sum_data$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-80, 230, 40), labels = seq(-80, 230, 40)
      , cex=1
      , las = 1)

axis (side = 2, at = sum_data$ID, labels = paste(sum_data$Response, sep = "")
      , cex=1
      , las = 2)


text(ifelse(sum_data$boot_CI2 > 120, sum_data$boot_CI1-20, sum_data$boot_CI2+2.5), sum_data$ID
     , paste(sum_data$n_obs,"/",sum_data$n_study, sep = ""), cex=0.85, adj=0)


text(110,42.75,"(a) Physical", cex=1, adj=0)
text(110,34.25,"(b) Chemical", cex=1, adj=0)
text(110,25.25,"(c) Biological", cex=1, adj=0)
text(110,11.25,"(d) Environmental", cex=1, adj=0)
text(110,2.25,"(e) Agronomic", cex=1, adj=0)


abline(h=c(3, 12, 26, 35), col="black", lty=3, lwd=2)

mtext(side = 1, text = paste("Soil health indicator (% changes)", sep=" "), line = 1, cex=1.05, outer = T, adj = 0.7)

dev.off()


#*****************************************************************************************************************
#END
#*****************************************************************************************************************

