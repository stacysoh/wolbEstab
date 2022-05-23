########################################################################
######################## FIGURE 2 ######################################
########################################################################
rm(list=ls())
load("C:/Users/stacy/OneDrive/Desktop/Work/wolb/wolbresults1000simul_diffinterventiondates_condensed_for_manu_figs.RData")
#release (suppression) start: t=20
#release (suppression) stop = 1820
#intervention start: t=1700
#intervention end: t=2400
#start of intervention overlaps with release

########################
### wAlbB FEMALE ###
########################

png(file="C:/Users/stacy/OneDrive/Desktop/Work/wolb/fig2.png",units='in',height=16,width=15,res=150)
par(las=1,mfrow=c(2,2),mar = c(3.5, 4.9, 2, 0.8),pty='s')

plot(x=c(0,3500),y=range(femaleMosW1_supp), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n",
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=2.3)
axis(1, at=seq(0, 3500, by=500), labels=seq(0, 35, by=5), cex.axis=2.3, 
     mgp=c(3,1.4,0))
title(ylab="wAlbB Female Population ('000)", line=2.9, cex.lab=2.3)
title(xlab="Time (Days, '00)", line=3.6, cex.lab=2.3)
mytitle = "A"
mtext(side=3, line=0.7, at=1, adj=0, cex=2.5, mytitle)

for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#suppresion with ehi's error
for (i in 1:ncol(femaleMosW1_supp))
{lines(x=c(0:3500),y=femaleMosW1_supp[,i], col=rgb(158/255,158/255,158/255,alpha=0.04))}
femaleMosW1_supp_median = apply(femaleMosW1_supp, 1, median, na.rm=T)
lines(femaleMosW1_supp_median, col="grey37", lwd=2)

legend(x="topleft",legend=c(expression(paste("S2, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('grey37','red3'),
       lwd=5, cex=2.1) #, inset=c(-0.37, 0)


##############

plot(x=c(0,3500),y=range(femaleMosW1_supp), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n",
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=2.3)
axis(1, at=seq(0, 3500, by=500), labels=seq(0, 35, by=5), cex.axis=2.3, 
     mgp=c(3,1.4,0))
title(ylab="", line=2.9, cex.lab=2.3)
title(xlab="Time (Days, '00)", line=3.6, cex.lab=2.3)
mytitle = "B"
mtext(side=3, line=0.7, at=1, adj=0, cex=2.5, mytitle)

for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#suppression with verily's error
for (i in 1:ncol(femaleMosW1_supp_v))
{lines(x=c(0:3500),y=femaleMosW1_supp_v[,i], col=rgb(139/255,69/255,19/255,alpha=0.04))}
femaleMosW1_supp_v_median = apply(femaleMosW1_supp_v, 1, median, na.rm=T)
lines(femaleMosW1_supp_v_median, col="chocolate4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S2, FRER: ", "10"^"-9")),"S1"),
       bty='n',
       col=c('chocolate4','red3'),
       lwd=5, cex=2.1) #, inset=c(-0.37, 0)

#legend(x="topleft",legend=c("S1", expression(paste("S2, FRER :", "10"^"-3")), 
#                            expression(paste("S2, FRER: ", "10"^"-9"))),
#       bty='n',
#       col=c('red3','grey37', 'chocolate4'),
#      lwd=5, cex=2) #, inset=c(-0.37, 0)



########################
### wild FEMALE ###
########################
plot(x=c(0,3500),y=range(femaleMosU_supp), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n",
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=2.3)
axis(1, at=seq(0, 3500, by=500), labels=seq(0, 35, by=5), cex.axis=2.3, 
     mgp=c(3,1.4,0))
title(ylab="Wild Female Population ('000)", line=2.9, cex.lab=2.3)
title(xlab="Time (Days, '00)", line=3.6, cex.lab=2.3)
mytitle = "C"
mtext(side=3, line=0.7, at=1, adj=0, cex=2.5, mytitle)

for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

legend(x="topleft",legend=c(expression(paste("S2, FRER: ", "10"^"-3")),"S1"),
       bty='n',
       col=c('grey37','red3'),
       lwd=5, cex=2.1) #, inset=c(-0.37, 0)

#suppresion with ehi's error
for (i in 1:ncol(femaleMosU_supp))
{lines(x=c(0:3500),y=femaleMosU_supp[,i], col=rgb(158/255,158/255,158/255,alpha=0.04))}
femaleMosU_supp_median = apply(femaleMosU_supp, 1, median, na.rm=T)
lines(femaleMosU_supp_median, col="grey37", lwd=2)

##############

plot(x=c(0,3500),y=range(femaleMosU_supp), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n",
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=2.3)
axis(1, at=seq(0, 3500, by=500), labels=seq(0, 35, by=5), cex.axis=2.3, 
     mgp=c(3,1.4,0))
title(ylab="", line=2.9, cex.lab=2.3)
title(xlab="Time (Days, '00)", line=3.6, cex.lab=2.3)
mytitle = "D"
mtext(side=3, line=0.7, at=1, adj=0, cex=2.5, mytitle)

for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#suppression with verily's error
for (i in 1:ncol(femaleMosU_supp_v))
{lines(x=c(0:3500),y=femaleMosU_supp_v[,i], col=rgb(139/255,69/255,19/255,alpha=0.04))}
femaleMosU_supp_v_median = apply(femaleMosU_supp_v, 1, median, na.rm=T)
lines(femaleMosU_supp_v_median, col="chocolate4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S2, FRER: ", "10"^"-9")),"S1"),
       bty='n',
       col=c('chocolate4','red3'),
       lwd=5, cex=2.1) #, inset=c(-0.37, 0)


dev.off()





########################################################################
######################## FIGURE 3 ######################################
########################################################################



#########################################
##First strain female irradiation (S3A)##
#########################################

png(file="C:/Users/stacy/OneDrive/Desktop/Work/wolb/fig3.png",units='in',height=24,width=22,res=150)
#par(las=1,mfrow=c(3,3),mar = c(3.5, 7.3, 2, 0.8),pty='s') #bottom left top right
par(las=1,mfrow=c(3,3),mar = c(7.8, 7.3, 3.8, 0.8),pty='s') #bottom left top right

plot(x=c(0,3500),y=range(femaleMosU_overfloodingwerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="Wild Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "A"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date

for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#overflooding with error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosU_overfloodingwerror))
{lines(x=c(0:3500),y=femaleMosU_overfloodingwerror[,i],
       col=rgb(139/255,71/255,137/255,alpha=0.03))}
femaleMosU_overfloodingwerror_median = apply(femaleMosU_overfloodingwerror, 1, median, na.rm=T)
lines(femaleMosU_overfloodingwerror_median, col="orchid4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('orchid4','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

##############

plot(x=c(0,3500),y=range(femaleMosU_overfloodingwerror_verily), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 30, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "B"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date

for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)


#overflooding with verily error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosU_overfloodingwerror_verily))
{lines(x=c(0:3500),y=femaleMosU_overfloodingwerror_verily[,i],
       col=rgb(238/255,130/255,238/255,alpha=0.03))}
femaleMosU_overfloodingwerror_verily_median = apply(femaleMosU_overfloodingwerror_verily, 1, median, na.rm=T)
lines(femaleMosU_overfloodingwerror_verily_median, col="violetred4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('violetred4', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)



##############
plot(x=c(0,3500),y=range(femaleMosU_overfloodingnoerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 30, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "C"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date

for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#overflooding with no error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosU_overfloodingnoerror))
{lines(x=c(0:3500),y=femaleMosU_overfloodingnoerror[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosU_overfloodingnoerror_median = apply(femaleMosU_overfloodingnoerror, 1, median, na.rm=T)
lines(femaleMosU_overfloodingnoerror_median, col="orange", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A, FRER :", "0")),"S1"),
       bty='n',
       col=c('orange', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


##################################################
##First strain male and female irradiation (S3B)##
##################################################

plot(x=c(0,3500),y=range(femaleMosU_overfloodingwerror_sit), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="Wild Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "D"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)


#overflooding with error (IIT-SIT), SIT=T
for (i in 1:ncol(femaleMosU_overfloodingwerror_sit))
{lines(x=c(0:3500),y=femaleMosU_overfloodingwerror_sit[,i],
       col=rgb(139/255,71/255,137/255,alpha=0.03))}
femaleMosU_overfloodingwerror_sit_median = apply(femaleMosU_overfloodingwerror_sit, 1, median, na.rm=T)
lines(femaleMosU_overfloodingwerror_sit_median, col="orchid4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('orchid4', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


##############

plot(x=c(0,3500),y=range(femaleMosU_overfloodingwerror_verily_sit), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 30, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "E"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#overflooding with verily error (IIT-SIT), SIT=T
for (i in 1:ncol(femaleMosU_overfloodingwerror_verily_sit))
{lines(x=c(0:3500),y=femaleMosU_overfloodingwerror_verily_sit[,i],
       col=rgb(238/255,130/255,238/255,alpha=0.03))}
femaleMosU_overfloodingwerror_verily_sit_median = apply(femaleMosU_overfloodingwerror_verily_sit, 1, median, na.rm=T)
lines(femaleMosU_overfloodingwerror_verily_sit_median, col="violetred4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('violetred4','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


##############
plot(x=c(0,3500),y=range(femaleMosU_overfloodingnoerror_sit), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 30, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "F"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#overflooding with no error (IIT-SIT), SIT=T
for (i in 1:ncol(femaleMosU_overfloodingnoerror_sit))
{lines(x=c(0:3500),y=femaleMosU_overfloodingnoerror_sit[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosU_overfloodingnoerror_sit_median = apply(femaleMosU_overfloodingnoerror_sit, 1, median, na.rm=T)
lines(femaleMosU_overfloodingnoerror_sit_median, col="orange", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B, FRER :", "0")),"S1"),
       bty='n',
       col=c('orange', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)



###################################
##Second strain introduction (S4)##
###################################

plot(x=c(0,3500),y=range(femaleMosU_2ndstrainwerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="Wild Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "G"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#overflooding with error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosU_2ndstrainwerror))
{lines(x=c(0:3500),y=femaleMosU_2ndstrainwerror[,i],
       col=rgb(139/255,71/255,137/255,alpha=0.03))}
femaleMosU_2ndstrainwerror_median = apply(femaleMosU_2ndstrainwerror, 1, median, na.rm=T)
lines(femaleMosU_2ndstrainwerror_median, col="orchid4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('orchid4','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

##############

plot(x=c(0,3500),y=range(femaleMosU_2ndstrainwerror_verily), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 30, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "H"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)


#overflooding with verily error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosU_2ndstrainwerror_verily))
{lines(x=c(0:3500),y=femaleMosU_2ndstrainwerror_verily[,i],
       col=rgb(238/255,130/255,238/255,alpha=0.03))}
femaleMosU_2ndstrainwerror_verily_median = apply(femaleMosU_2ndstrainwerror_verily, 1, median, na.rm=T)
lines(femaleMosU_2ndstrainwerror_verily_median, col="violetred4", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('violetred4','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

##############
plot(x=c(0,3500),y=range(femaleMosU_overfloodingnoerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 30, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "I"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3", lwd=2)

#overflooding with no error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosU_2ndstrainnoerror))
{lines(x=c(0:3500),y=femaleMosU_2ndstrainnoerror[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosU_2ndstrainnoerror_median = apply(femaleMosU_2ndstrainnoerror, 1, median, na.rm=T)
lines(femaleMosU_2ndstrainnoerror_median, col="orange", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "0")),"S1"),
       bty='n',
       col=c('orange','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


dev.off()




########################################################################
######################## FIGURE 4 ######################################
########################################################################


#########################################
##First strain female irradiation (S3A)##
#########################################

png(file="C:/Users/stacy/OneDrive/Desktop/Work/wolb/fig4.png",units='in',height=24,width=22,res=150)
#par(las=1,mfrow=c(3,3),mar = c(3.5, 7.3, 2, 0.8),pty='s') #bottom left top right
par(las=1,mfrow=c(3,3),mar = c(7.8, 7.3, 3.8, 0.8),pty='s') #bottom left top right

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingwerror_verily), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wAlbB Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "A"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with error (IIT-SIT), SIT=F
   #for (i in 1:ncol(femaleMosW1_overfloodingwerror))
   #{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror[,i],
   #       col=rgb(100/255,149/255,237/255,alpha=0.03))}
   #femaleMosW1_overfloodingwerror_median = apply(femaleMosW1_overfloodingwerror, 1, median, na.rm=T)
   #lines(femaleMosW1_overfloodingwerror_median, col="cornflowerblue", lwd=2)

for (i in 1:ncol(femaleMosW1_overfloodingwerror_verily))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror_verily[,i],
       col=rgb(100/255,149/255,237/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_verily_median = apply(femaleMosW1_overfloodingwerror_verily, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_verily_median, col="cornflowerblue", lwd=2)


legend(x="topleft",legend=c(expression(paste("S3A, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('cornflowerblue','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

##############


plot(x=c(0,3500),y=range(femaleMosW1_overfloodingwerror_verily), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "B"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)


#overflooding with verily error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosW1_overfloodingwerror_verily))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror_verily[,i],
       col=rgb(154/255,192/255,205/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_verily_median = apply(femaleMosW1_overfloodingwerror_verily, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_verily_median, col="lightblue3", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('lightblue3', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


##############

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "C"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosW1_overfloodingnoerror))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror[,i],
       col=rgb(46/255,139/255,87/255,alpha=0.03))}
femaleMosW1_overfloodingnoerror_median = apply(femaleMosW1_overfloodingnoerror, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_median, col="seagreen", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A, FRER :", "0")),"S1"),
       bty='n',
       col=c('seagreen', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


##################################################
##First strain male and female irradiation (S3B)##
##################################################

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingwerror_sit), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wAlbB Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "D"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosW1_overfloodingwerror_sit))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror_sit[,i],
       col=rgb(100/255,149/255,237/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_sit_median = apply(femaleMosW1_overfloodingwerror_sit, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_sit_median, col="cornflowerblue", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('cornflowerblue', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

############

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingwerror_verily_sit), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "E"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with verily error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosW1_overfloodingwerror_verily_sit))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror_verily_sit[,i],
       col=rgb(154/255,192/255,205/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_verily_sit_median = apply(femaleMosW1_overfloodingwerror_verily_sit, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_verily_sit_median, col="lightblue3", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('lightblue3', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

#############

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_sit), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "F"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (IIT-SIT), SIT=F
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit[,i],
       col=rgb(46/255,139/255,87/255,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_median = apply(femaleMosW1_overfloodingnoerror_sit, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_median, col="seagreen", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B, FRER :", "0")),"S1"),
       bty='n',
       col=c('seagreen', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


###################################
##Second strain introduction (S4)##
###################################


plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainwerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wAlbB Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "G"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#second strain with error
for (i in 1:ncol(femaleMosW1_2ndstrainwerror))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainwerror[,i],
       col=	rgb(100/255,149/255,237/255,alpha=0.03))}
femaleMosW1_2ndstrainwerror_median = apply(femaleMosW1_2ndstrainwerror, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainwerror_median, col="cornflowerblue", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('cornflowerblue', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

########

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainwerror_verily), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "H"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#second strain with verily error
for (i in 1:ncol(femaleMosW1_2ndstrainwerror_verily))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainwerror_verily[,i],
       col=	rgb(154/255,192/255,205/255,alpha=0.03))}
femaleMosW1_2ndstrainwerror_verily_median = apply(femaleMosW1_2ndstrainwerror_verily, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainwerror_verily_median, col="lightblue3", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('lightblue3', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


#########

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainnoerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "I"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#second strain with no error
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror[,i],
       col=rgb(46/255,139/255,87/255,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_median = apply(femaleMosW1_2ndstrainnoerror, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_median, col="seagreen", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "0")),"S1"),
       bty='n',
       col=c('seagreen', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)



dev.off()

########################################################################
######################## FIGURE 5 ######################################
########################################################################

png(file="C:/Users/stacy/OneDrive/Desktop/Work/wolb/fig5.png",units='in',height=8,width=22,res=150)
#par(las=1,mfrow=c(3,3),mar = c(3.5, 7.3, 2, 0.8),pty='s') #bottom left top right
par(las=1,mfrow=c(1,3),mar = c(7.8, 7.3, 3.8, 0.8),pty='s') #bottom left top right


###wMEl####
plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainwerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wMel Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "A"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


femaleMosW2_baseline_median = apply(femaleMosW2_baseline, 1, median, na.rm=T)
lines(femaleMosW2_baseline_median, col="red3", lwd=2)

#second strain with error
for (i in 1:ncol(femaleMosW2_2ndstrainwerror))
{lines(x=c(0:3500),y=femaleMosW2_2ndstrainwerror[,i],
       col=	rgb(100/255,149/255,237/255,alpha=0.03))}
femaleMosW2_2ndstrainwerror_median = apply(femaleMosW2_2ndstrainwerror, 1, median, na.rm=T)
lines(femaleMosW2_2ndstrainwerror_median, col="cornflowerblue", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "10"^"-3")),"S1"),
       bty='n',
       col=c('cornflowerblue','red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


######

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainwerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "B"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


femaleMosW2_baseline_median = apply(femaleMosW2_baseline, 1, median, na.rm=T)
lines(femaleMosW2_baseline_median, col="red3", lwd=2)

#second strain with verily error
for (i in 1:ncol(femaleMosW2_2ndstrainwerror_verily))
{lines(x=c(0:3500),y=femaleMosW2_2ndstrainwerror_verily[,i],
       col=	rgb(154/255,192/255,205/255,alpha=0.03))}
femaleMosW2_2ndstrainwerror_verily_median = apply(femaleMosW2_2ndstrainwerror_verily, 1, median, na.rm=T)
lines(femaleMosW2_2ndstrainwerror_verily_median, col="lightblue3", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "10"^"-9")),"S1"),
       bty='n',
       col=c('lightblue3', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

######

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainwerror), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "C"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


femaleMosW2_baseline_median = apply(femaleMosW2_baseline, 1, median, na.rm=T)
lines(femaleMosW2_baseline_median, col="red3", lwd=2)

#second strain with no error
for (i in 1:ncol(femaleMosW2_2ndstrainnoerror))
{lines(x=c(0:3500),y=femaleMosW2_2ndstrainnoerror[,i],
       col=rgb(46/255,139/255,87/255,alpha=0.03))}
femaleMosW2_2ndstrainnoerror_median = apply(femaleMosW2_2ndstrainnoerror, 1, median, na.rm=T)
lines(femaleMosW2_2ndstrainnoerror_median, col="seagreen", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4, FRER :", "0")),"S1"),
       bty='n',
       col=c('seagreen', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)
dev.off()





########################################################################
######################## FIGURE 6 ######################################
########################################################################
load("C:/Users/stacy/OneDrive/Desktop/Work/wolb/wolbresults1000simul.RData")

#########################################
##First strain female irradiation (S3A)##
#########################################

png(file="C:/Users/stacy/OneDrive/Desktop/Work/wolb/fig6.png",units='in',height=24,width=22,res=150)
#par(las=1,mfrow=c(3,3),mar = c(3.5, 7.3, 2, 0.8),pty='s') #bottom left top right
par(las=1,mfrow=c(3,3),mar = c(7.8, 7.3, 3.8, 0.8),pty='s') #bottom left top right

######

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wAlbB Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "A"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_1.3))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_1.3[,i],
       col=rgb(255/255,193/255,37/255,alpha=0.03))}
femaleMosW1_overfloodingnoerror_1.3_median = apply(femaleMosW1_overfloodingnoerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_1.3_median, col="goldenrod1", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A [I], FRER :", "0")), "S1"),
       bty='n',
       col=c('goldenrod1', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)



######

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "B"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror[,i],
       col=rgb(255/255,140/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_median = apply(femaleMosW1_overfloodingnoerror, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_median, col="darkorange", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A [II], FRER :", "0")),"S1"),
       bty='n',
       col=c('darkorange', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


######

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "C"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_1.6))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_1.6[,i],
       col=rgb(255/255,69/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_1.6_median = apply(femaleMosW1_overfloodingnoerror_1.6, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_1.6_median, col="orangered", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3A [III], FRER :", "0")),"S1"),
       bty='n',
       col=c('orangered', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


##################################################
##First strain male and female irradiation (S3B)##
##################################################

####

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_sit_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wAlbB Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "D"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)


#overflooding with no error (intIntensity=4), SIT=T
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit_1.3))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit_1.3[,i],
       col=rgb(255/255,193/255,37/255,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_1.3_median = apply(femaleMosW1_overfloodingnoerror_sit_1.3, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_1.3_median, col="goldenrod1", lwd=2)


legend(x="topleft",legend=c(expression(paste("S3B [I], FRER :", "0")),"S1"),
       bty='n',
       col=c('goldenrod1', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


####

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_sit_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "E"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit[,i],
       col=rgb(255/255,140/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_median = apply(femaleMosW1_overfloodingnoerror_sit, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_median, col="darkorange", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B [II], FRER :", "0")),"S1"),
       bty='n',
       col=c('darkorange', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)


####

plot(x=c(0,3500),y=range(femaleMosW1_overfloodingnoerror_sit_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "F"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#overflooding with no error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit_1.6))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit_1.6[,i],
       col=rgb(255/255,69/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_1.6_median = apply(femaleMosW1_overfloodingnoerror_sit_1.6, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_1.6_median, col="orangered", lwd=2)

legend(x="topleft",legend=c(expression(paste("S3B [III], FRER :", "0")),"S1"),
       bty='n',
       col=c('orangered', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

###################################
##Second strain introduction (S4)##
###################################

######

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainnoerror_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="wAlbB Female Population ('000)", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "G"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#second strain with no error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror_1.3))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror_1.3[,i],
       col=rgb(255/255,193/255,37/255,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_1.3_median = apply(femaleMosW1_2ndstrainnoerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_1.3_median, col="goldenrod1", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4 [I], FRER :", "0")),"S1"),
       bty='n',
       col=c('goldenrod1', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

######

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainnoerror_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "H"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#second strain with no error  (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror[,i],
       col=rgb(255/255,140/255,0,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_median = apply(femaleMosW1_2ndstrainnoerror, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_median, col="darkorange", lwd=2)

legend(x="topleft",legend=c(expression(paste("S4 [II], FRER :", "0")),"S1"),
       bty='n',
       col=c('darkorange', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

######

plot(x=c(0,3500),y=range(femaleMosW1_2ndstrainnoerror_1.3), #range(femaleMosU_supp) c(0,2200)
     col="white", las=1, xlab="", ylab="", 
     yaxt="n", xaxt="n", 
     main = "", adj = 0, cex.main=2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), labels=c(0,1,2,3,4), cex.axis=3.6)
axis(1, at=seq(0, 3500, by=1000), labels=seq(0, 35, by=10), cex.axis=3.6, 
     mgp=c(3,2.5,0))
title(ylab="", line=4.9, cex.lab=3.6) #cex.lab=1.4
title(xlab="Time (Days, '00)", line=6, cex.lab=3.6) #cex.lab=1.4
mytitle = "I"
mtext(side=3, line=0.7, at=1, adj=0, cex=3.1, mytitle)

abline(v=1700,col=4,lty=2, lwd=2)
abline(v=2400, col='chocolate4', lty=2, lwd=2) #added intervention end date


for (i in 1:ncol(femaleMosW1_baseline))
{lines(x=c(0:3500),y=femaleMosW1_baseline[,i], col=rgb(1,0,0,alpha=0.01))}
femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3", lwd=2)

#second strain with no error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror_1.6))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror_1.6[,i],
       col=rgb(255/255,69/255,0,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_1.6_median = apply(femaleMosW1_2ndstrainnoerror_1.6, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_1.6_median, col="orangered", lwd=2)


legend(x="topleft",legend=c(expression(paste("S4 [III], FRER :", "0")),"S1"),
       bty='n',
       col=c('orangered', 'red3'),
       lwd=5, cex=3, x.intersp = 0.4) #, inset=c(-0.37, 0)

dev.off()

