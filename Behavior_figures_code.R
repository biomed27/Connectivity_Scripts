library(ANTsR)
library(knitr)
behavior <- read.csv("/Users/omega/Documents/Natalie/SCCAN_tutorial/Eig_zscore/data/All_Behavior.csv")
adind <-c(1,2,8,9,10,12,13,14,15,16,19)
conind<-c(3,4,5,6,7,11,17,18,20,21,22,23,24)
ad_behavior <-behavior[adind,]
con_behavior <-behavior[conind,]

#figure 1 Swim Distance, Swim time, swim velocity
a <- c(0.6,1.6,2.6,3.6,4.6)
b<-a-.1
#png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_Distance_ConvsNOS2.png')
ad_dist <-ad_behavior[, c(2:6)]
cd_dist <-con_behavior[, c(2:6)]
time <-1:5
one<-boxplot(ad_dist, ylab = 'Swim Distance (cm)', border="red", boxwex = 0.35 ,outline = T, ylim = c(0,1400))
med_addist1 <- median(ad_dist$d1)
med_addist2 <- median(ad_dist$d2)
med_addist3 <- median(ad_dist$d3)
med_addist4 <- median(ad_dist$d4)
med_addist5 <- median(ad_dist$d5)
med_addists<-c(med_addist1,med_addist2,med_addist3,med_addist4,med_addist5)
abline(lm(med_addists~time), lty =  1 , lwd = 2 , col = 'red')
#dists<-c(1,2,3,4,5)
#lines(dists,med_addists,col ="red")
two<-boxplot(cd_dist, ylab = 'Swim Distance (cm)', main = "Average Swim Distance Traveled", border="black", xaxt='n', boxwex = 0.35 , add = T, at = a, outline = T)
med_cddist1 <- median(cd_dist$d1)
med_cddist2 <- median(cd_dist$d2)
med_cddist3 <- median(cd_dist$d3)
med_cddist4 <- median(cd_dist$d4)
med_cddist5 <- median(cd_dist$d5)
med_cddists<-c(med_cddist1,med_cddist2,med_cddist3,med_cddist4,med_cddist5)
abline(lm(med_cddists~time), lty =  1 , lwd = 2 , col = 'black')
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
# lines(a,med_cddists,col ="black")
#dev.off()


png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_Time_ConvsNOS2.png')
ad_time <-ad_behavior[, c(7:11)]
cd_time <-con_behavior[, c(7:11)]
boxplot(ad_time, ylab = 'Swim Time (sec)', border="red", boxwex = 0.35, outline = T, ylim= c(0,70))
med_adtime1 <- median(ad_time$s1)
med_adtime2 <- median(ad_time$s2)
med_adtime3 <- median(ad_time$s3)
med_adtime4 <- median(ad_time$s4)
med_adtime5 <- median(ad_time$s5)
med_adtimes<-c(med_adtime1,med_adtime2,med_adtime3,med_adtime4,med_adtime5)
dists<-c(1,2,3,4,5)
lines(dists,med_adtimes,col ="red")
boxplot(cd_time, ylab = 'Swim Time (sec)', main = "Average Swim Time",  border="black",xaxt='n', boxwex = 0.35 , add = T, at = b, outline = T)
med_cdtime1 <- median(cd_time$s1)
med_cdtime2 <- median(cd_time$s2)
med_cdtime3 <- median(cd_time$s3)
med_cdtime4 <- median(cd_time$s4)
med_cdtime5 <- median(cd_time$s5)
med_cdtimes<-c(med_cdtime1,med_cdtime2,med_cdtime3,med_cdtime4,med_cdtime5)
lines(a,med_cdtimes,col ="black")
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
dev.off()

png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_Velocity_ConvsNOS2.png')
ad_vel <-ad_behavior[, c(12:16)]
cd_vel <-con_behavior[, c(12:16)]
boxplot(ad_vel, ylab = 'Swim Velocity (cm/sec)', border="red", boxwex = 0.35, outline = T,ylim = c(10,25))
med_advel1 <- median(ad_vel$v1)
med_advel2 <- median(ad_vel$v2)
med_advel3 <- median(ad_vel$v3)
med_advel4 <- median(ad_vel$v4)
med_advel5 <- median(ad_vel$v5)
med_advels<-c(med_advel1,med_advel2,med_advel3,med_advel4,med_advel5)
dists<-c(1,2,3,4,5)
lines(dists,med_advels,col ="red")
boxplot(cd_vel, ylab = 'Swim Velocity (cm/sec)', main = "Average Swim Velocity", border="black", xaxt='n', boxwex = 0.35 , add = T, at = a, outline = T)
med_cdvel1 <- median(cd_vel$v1)
med_cdvel2 <- median(cd_vel$v2)
med_cdvel3 <- median(cd_vel$v3)
med_cdvel4 <- median(cd_vel$v4)
med_cdvel5 <- median(cd_vel$v5)
med_cdvels<-c(med_cdvel1,med_cdvel2,med_cdvel3,med_cdvel4,med_cdvel5)
lines(a,med_cdvels,col ="black")
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
dev.off()


#Swim % Proxmity Day 3 Day 5
probe.data <- read.csv('/Users/omega/Documents/Natalie/SCCAN_tutorial/PROBE_Prox35.csv')
d <- 1.3
ad_prox3 <-probe.data[adind, ]
cd_prox3 <-probe.data[conind, ]
ad_prox33 <-ad_prox3[ ,c(1)]
cd_prox33 <-cd_prox3[ ,c(1)]
#png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_ProximityDay3_ConvsNOS2.png')
boxplot(ad_prox33, ylab = 'Percent (%)', border="red", boxwex = 0.35, outline = T, ylim = c(0,16))
boxplot(cd_prox33, ylab = 'Percent (%)', main = "Percentage of Time Spent in Target Zone on Day 3", border="black", xaxt='n', boxwex = 0.35 , add = T, at = d, outline = T)
y <- 15
# set an offset for tick lengths
offset <- 0.5
# draw first horizontal line
lines(c(1,1.3),c(y, y))
# draw ticks
lines(c(1,1),c(y, y-offset))
lines(c(1.3,1.3),c(y, y-offset))
# draw asterics
text(1.15,y+offset, "*")
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
#dev.off()

ad_prox5 <-ad_prox3[ ,c(2)]
cd_prox5 <-cd_prox3[ ,c(2)]
#png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_ProximityDay5_ConvsNOS2.png')
boxplot(ad_prox5, ylab = 'Percent (%)', border="red", boxwex = 0.35, outline = T, ylim = c(0,16))
boxplot(cd_prox5, ylab = 'Percent (%)', main = "Percentage of Time Spent in Target Zone on Day 5", border="black", xaxt='n', boxwex = 0.35 , add = T, at = d, outline = T)
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
#dev.off()

total.dist.prox <-read.csv('/Users/omega/Documents/Natalie/SCCAN_tutorial/TotalDistance_Prox35.csv')
ad_dist3 <-total.dist.prox[adind, ]
cd_dist3 <-total.dist.prox[conind, ]
ad_dist33 <-ad_dist3[ ,c(1)]
cd_dist33 <-cd_dist3[ ,c(1)]
png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_TotalDistDay3_ConvsNOS2.png')
boxplot(ad_dist33, ylab = 'Distance (cm)', border="red", boxwex = 0.35, outline = T, ylim = c(45000,105000))
boxplot(cd_dist33, ylab = 'Distance (cm)', main = "Total Distance to Platform on Day 3", border="black", xaxt='n', boxwex = 0.35 , add = T, at = d, outline = T)
y <- 1e05
# set an offset for tick lengths
offset <- 2000
# draw first horizontal line
lines(c(1,1.3),c(y, y))
# draw ticks
lines(c(1,1),c(y, y-offset))
lines(c(1.3,1.3),c(y, y-offset))
# draw asterics
text(1.15, y+offset, "*")
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
dev.off()

ad_dist5 <-ad_dist3[ ,c(2)]
cd_dist5 <-cd_dist3[ ,c(2)]
#png(filename = '/Users/omega/Documents/Natalie/Thesis/Figures/figures_boxplots/Swim_TotalDistDay5_ConvsNOS2.png')
boxplot(ad_dist5, ylab = 'Distance (cm)', border="red", boxwex = 0.35, outline = T, ylim = c(35000,90000))
boxplot(cd_dist5, ylab = 'Distance (cm)', main = "Total Distance to Platform on Day 5", border="black", xaxt='n', boxwex = 0.35 , add = T, at = d, outline = T)
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
#dev.off()

total.entries <- read.csv('/Users/omega/Documents/Natalie/SCCAN_tutorial/Total_Entries35.csv')
ad_entry3 <-total.entries[adind, ]
cd_entry3 <-total.entries[conind, ]
ad_entry33 <-ad_entry3[ ,c(1)]
cd_entry33 <-cd_entry3[ ,c(1)]
#png(filename = '/Users/omega/Documents/Natalie/mypaper/Swim_TotalEntries3_ConvsNOS2.png')
boxplot(ad_entry33, ylab = 'Total Number of Entries to Platform Day3', border="red", boxwex = 0.35, outline = F, ylim = c(0,10))
boxplot(cd_entry33, ylab = 'Total Number of Entries to Platform Day3', border="black", xaxt='n', boxwex = 0.35 , add = T, at = d, outline = F)
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
#dev.off()

ad_entry5 <-ad_entry3[ ,c(2)]
cd_entry5 <-cd_entry3[ ,c(2)]
#png(filename = '/Users/omega/Documents/Natalie/mypaper/Swim_TotalEntries_Day5_ConvsNOS2.png')
boxplot(ad_entry5, ylab = 'Total Number of Entries to Platform Day5', border="red", boxwex = 0.35, outline = F, ylim = c(0,10))
boxplot(cd_entry5, ylab = 'Total Number of Entries to Platform Day5', border="black", xaxt='n', boxwex = 0.35 , add = T, at = d, outline = F)
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)
#dev.off()

###STATS####
#SWIM DISTANCE
t.test(ad_dist[ ,1],cd_dist[ ,1])
t.test(ad_dist[ ,2],cd_dist[ ,2])
t.test(ad_dist[ ,3],cd_dist[ ,3])
t.test(ad_dist[ ,4],cd_dist[ ,4])
t.test(ad_dist[ ,5],cd_dist[ ,5])

#SWIM TIME
t.test(ad_time[ ,1],cd_time[ ,1])
t.test(ad_time[ ,2],cd_time[ ,2])
t.test(ad_time[ ,3],cd_time[ ,3])
t.test(ad_time[ ,4],cd_time[ ,4])
t.test(ad_time[ ,5],cd_time[ ,5])

#SWIM VELOCITY
t.test(ad_vel[ ,1],cd_vel[ ,1])
t.test(ad_vel[ ,2],cd_vel[ ,2])
t.test(ad_vel[ ,3],cd_vel[ ,3])
t.test(ad_vel[ ,4],cd_vel[ ,4])
t.test(ad_vel[ ,5],cd_vel[ ,5])

#PROBES-PERCENTAGE IN ZONE
t.test(ad_prox33,cd_prox33)
t.test(ad_prox5,cd_prox5)

#PROBES-TOTAL DISTANCE TO TARGET ZONE
t.test(ad_dist33,cd_dist33)
t.test(ad_dist5,cd_dist5)

#NUMBER OF ENTRIES IN ZONE
t.test(ad_entry33,cd_entry33)
t.test(ad_entry5,cd_entry5)

###AIC SCORES FOR PREDICTORS##
aicscores<-read.csv("/Users/omega/Documents/Natalie/CVN_AD_Time3/AIC_scores_time.csv")
plot(aicscores[ ,1],aicscores[ ,2], main = "AIC scores for Different Image Contrasts", xlab = "Number of Predictors", ylab="AIC scores",ylim = c(100,160))
points(aicscores[ ,1],aicscores[ ,3], col=2)
points(aicscores[ ,1],aicscores[ ,4], col=3)
legend("topright", title = 'Image Contrasts', legend = c('Jacobian', 'T1-Mang', 'Susceptibility'), fill=c('black','red','green'), horiz=FALSE)

##HEATMAP###
setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1,width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")
pheatmap(heatmap_time4, color = colorRampPalette(c("firebrick2", "orange2", "yellow"))(50),labels_col=c("150","200","250","300") ,labels_row = c("0.005","0.01","0.05","0.1","0.15","0.2"),cluster_rows = FALSE,cluster_cols = FALSE, fontsize = 12,legend = TRUE, main = "Error Prediction Map of Time Swum on Day 4 (s)")
grid.text("Cluster Threshold", y=-0.05,gp=gpar(fontsize=12,fontface="bold"))
grid.text("Sparsity", x=0.95, rot=90, gp=gpar(fontsize=12,fontface="bold"))


### Slope of Individual Animal
plot(as.numeric(ad_behavior[1,2:6]),type="o", lwd=2, pch=22, lty=2, xaxt="n",ylim=c(0,1250),col="red",xlab="Day",ylab="Swim Distance (cm)",main="Learning Ability")
lines(as.numeric(ad_behavior[2,2:6]), type="o",lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[3,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[4,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[5,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[6,2:6]), type="o",lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[7,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[8,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[9,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[10,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(ad_behavior[11,2:6]), type="o", lwd=2, pch=22, lty=2, col="red")
lines(as.numeric(con_behavior[1,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[2,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[3,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[4,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[5,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[6,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[7,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[8,2:6]), type="o",lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[9,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[10,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[11,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[12,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
lines(as.numeric(con_behavior[13,2:6]), type="o", lwd=2, pch=22, lty=2, col="black")
axis(1,at=1:5,labels=colnames(behavior[2:6]))
legend("bottomleft", title = 'Genotype', legend = c('NOS2', 'CVN'), fill=c('black','red'), horiz=TRUE)


### Calculating the slopes ####
learning.rate <-matrix(0,ncol = 2, nrow = 24)
for (i in 1:24){
  time <-1:5
  y <- as.numeric(behavior[i,c(2:6)])
  mylm<-summary(lm(y~time))
  learning.rate[i,1] <-mylm$coefficients[1,1]
  learning.rate[i,2] <-mylm$coefficients[2,1]
}
learning.rate <-cbind(as.character(behavior[1:24,1]),learning.rate)
colnames(learning.rate) <-c("Runno","Intercept","Slope")
learning.file<-write.table(learning.rate, file = '/Users/omega/Documents/Natalie/SCCAN_tutorial/Eig_zscore/data/learning_rate.csv', append = FALSE, sep = ",", col.names = NA, row.names = TRUE)
