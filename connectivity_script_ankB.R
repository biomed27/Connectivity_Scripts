#Connectivity Global and ROI Script
#install.packages("broom")
library(broom)
library(abind)
library(R.matlab)
library(gplots)

################## Global Statistics ##########################
#Load in Data
path.global.ankB <-"/Users/omega/ankyrinB/ankB_NetworkMeasures"
runnums.ankB <- c("N54643", "N54645","N54647","N54649","N54694","N54695","N54696","N54697","N54698","N54701","N54702","N54703")
global_files.ankB<-list.files(path = path.global.ankB, pattern = "_netproperties",full.names = T,recursive = T)

output.global.stats <- file.path(path.global.ankB,'global_stats_twosided.csv')

#Combine global paramters fom each subject into one data frame
global_connet_mat <-read.table(file = global_files.ankB[1], header = FALSE, sep = "\t", nrow = 19)
global_connet_mat_new <-global_connet_mat[ , -c(1)]
for (i in 2:length(global_files.ankB)){
  global.added <-read.table(file = global_files.ankB[i], header = FALSE, sep = "\t", nrow = 19)
  global_connet_mat_new <-cbind(global_connet_mat_new, global.added[,2])
}
colnames(global_connet_mat_new) <-runnums.ankB

ko.ankB<-c(2,3,4,9,10,11)
wt.ankB <-c(1,5,6,7,8,12)

wt.global <-global_connet_mat_new[,wt.ankB]
ko.global <-global_connet_mat_new[,ko.ankB]

# Calculate the mean, standard deviation and non-paramatrix(wilcox.test) between the wild type (wt) and knock-out groups (ko)
global.data.frame <-data.frame()
mean.wt <-data.frame()
mean.ko <-data.frame()
sd.wt <-data.frame()
sd.ko <-data.frame()
for (i in 1:nrow(wt.global)){
  global.test <-wilcox.test(as.numeric(wt.global[i, ]), as.numeric(ko.global[i, ]), alternative = c("two.sided"), conf.int = TRUE)
  mean.wtt <- mean(as.numeric(wt.global[i, ]))
  mean.koo <-mean(as.numeric(ko.global[i, ]))
  sd.wtt <- sd(as.numeric(wt.global[i, ]))
  sd.koo <-sd(as.numeric(ko.global[i, ]))
  output.global<-tidy(global.test)
  global.data.frame <- rbind(global.data.frame,output.global)
  mean.wt <-rbind(mean.wt,mean.wtt)
  mean.ko <-rbind(mean.ko,mean.koo)
  sd.wt <-rbind(sd.wt,sd.wtt)
  sd.ko <-rbind(sd.ko,sd.koo)
  global.dataframe.others <-cbind(global.data.frame,mean.wt,mean.ko,sd.wt,sd.ko)
}
global.stats.data.frame <-cbind(global_connet_mat[ ,1],global.dataframe.others)
colnames(global.stats.data.frame) <-c("Global_Params",'estimate', 'statistic','p.value', 'conf.low','conf.high','method','alternative',"mean_ko",'mean_wt','sd_ko','sd.wt')
#output.global.file<-write.table(global.stats.data.frame, file = output.global.stats, append = FALSE, sep = ",", col.names = NA, row.names = TRUE)

############## ROI Statistics ####################################
# Load in Data
path.global.ankB <-"/Users/omega/ankyrinB/ankB_NetworkMeasures"
output.roi.stats <- file.path(path.global.ankB,'roi_stats_qvals_BH.csv')
sig.output.roi <- file.path(path.global.ankB,'significant_rois_params.csv')
roi_files<-list.files(path = path.global.ankB, pattern = "_netproperties",full.names = T,recursive = T)

# Load ROI matricies and index each one into a list
list.roi.files <- replicate(length(roi_files), list())
for( i in 1:length(roi_files)){
  roi_connet_mat <-read.table(file = roi_files[i], header = TRUE, sep = "\t", nrow = 15, skip = 19)
  roi_connet_mat_new<-roi_connet_mat[ , -c(1)]
  list.roi.files[[i]] <-roi_connet_mat_new
}

ko.ankB<-c(2,3,4,9,10,11)
wt.ankB <-c(1,5,6,7,8,12)

# Concatenate the list into a 3 dimensional matrix (14 rows by 332 columns (rois) by number of animals)
# Perform a t-test between the two groups and apply Benjamanni Hochberg correction to attend to multiple comparisons
g<-abind(list.roi.files, along =3)
p <-matrix(NA, nrow = 14, ncol = 332)
q <-matrix(NA, nrow = 14, ncol = 332)
t <-matrix(NA, nrow = 14, ncol = 332)
sig.roi <-matrix(NA, nrow = 14, ncol = 332)
for (i in 1:14){
  for(j in 1:332) {
    p[i,j] = try(t.test(x = as.numeric(g[i,j ,ko.ankB]), y=as.numeric(g[i,j, wt.ankB]))$p.value, silent = TRUE)
    t[i,j]=try(t.test(x = as.numeric(g[i,j ,ko.ankB]), y=as.numeric(g[i,j, wt.ankB]))$statistic, silent = TRUE)
    q[i,j] <-p.adjust(p[i,j], method = "BH")
    sig.roi[i,j] <-q[i,j] <= 0.05
  }
}

# Gather all the ROIS that are significant
need.rows <-matrix(0, nrow =14 , ncol = 50)
for(i in 1:14){
  which.rows <-which(sig.roi[i,], arr.ind = TRUE, useNames = TRUE)
  ind <-length(which.rows)
  need.rows[i,1:ind] <-which.rows
}
sig.roi.combined<-cbind(roi_connet_mat[ ,1],data.frame(need.rows))
#output.sig.file<-write.table(sig.roi.combined, file = sig.output.roi, append = FALSE, sep = ",", col.names = NA, row.names = TRUE)

#Output the q-values for each region
sig.test<-cbind(roi_connet_mat[ ,1],data.frame(q))
header <- colnames(roi_connet_mat_new)
colnames(sig.test) <-c("Parameters",header[1:332])
#output.roi.file<-write.table(sig.test, file = output.roi.stats, append = FALSE, sep = ",", col.names = NA, row.names = TRUE)

#### HEAT MAP GENERATION #####
library(R.matlab)
roi.sig.params <-read.csv('/Users/omega/ankyrinB/ankB_NetworkMeasures/significant_rois_params.csv')
path.cvn.connect <-"/Users/omega/ankyrinB/ankb_ConnectivityMatrix"
roi_mat<-list.files(path = path.cvn.connect, pattern = "_connectivitymatrix.mat",full.names = T,recursive = T)
list.roi.files <- replicate(length(roi_mat), list())
for( i in 1:length(roi_mat)){
  roi_connet_mat <-readMat(roi_mat[i])
  roi_mat_new <-roi_connet_mat$connectivity
  list.roi.files[[i]] <-roi_mat_new
}
g<-abind(list.roi.files, along =3)
roi.guide <-read.csv('/Users/omega/Documents/MATLAB/connectivity_scripts/ROIguide.csv')

ko.ankB<-c(2,3,4,9,10,11)
wt.ankB <-c(1,5,6,7,8,12)

which.sig.cols <-which(roi.sig.params[1, ] != 0, arr.ind = FALSE)
try.again <-as.numeric(roi.sig.params[1, which.sig.cols])
need <-try.again[-c(1,2)]
mean.roi.koankB <-apply(g[, ,ko.ankB ], c(1,2),mean)
mean.roi.wtankB <-apply(g[, ,wt.ankB], c(1,2),mean)
sig.con.koankB <- mean.roi.koankB[need,need]
sig.con.wtankB <- mean.roi.wtankB[need,need]
x1 = heatmap.2((as.matrix(sig.con.koankB)),  Rowv=FALSE, Colv=FALSE, dendrogram = "none" ,trace="none" ,margins = c(5, 5), breaks = seq(0,3,0.01),col = heat.colors ,labRow = as.character(roi.guide[need,3]),labCol =as.character(roi.guide[need,3]), main = c('KO:', as.character(roi.sig.params[1,2])))
x2 = heatmap.2((as.matrix(sig.con.wtankB)),  Rowv=FALSE, Colv=FALSE, dendrogram = "none" ,trace="none" ,margins = c(5, 5), breaks = seq(0,3,0.01),col = heat.colors ,labRow = as.character(roi.guide[need,3]),labCol =as.character(roi.guide[need,3]), main = c('WT:', as.character(roi.sig.params[1,2])))

