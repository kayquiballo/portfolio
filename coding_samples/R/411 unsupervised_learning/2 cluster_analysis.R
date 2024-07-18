require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(ggplot2)
library(tidyverse)
library(Rtsne)

################################################################################
####################               Data Prep                ####################
################################################################################

alldata = read.csv("Melbourne_housing_FULL.csv",header=TRUE)
houses = alldata[complete.cases(alldata),]
workdata = houses[,c("Rooms","Price","Distance","Bedroom2",
                     "Bathroom","Car","Landsize","BuildingArea","YearBuilt")]
workdata$Distance <- as.integer(workdata$Distance)
workdata$BuildingArea <- as.integer(workdata$BuildingArea)
print(str(workdata))

################################################################################
####################               EDA                      ####################
################################################################################

################## set up ################## 
x1<-workdata$Rooms
x2<-workdata$Price
x3<-workdata$Distance
x4<-workdata$Bedroom2
x5<-workdata$Bathroom
x6<-workdata$Car
x7<-workdata$Landsize
x8<-workdata$BuildingArea
x9<-workdata$YearBuilt
mydata1<-cbind.data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9)

x<-as.matrix(mydata1)
y<-dist(x)

xsc<-scale(x)
y<-dist(xsc)

################## outliers ################## 

summary(xsc)
nrow(xsc) #8887
y<-dist(xsc)
#x4, x9 have min < -3
#x1-x8 have max > 3

xsc <- as.data.frame(xsc)
xsc2 <- xsc[abs(xsc$x1) <= 3.29  &
      abs(xsc$x2) <= 3.29  &
      abs(xsc$x3) <= 3.29  &
      abs(xsc$x4) <= 3.29  &
      abs(xsc$x5) <= 3.29  &
      abs(xsc$x6) <= 3.29  &
      abs(xsc$x7) <= 3.29  &
      abs(xsc$x8) <= 3.29  &
      abs(xsc$x9) <= 3.29  , ]
xsc3 <- houses[abs(xsc$x1) <= 3.29  &
              abs(xsc$x2) <= 3.29  &
              abs(xsc$x3) <= 3.29  &
              abs(xsc$x4) <= 3.29  &
              abs(xsc$x5) <= 3.29  &
              abs(xsc$x6) <= 3.29  &
              abs(xsc$x7) <= 3.29  &
              abs(xsc$x8) <= 3.29  &
              abs(xsc$x9) <= 3.29  , ]

xsc2<-as.matrix(xsc2)
for_later<-cbind.data.frame(xsc2, xsc3)
nrow(xsc2) #8399 -- 488 pts removed which is 5.5%
summary(xsc2)
y<-dist(xsc2)

################## Correlation Plot ################## 
workdata2 <- xsc <- as.data.frame(xsc2)
M = cor(workdata2)
corrplot(M,  method="number")

#reordered for PCA -- PCA1(x1,x4,x8,x5,x6) | PCA2(x2,x3,x9) | PCA3(x7)
corrplot(cor(workdata2[,c("x1","x4","x8","x5","x6","x2","x3","x9","x7")]),  method="number")

################################################################################
####################               PCA/FA                   ####################
################################################################################

################## Primary Component Analysis ################## 

par(mfrow=c(1,1))
fa.parallel(workdata2,fa="pc",n.iter=100,show.legend=FALSE,main="Screeplot wtih parallel analysis")
fa.parallel(M, n.obs=8399, fa="both", n.iter=100, show.legend=TRUE, main="Scree plot with parallel analysis")
scree(M) #indicates how many components to keep (above dotted line)
#scree plot indicates possibly 2-3 groups for PCA and 1 group for FA

Z<-eigen(M)
Z$val #eigenvalues bigger than 1 rule

#include loadings above .3-.5 | cumulative variance should be 90-95% | #no need to rotate 
#RMSR – (absolute fit) A value less than .1 is generally considered a good fit
pc2 <- principal(workdata2,nfactors=2,rotate="none",scores=TRUE) #rmsr = .09 | fit = 0.95
pc3 <- principal(workdata2,nfactors=3,rotate="none",scores=TRUE) #rmsr = .08 | fit = 0.96
#groups are PCA1(x1,x4,x8,x5,x6) | PCA2(x2,x3,x9) | PCA3(x7)
#cumvar is 44%,19%,11% --> 44%,63%,74%

pc.1 <- pc2$scores[,1]
pc.2 <- pc2$scores[,2]

################## Factor Analysis ################## 

#fa(workdata2, nfactors=3, rotate="none", fm="pa")
fa(workdata2, nfactors=3, rotate="none", fm="ml") #low bic
#fa(workdata2, nfactors=3, rotate="varimax", fm="pa")
fa(workdata2, nfactors=3, rotate="varimax", fm="ml") #low bic
#groups are PCA1(x1,x4,x8,x5,x6) | PCA2(x2,x3,x9) | PCA3(x7)

################################################################################
####################               Cluster                  ####################
################################################################################

################## Hierarchical Cluster Analysis ################## 

hc.complete<-hclust(y,method="complete")
hc.average<-hclust(y,method="average")
hc.single<-hclust(y,method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab=" ",sub=" ", cex=0.9)
plot(hc.average,main="Average Linkage", xlab=" ",sub=" ", cex=0.9)
plot(hc.single,main="Single Linkage", xlab=" ", sub=" ",cex=0.9)

workdata3 <- workdata2
group2<-cutree(hc.complete, 2)
group4<-cutree(hc.complete, 4)
group5<-cutree(hc.complete, 5)

#combine pca with clusters
workdata3<-cbind.data.frame(workdata2,group2,group4,group5,pc.1,pc.2)

################## K-Means Cluster Analysis ################## 

km.out2=kmeans(workdata2,2,nstart=50)
km.out4=kmeans(workdata2,4,nstart=50)
km.out5=kmeans(workdata2,5,nstart=50)

workdata3$cluster2 <- km.out2$cluster
workdata3$cluster4 <- km.out4$cluster
workdata3$cluster5 <- km.out5$cluster

km.out2
km.out4
km.out5

kmean_withinss <- function(k) {
  cluster <- kmeans(workdata2, k)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <- 10
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)
# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 10, by = 1))

################## Plotting ################## 

require(gridExtra)

plot1 <- ggplot(workdata3, aes(x=pc.1, y=pc.2, colour = as.factor(group2))) +
  geom_point() +
  ggtitle("Scatter Plot PC1 vs PC2 - 2 Clusters - Hierarchical") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

plot2 <- ggplot(workdata3, aes(x=pc.1, y=pc.2, colour = as.factor(cluster2))) +
  geom_point() +
  ggtitle("Scatter Plot PC1 vs PC2 - 2 Clusters - K-Means") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

plot3 <- ggplot(workdata3, aes(x=pc.1, y=pc.2, colour = as.factor(group4))) +
  geom_point() +
  ggtitle("Scatter Plot PC1 vs PC2 - 4 Clusters - Hierarchical") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

plot4 <- ggplot(workdata3, aes(x=pc.1, y=pc.2, colour = as.factor(cluster4))) +
  geom_point() +
  ggtitle("Scatter Plot PC1 vs PC2 - 4 Clusters - K-Means") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

plot5 <- ggplot(workdata3, aes(x=pc.1, y=pc.2, colour = as.factor(group5))) +
  geom_point() +
  ggtitle("Scatter Plot PC1 vs PC2 - 5 Clusters - Hierarchical") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

plot6 <- ggplot(workdata3, aes(x=pc.1, y=pc.2, colour = as.factor(cluster5))) +
  geom_point() +
  ggtitle("Scatter Plot PC1 vs PC2 - 5 Clusters - K-Means") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

grid.arrange(plot1, plot2, ncol=2)
grid.arrange(plot3, plot4, ncol=2)
grid.arrange(plot5, plot6, ncol=2)

################################################################################
####################               Results                  ####################
################################################################################

workdata4<-cbind.data.frame(for_later,group2,group4,group5,pc.1,pc.2)
workdata4$cluster2 <- km.out2$cluster
workdata4$cluster4 <- km.out4$cluster
workdata4$cluster5 <- km.out5$cluster

CrossTable(workdata4[workdata4$Regionname=="Northern Metropolitan",]$cluster2, workdata4[workdata4$Regionname=="Northern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Northern Metropolitan",]$cluster4, workdata4[workdata4$Regionname=="Northern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Northern Metropolitan",]$cluster5, workdata4[workdata4$Regionname=="Northern Metropolitan",]$Type)

CrossTable(workdata4[workdata4$Regionname=="Western Metropolitan",]$cluster2, workdata4[workdata4$Regionname=="Western Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Western Metropolitan",]$cluster4, workdata4[workdata4$Regionname=="Western Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Western Metropolitan",]$cluster5, workdata4[workdata4$Regionname=="Western Metropolitan",]$Type)

CrossTable(workdata4[workdata4$Regionname=="Southern Metropolitan",]$cluster2, workdata4[workdata4$Regionname=="Southern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Southern Metropolitan",]$cluster4, workdata4[workdata4$Regionname=="Southern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Southern Metropolitan",]$cluster5, workdata4[workdata4$Regionname=="Southern Metropolitan",]$Type)

CrossTable(workdata4[workdata4$Regionname=="Eastern Metropolitan",]$cluster2, workdata4[workdata4$Regionname=="Eastern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Eastern Metropolitan",]$cluster4, workdata4[workdata4$Regionname=="Eastern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Eastern Metropolitan",]$cluster5, workdata4[workdata4$Regionname=="Eastern Metropolitan",]$Type)

CrossTable(workdata4[workdata4$Regionname=="South-Eastern Metropolitan",]$cluster2, workdata4[workdata4$Regionname=="South-Eastern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="South-Eastern Metropolitan",]$cluster4, workdata4[workdata4$Regionname=="South-Eastern Metropolitan",]$Type)
CrossTable(workdata4[workdata4$Regionname=="South-Eastern Metropolitan",]$cluster5, workdata4[workdata4$Regionname=="South-Eastern Metropolitan",]$Type)

CrossTable(workdata4[workdata4$Regionname=="Northern Victoria",]$cluster2, workdata4[workdata4$Regionname=="Northern Victoria",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Northern Victoria",]$cluster4, workdata4[workdata4$Regionname=="Northern Victoria",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Northern Victoria",]$cluster5, workdata4[workdata4$Regionname=="Northern Victoria",]$Type)

CrossTable(workdata4[workdata4$Regionname=="Western Victoria",]$cluster2, workdata4[workdata4$Regionname=="Western Victoria",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Western Victoria",]$cluster4, workdata4[workdata4$Regionname=="Western Victoria",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Western Victoria",]$cluster5, workdata4[workdata4$Regionname=="Western Victoria",]$Type)

CrossTable(workdata4[workdata4$Regionname=="Eastern Victoria",]$cluster2, workdata4[workdata4$Regionname=="Eastern Victoria",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Eastern Victoria",]$cluster4, workdata4[workdata4$Regionname=="Eastern Victoria",]$Type)
CrossTable(workdata4[workdata4$Regionname=="Eastern Victoria",]$cluster5, workdata4[workdata4$Regionname=="Eastern Victoria",]$Type)

CrossTable(workdata4$cluster2, workdata4$Type)
CrossTable(workdata4$cluster4, workdata4$Type)
CrossTable(workdata4$cluster5, workdata4$Type)



#x1
(1.07031840 * sd(workdata4$Rooms)) + mean(workdata4$Rooms)
(1.05497506 * sd(workdata4$Rooms)) + mean(workdata4$Rooms)
(-0.05291859 * sd(workdata4$Rooms)) + mean(workdata4$Rooms)
(-0.34358179 * sd(workdata4$Rooms)) + mean(workdata4$Rooms)
(-1.32067330 * sd(workdata4$Rooms)) + mean(workdata4$Rooms)

#x2
(-0.1215249 * sd(workdata4$Price)) + mean(workdata4$Price)
(1.4068768 * sd(workdata4$Price)) + mean(workdata4$Price)
(-0.3372832 * sd(workdata4$Price)) + mean(workdata4$Price)
(0.2865572 * sd(workdata4$Price)) + mean(workdata4$Price)
(-0.7330484 * sd(workdata4$Price)) + mean(workdata4$Price)

#x3
(0.6795259 * sd(as.integer(workdata$Distance))) + mean(as.integer(workdata$Distance))
(-0.3866424 * sd(as.integer(workdata$Distance))) + mean(as.integer(workdata$Distance))
(0.3354507 * sd(as.integer(workdata$Distance))) + mean(as.integer(workdata$Distance))
(-0.7561327 * sd(as.integer(workdata$Distance))) + mean(as.integer(workdata$Distance))
(-0.5115393 * sd(as.integer(workdata$Distance))) + mean(as.integer(workdata$Distance))

#x4
(1.07617171 * sd(workdata4$Bedroom2)) + mean(workdata4$Bedroom2)
(1.03312577 * sd(workdata4$Bedroom2)) + mean(workdata4$Bedroom2)
(-0.04347467 * sd(workdata4$Bedroom2)) + mean(workdata4$Bedroom2)
(-0.36205622 * sd(workdata4$Bedroom2)) + mean(workdata4$Bedroom2)
(-1.31308079 * sd(workdata4$Bedroom2)) + mean(workdata4$Bedroom2)

#x5
(0.8374022 * sd(workdata4$Bathroom)) + mean(workdata4$Bathroom)
(0.9629147 * sd(workdata4$Bathroom)) + mean(workdata4$Bathroom)
(-0.1945884 * sd(workdata4$Bathroom)) + mean(workdata4$Bathroom)
(-0.5272089 * sd(workdata4$Bathroom)) + mean(workdata4$Bathroom)
(-0.7279883 * sd(workdata4$Bathroom)) + mean(workdata4$Bathroom)

#x6
(0.4320565 * sd(workdata4$Car)) + mean(workdata4$Car)
(0.3880190 * sd(workdata4$Car)) + mean(workdata4$Car)
(0.1361466 * sd(workdata4$Car)) + mean(workdata4$Car)
(-0.6909021 * sd(workdata4$Car)) + mean(workdata4$Car)
(-0.5960803 * sd(workdata4$Car)) + mean(workdata4$Car)

#x7
(0.06456189 * sd(workdata4$Landsize)) + mean(workdata4$Landsize)
(0.08917351 * sd(workdata4$Landsize)) + mean(workdata4$Landsize)
(-0.03350785 * sd(workdata4$Landsize)) + mean(workdata4$Landsize)
(-0.17593744 * sd(workdata4$Landsize)) + mean(workdata4$Landsize)
(-0.23725413 * sd(workdata4$Landsize)) + mean(workdata4$Landsize)

#x8
(0.6421044 * sd(workdata4$BuildingArea)) + mean(workdata4$BuildingArea)
(0.8611138 * sd(workdata4$BuildingArea)) + mean(workdata4$BuildingArea)
(-0.2132566 * sd(workdata4$BuildingArea)) + mean(workdata4$BuildingArea)
(-0.3188808 * sd(workdata4$BuildingArea)) + mean(workdata4$BuildingArea)
(-0.8037035 * sd(workdata4$BuildingArea)) + mean(workdata4$BuildingArea)


#     x6          x7         x8         x9
# 0.4320565  0.06456189  0.6421044  0.6830254
# 0.3880190  0.08917351  0.8611138 -0.6305112
# 0.1361466 -0.03350785 -0.2132566  0.3437024
# -0.6909021 -0.17593744 -0.3188808 -1.4003684
# -0.5960803 -0.23725413 -0.8037035  0.4010784

#x9
(0.6830254 * sd(workdata4$YearBuilt)) + mean(workdata4$YearBuilt)
(-0.6305112 * sd(workdata4$YearBuilt)) + mean(workdata4$YearBuilt)
(0.3437024 * sd(workdata4$YearBuilt)) + mean(workdata4$YearBuilt)
(-1.4003684 * sd(workdata4$YearBuilt)) + mean(workdata4$YearBuilt)
(0.4010784 * sd(workdata4$YearBuilt)) + mean(workdata4$YearBuilt)

################################################################
######################        rtsne             ################
################################################################
tsne <- Rtsne(workdata2, dims = 2, perplexity = 50, verbose = TRUE, 
      max_iiter=5000, learning=2000, check_duplicates=FALSE)

#visualizing
colors = rainbow(length(unique(workdata4$cluster5)))
names(colors) = unique(workdata4$cluster5)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main = 'tSNE', xlab = 'tSNE dimension 1', ylab = 'tSNE dimension 2', cex.main = 2, cex.lab = 1.5)
text(tsne$Y, labels = workdata4$cluster5, col = colors[workdata4$cluster5])


################################################################
######################        leaflet             ################
################################################################

par(mfrow=c(1,3))

#house
ggplot() +  geom_point(data=workdata4[workdata4$Type=="h",], 
                         aes(x=Longtitude, y=Lattitude, 
                             group=cluster5, col = as.character(cluster5)))
#townhouse
ggplot() +  geom_point(data=workdata4[workdata4$Type=="t",], 
                       aes(x=Longtitude, y=Lattitude, 
                           group=cluster5, col = as.character(cluster5)))
#unit
ggplot() +  geom_point(data=workdata4[workdata4$Type=="u",], 
                       aes(x=Longtitude, y=Lattitude, 
                           group=cluster5, col = as.character(cluster5)))


