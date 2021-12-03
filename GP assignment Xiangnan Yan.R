#######################################################
########################part1##########################

library(gtable)
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(readr)
library(factoextra)
#import data
PAQ_Xiangnan <- read.csv("E:/Data/PAQ_Xiangnan.txt",sep="")

#change long data to wide 
data_wide<-PAQ_Xiangnan%>%
  arrange(value)%>%
  spread(var,value)

summary(data_wide)
#delete the case with NA, and delete the column of id,age,sex
data1<-data_wide[-c(280),-c(1,2,12)]

summary(data1)
describe(data1)
cor(data1)

#PCA
data_pca=princomp(data1,cor=TRUE)
summary(data_pca,loadings=TRUE)

####Determine the amount of principal component to retain

#the number and plot of eigenvalue of component
eigenvalue<-get_eigenvalue(data_pca)
eigenvalue

#line plot
plot(data_pca$sdev^2,          
     xlab= "Component number",  
     ylab="Component variance",
     type = "l", 
     main="Scree diagram",
     xaxt = 'n')                 
axis(side=1,at=c(1,2,3,4,5,6,7,8,9),labels=c("1","2","3","4","5","6","7","8","9"))
#bar plot
plot(data_pca) 

#number and plot of variance of component explained
variance = data_pca$sdev^2 / sum(data_pca$sdev^2)
variance

qplot(c(1:9), variance) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#It seems that we can keep three principal components.

#results
results<-get_pca_var(data_pca)
results

#variable coreleation plot
fviz_pca_var(data_pca,col.var = "black")
#cos2
head(results$cos2,9)
fviz_cos2(data_pca,choice = "var",axes = 1:3)
fviz_pca_var(data_pca,col.var = "cos2",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)
#individuals analysis
indiviual<-get_pca_ind(data_pca)
indiviual


#scatterplot participants values according to comp 1

plot(data_pca$scores[,1], 
     xlab="Participant",  
     ylab="Component.1",
     xaxt = 'n')
axis(side=1,at=c(1:300),labels=c(1:300))


plot(data_pca$scores[,2], 
     xlab="Participant",  
     ylab="Component.2",
     xaxt = 'n')
axis(side=1,at=c(1:300),labels=c(1:300))

#barplots for each component, every bar represents one participant

barplot(data_pca$scores[,1], 
        xlab="Participant",  
        ylab="Component.1",
        xaxt = 'n')
axis(side=1,at=c(1:300),labels=c(1:300))


barplot(data_pca$scores[,2], 
        xlab="Participant",  
        ylab="Component.2",
        xaxt = 'n')
axis(side=1,at=c(1:300),labels=c(1:300))



#label: feature/fashion index"

biplot(data_pca,col=c("red","black"),
       xlab="label1")


#######################################################
########################part2##########################

library(psych)
library(vegan)
require(smacof)   
require(MASS) 

#import data
nations <- read_table("E:/Data/Nations.txt")
#change data set as matrix
nations_matrix<-as.matrix(nations)
row.names(nations_matrix)<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","UdSSR","USA","Yugoslavia")

#describe data
summary(nations_matrix)
describe(nations_matrix)
str(nations_matrix)


#Dissimilarities
nations.d <- abs(sim2diss(nations_matrix, method= 1,to.dist = TRUE))
nations.d

#Non-metric multidimensional scaling
nations_mds<-isoMDS(nations.d,k=2)
nations_mds

#scatter plot
plot(nations_mds$points[,1],nations_mds$points[,2],
     xlab="Corrdinate 1",ylab="Corrdinate 2",type='n')
text(nations_mds$points[,1],nations_mds$points[,2],
     labels = rownames(nations_matrix),cex = 0.7)

# putting in vertical and horizontal lines

plot(nations_mds$points[,1],nations_mds$points[,2],
     xlab="Corrdinate 1",ylab="Corrdinate 2",
     xlim=range(nations_mds$points[,1]*1.2),type='n')
text(nations_mds$points[,1],nations_mds$points[,2],labels =colnames(nations_matrix),cex = 0.8)
abline(h=0, v=0, col = "gray60", lty = 2)


# Shepard plot
nations_shep <- Shepard(nations.d, nations_mds$points)

plot(nations_shep, pch = 20, xlab = "Dissimilarity",
     ylab = "Distance", col= "darkgray", main = "Shephard's Diagram")
lines(nations_shep$x, nations_shep$yf, type = "S")

