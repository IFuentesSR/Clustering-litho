data<-read.csv("C:/Users/ifue3702/Documents/lithology.csv", header=TRUE, sep=",")
data2<-data[7:8]
data2['Long']<-data$long
data2['Latit']<-data$latit
data2['Litho']<-data$MajorLithCode
sample<-data2[sample(nrow(data2), 20000), ]
library(ggplot2)
dsample<-dist(sample)
hc <- hclust(dsample, "ave")
sample['clust']<-cutree(hc, k = 20)


normalised<-read.csv('C:/Users/ifue3702/Documents/normalised.csv')
library(cluster)

#calculationg the distance matrix by gower (for mixed data)
aver<-daisy(normalised, metric="gower")

#estimating the number of clusters by optimum average silhouette width
pamk.best <- pamk(aver, krange=1:30, usepam = T)

cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")

#clustering by the k medoids or PAM
medoids<-pam(normalised, pamk.best$nc)

normalised['medoids']<-medoids[3]

write.csv(normalised, file='C:/Users/ifue3702/Documents/medoids.csv')