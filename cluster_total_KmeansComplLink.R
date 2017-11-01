#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
library(rminer)
library(CrossClustering)
library(dplyr)
library(cluster)
set.seed(1)

#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time <- cbind(bank, time_axis)

bank_time_without_y <- subset(bank_time[,-21])

#----------------------Clustering----------------------------#
    
# Setting up clustering training set
d <- daisy(bank_time_without_y, metric = "gower")

cc_hyper <- CrossClustering(d, k.w.min = 2, k.w.max=19, k.c.max = 19)

hyper_nr <- unlist(cc_hyper$Optimal.cluster)

# printing clustering information training set
cat("amount of clusters training set:", cc_hyper$Optimal.cluster, "\n")

clusters <- kmeans(d, hyper_nr, iter.max = 10, nstart = 1)

data <- bank_time

for (i in 1:nrow(data)) {
  data$cluster[i] <- unlist(clusters$cluster[[i]])
}

# memory clean
gc()
    

    
# Write file 
write.table(data, "~/thesis/data/total_with_clust_KmeansCompLink1.txt", sep=";")

gc()





