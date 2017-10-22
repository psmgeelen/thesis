#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
library(rminer)
library(CrossClustering)
library(dplyr)
library(cluster)
library(dbscan)
library(diffusionMap)
set.seed(1)


#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time <- cbind(bank, time_axis)

bank_time_without_y <- subset(bank_time[,-21])

#----------------------Clustering----------------------------#
    
# Setting up clustering training set
d <- daisy(bank_time_without_y, metric = "gower")

epsilon <- epsilonCompute(d, p = 0.01)

clusters <- optics(d, epsilon)

data <- bank_time

data$cluster <- unlist(clusters$cluster)

nrclust <- length(unique(clusters$cluster))

# printing clustering information training set
cat("amount of clusters training set:", nrclust, "\n")

# memory clean
gc()
    

    
# Write file 
write.table(data, "~/thesis/data/total_with_clust_DBSCAN.txt", sep=";")

gc()





