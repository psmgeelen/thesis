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
clusters <- CrossClustering(d, k.w.min = 2, k.w.max=19, k.c.max = 19)
    
# printing clustering information training set
cat("amount of clusters training set:", clusters$Optimal.cluster, "\n")
cat("Silhouette of training set clusters", clusters$Silhouette, "\n")
cat("Ommited information", (1-(clusters$n.clustered/clusters$n.total))*100, "%", "\n")

# memory clean
gc()
    
#Create cluster_n for cluster amount paramater for training set
clus_amount <- unlist(clusters$Optimal.cluster)
    
# adding clustering information to variable
clust_tot <- vector(mode="numeric", length=0)
    
# Labeling of clusters and aggregation of clustered data. 
    
for (q in 1:clus_amount) {
    
  ss_clust <- subset(bank_time[clusters$Cluster.list[[q]],])
  
  ss_it_length <- nrow(ss_clust)
    
  clust_n <- replicate(ss_it_length, q)
      
  ss_clust <- cbind(ss_clust, clust_n)
      
  clust_tot <- rbind(clust_tot, ss_clust)
      
} 
    
# Addition of non-assigned data with "is not equal" to feature. 
# listning what items are assigned
    
clusters_info <- unlist(clusters$Cluster.list)
    
non_cluster_group <- anti_join(bank_time, clust_tot)
    
# Add column for missing values
non_cluster_group$clust_n <- vector(mode="numeric", length= nrow(non_cluster_group))
    
# build final dataset
data <- rbind(clust_tot, non_cluster_group)
    
# Write file 
write.table(data, "~/thesis/data/total_with_clust.txt", sep=";")

gc()





