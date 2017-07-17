#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
library(rminer)
library(CrossClustering)
library(dplyr)

#Data Prep Phase 
boxplot(bank$age)
boxplot(bank$duration)
boxplot(bank$emp.var.rate)
boxplot(bank$cons.conf.idx)
boxplot(bank$euribor3m)

#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time1 <- cbind(bank, time_axis)
bank_time <- bank_time1[1:10000,]


#----------------------Clustering----------------------------#

# Setting up clustering training set
d <- dist(bank_time, method = "euclidean")
clusters <- CrossClustering(d, k.w.min = 2, k.w.max=20, k.c.max = 21)

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

# this methodology deconstructs the prior dataset and add variables as long as they are part
# of the clustering grouping. It must be noted that the CrossClustering uses a partial clu-
# tering methodology, meaning that not all data will be assigned into a specific cluster. 
# This is beneficial for accuracy of the clusters. Yet when deconstructing the dataset to
# assign value, it must be noted that there will be a "rest" group, that is not assigned to 
# any cluster. Therefore an additional feature must be inlcuded into the script, by which
# the not assigned data can be re-introduced in the data set. Below, the clusters are added
# through a loop. The data that has no cluster assignment will added straight after. 


# Labeling of clusters and aggregation of clustered data. 

for (c in 1:clus_amount) {
    
    ss_clust <- subset(bank_time[clusters$Cluster.list[[c]],])
    
    ss_it_length <- nrow(ss_clust)
    
    clust_n <- replicate(ss_it_length, c)
    
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

# memory clean
gc()


#----------------------Modeling----------------------------#

#Set modeling techniques, for more information see description in rminer documentation
models <- c("ctree", "ksvm", "mlpe", "lr")

# Hold-out (train, test sets)
H=holdout(data$y,ratio=1/3)
  
  
  for (i in models) {

    M=fit(y~.,data[H$tr,],model=i, task="class")
    M2=fit(y~.,data[H$tr,],model=i, task="prob")
  
    # Creating variables model
    P=predict(M,data[H$ts,])
    P2=predict(M2,data[H$ts,])
  
  
    #Title
    cat(paste("----- Results for model", i, "-----"))
    # Print stats. 
    # CONF Matrix 
    print(paste("Confusion Matrix Predicition/Probability", i))
    print(mmetric(data$y[H$ts],P,"CONF"))
    print(mmetric(data$y[H$ts],P,"CONF"))
    # AUC of ROC
    cat(paste("AUC for ROC for model", i, "= "))
    cat(mmetric(data$y[H$ts],P2,"AUC"), "\n")
    # AUC ALIFT
    cat(paste("AUC for ALIFT model", i, "= "))
    cat(mmetric(data$y[H$ts],P2,"ALIFT"), "\n")
    # Accuracy
    cat(paste("Accuracy Pred. model", i, "= "))
    cat(mmetric(data$y[H$ts],P,"ACC"), "\n")
    cat(paste("Accuracy Prob. model", i, "= "))
    cat(mmetric(data$y[H$ts],P2,"ACC"), "\n") 
  
    #ROC
    mgraph(data$y[H$ts],P2,graph="ROC",TC=2,main=paste("ROC Curve for",i),
       baseline=TRUE,leg=i,Grid=10)
    #LIFT
    mgraph(data$y[H$ts],P2,graph="LIFT",TC=2,main=paste("LIFT Curve for", i),
       baseline=TRUE,leg=i,Grid=10)
} 

########Reset Memory
gc()





