#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
library(rminer)
library(CrossClustering)
library(dplyr)
set.seed(1)


#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time <- cbind(bank, time_axis)


#Set modeling techniques, for more information see description in rminer documentation
models <- c("lr", "ksvm", "ctree", "mlp")

#Variable prep
C0_t <- vector(mode="character", length=0)
C1_t <- vector(mode="numeric", length=0)
C2_t <- vector(mode="numeric", length=0)
C3_t <- vector(mode="numeric", length=0)
C4_t <- vector(mode="numeric", length=0)
C5_t <- vector(mode="numeric", length=0)
C6_t <- vector(mode="numeric", length=0)
C7_t <- vector(mode="numeric", length=0)
C8_t <- vector(mode="numeric", length=0)
C9_t <- vector(mode="numeric", length=0)
C10_t <- vector(mode="numeric", length=0)
C11_t <- vector(mode="numeric", length=0)
C12_t <- vector(mode="numeric", length=0)
C13_t <- vector(mode="numeric", length=0)
C14_t <- vector(mode="numeric", length=0)
C15_t <- vector(mode="character", length=0)

#----------------Modeling with Rolling Window--------------------#
# Hyper-Parameters
windowsize <- c(5000, 2000, 1500, 1000)
increments <- 500

# Measuring Time
t <- system.time(

  
# Loop
for (ws in windowsize) {

for (i in models)
  {
  for(c in 1:((nrow(bank_time)-(2*ws)) %/% increments)) # itterations rolling window
    {
    w1 <- (1+(c-1)*increments)
    w2 <- ((1+(c-1)*increments)+ws)
    
    #subsets for training and testing
    bank_time_ss_cl <- subset(bank_time[which(bank_time$time_axis >= w1 & bank_time$time_axis <= w2), ])
    bank_time_ss_cl_without_y <- subset(bank_time_ss_cl[,-21])
    
    #----------------------Clustering----------------------------#
    
    # Setting up clustering training set
    d <- dist(bank_time_ss_cl_without_y, method = "euclidean")
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
    
    # this methodology deconstructs the prior dataset and add variables as long as they are part
    # of the clustering grouping. It must be noted that the CrossClustering uses a partial clu-
    # tering methodology, meaning that not all data will be assigned into a specific cluster. 
    # This is beneficial for accuracy of the clusters. Yet when deconstructing the dataset to
    # assign value, it must be noted that there will be a "rest" group, that is not assigned to 
    # any cluster. Therefore an additional feature must be inlcuded into the script, by which
    # the not assigned data can be re-introduced in the data set. Below, the clusters are added
    # through a loop. The data that has no cluster assignment will added straight after. 
    
    
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
    
    non_cluster_group <- anti_join(bank_time_ss_cl, clust_tot)
    
    # Add column for missing values
    non_cluster_group$clust_n <- vector(mode="numeric", length= nrow(non_cluster_group))
    
    # build final dataset
    data <- rbind(clust_tot, non_cluster_group)
    
    ommited <- ((1-(clusters$n.clustered/clusters$n.total))*100)
    
    # clean out variables
    ss_clust <- vector(mode="numeric", length=0)
    ss_it_length <- vector(mode="numeric", length=0)
    clust_n <- vector(mode="numeric", length=0)
    ss_clust <- vector(mode="numeric", length=0)
    clust_tot <- vector(mode="numeric", length=0)
    
    #Holdout, chronology in this case is important in order to not overestimate prediction accuracy. 
    data_ts <- data[1:(1/3*nrow(data)),]
    data_tr <- data[((1/3*nrow(data))+1):(nrow(data)),]
    
    #Modeling and Predictions 
    M <- fit(y~.,data_tr,model=i, task = "prob")
    P <- predict(M, data_ts, type = "prob")
    
    #Perfomance measure
    cat("---Rolling Window model", i, "with", c, "th iteration","@ window-size",ws,"---", "\n")
    C1=mmetric(data_ts$y,P,metric="AUC")
    C2=mmetric(data_ts$y,P,metric="ALIFT")
    C3=mmetric(data_ts$y,P,metric="ACC")
    
    #Print findings
    cat("AUC of", i, ":", C1, "\n")
    cat("ALIFT of", i, ":", C2, "\n")
    cat("ACC of", i, ":", C3, "\n")
    
    # Stack values modeling
    C0_t <- c(C0_t, c)
    C1_t <- c(C1_t, C1)
    C2_t <- c(C2_t, C2)
    C3_t <- c(C3_t, C3)
    C4_t <- c(C4_t, i)
    C5_t <- c(C5_t, w1)
    C6_t <- c(C6_t, w2)
    C14_t <- c(C14_t, ws)
    C15_t <- c(C15_t, "yes")
    # Stack values clustering
    C9_t <- c(C9_t, unlist(clusters$Optimal.cluster))
    C10_t <- c (C10_t, unlist(clusters$Silhouette))
    C11_t <- c(C11_t, ommited)
    
    # clean variables
    data <- 0
    data_tr <- 0
    data_ts <- 0
    bank_time_ss_cl <- 0
    bank_time_ss_cl_without_y <- 0
    
    gc()

  } }
}

# system time finish
)

cat("---time---")
print(t)

#Combine Data Frame
rolling_window_sum <- cbind(C0_t,C4_t,C1_t,C2_t,C3_t,C5_t,C6_t,C14_t,C15_t,C9_t, C10_t, C11_t)
#Label Data Frame
colnames(rolling_window_sum) <- c("Itteration","Model","AUC", "ALIFT", "ACC", "Lower", "Upper", "Window-size", "Clustering", "Amount of Clusters", "Silhouette", "% Ommited")

#Show Table (back check)
head(rolling_window_sum)

# Write file 
write.table(rolling_window_sum, "/home/schnitzel/rolling_window_clust.txt", sep=";")

gc()





