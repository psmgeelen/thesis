#Load File, Load Pa ckages
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


#Set modeling techniques, for more information see description in rminer documentation
models <- c("lr", "ksvm", "ctree", "mlp", "mlpe")

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
    d <- daisy(bank_time_ss_cl_without_y, metric = "gower")
    cc_hyper <- CrossClustering(d, k.w.min = 2, k.w.max=19, k.c.max = 19)
    hyper_nr <- unlist(cc_hyper$Optimal.cluster)
    
    # printing clustering information training set
    cat("amount of clusters training set:", cc_hyper$Optimal.cluster, "\n")
    
    clusters <- kmeans(d, hyper_nr, iter.max = 10, nstart = 1)
    
    data <- bank_time_ss_cl
    
    for (o in 1:nrow(data)) {
      data$cluster[o] <- unlist(clusters$cluster[[o]])
    }
    
    # memory clean
    gc()
    
    
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
    C15_t <- c(C15_t, "Kmeans")
    # Stack values clustering
    C9_t <- c(C9_t, unlist(cc_hyper$Optimal.cluster))
    C10_t <- c (C10_t, "")
    C11_t <- c(C11_t, "")
    
    
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
colnames(rolling_window_sum) <- c("Itteration","Model","AUC", "ALIFT", "ACC", "Lower", "Upper", "Window-size", 
                                  "Clustering", "Amount of Clusters", "Silhouette", "% Ommited")

#Show Table (back check)
head(rolling_window_sum)

# Write file 
#write.table(rolling_window_sum, "/home/schnitzel/rolling_window_clust.txt", sep=";")
write.table(rolling_window_sum, "rolling_window_kmeans.txt", sep=";")

gc()





