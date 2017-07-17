#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
install.packages(rminer)
library(rminer)
library(CrossClustering)


#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time <- cbind(bank, time_axis)


#Set modeling techniques, for more information see description in rminer documentation
models <- c("lr", "ksvm", "ctree", "mlpe")

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

#----------------Modeling with Rolling Window--------------------#
windowsize <- 1500
increments <- 1000

itt <- ( (length(bank_time$y)-windowsize) %/% increments)
print(itt)

t <- system.time(
for (i in models)
  {
  for(c in 1:2) # iterations rolling window
    {
    tr1 <- (1+(c-1)*increments)
    tr2 <- ((1+(c-1)*increments)+windowsize)
    ts1 <- (((1+(c-1)*increments)+windowsize)+1)
    ts2 <- (((1+(c-1)*increments)+2*windowsize)+1)
    
    #subsets for training and testing
    bank_time_ss_tr <- subset(bank_time[which(bank_time$time_axis >= tr1 & bank_time$time_axis <= tr2), ])
    bank_time_ss_ts <- subset(bank_time[which(bank_time$time_axis >= ts1 & bank_time$time_axis <= ts2), ])
    bank_time_ss_cl <- subset(bank_time[which(bank_time$time_axis >= tr1 & bank_time$time_axis <= ts2), ])
    
    #create clustering information
    # distance estimation, k.c.max is maximum.
    distance <- dist(bank_time_ss_cl, method = "euclidean")
    clusters_list <- CrossClustering(d, k.w.min = 2, k.w.max = 21, k.c.max = 21)
    ommited <- ((1-(clusters_list$n.clustered/clusters_list$n.total))*100)
    
    
    bank_time_ss_tr$cluster <- 
    bank_time_ss_ts$cluster <-
    
    #Modeling and Predictions 
    M <- fit(y~.,bank_time_ss_tr,model=i, task = "prob")
    P <- predict(M, bank_time_ss_ts, type = "prob")
    
    #Perfomance measure
    cat("---Rolling Window model", i, "with", c, "th iteration---", "\n")
    C1=mmetric(bank_time_ss_ts$y,P,metric="AUC")
    C2=mmetric(bank_time_ss_ts$y,P,metric="ALIFT")
    C3=mmetric(bank_time_ss_ts$y,P,metric="ACC")
    C4=mmetric(bank_time_ss_ts$y,P,metric="CONF")
    C0=print(c)
    
    #Print findings
    cat("AUC of", i, ":", C1, "\n")
    cat("ALIFT of", i, ":", C2, "\n")
    cat("ACC of", i, ":", C3, "\n")
    cat("---Confusion Matrix---", "\n")
    print(C4)  
    
    # Model label
    C4 <- print(paste(i))
    # Stack values modeling
    C0_t <- c(C0_t, C0)
    C1_t <- c(C1_t, C1)
    C2_t <- c(C2_t, C2)
    C3_t <- c(C3_t, C3)
    C4_t <- c(C4_t, print(paste(i)))
    C5_t <- c(C5_t, tr1)
    C6_t <- c(C6_t, tr2)
    C7_t <- c(C7_t, ts1)
    C8_t <- c(C8_t, ts2)
    # Stack values clustering
    C9_t <- c(C9_t, clusters_list$Optimal.cluster)
    C10_t <- c (C10_t, clusters_list$Silhouette)
    C11_t <- c(C11_t, ommited)
    
    gc()

  } }
)

cat("---time---")
print(t)

#Combine Data Frame
rolling_window_sum <- cbind(C0_t,C4_t,C1_t,C2_t,C3_t,C5_t,C6_t,C7_t,C8_t, C9_t, C10_t, C11_t)
#Label Data Frame
colnames(rolling_window_sum) <- c("Itteration","Model","AUC", "ALIFT", "ACC", "TR1", "TR2", "TS1", "TS2", "Amount of Clusters", "Silhouette", "% Ommited")

#Show Table (back check)
head(rolling_window_sum)

gc()





