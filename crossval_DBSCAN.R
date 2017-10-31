#Load File, Load Packages
bank_time<-read.csv("~/thesis/data/total_with_clust_DBSCAN.txt",header=TRUE,sep=";")
library(rminer)
library(ggplot2)
set.seed(1)

#Set modeling techniques, for more information see description in rminer documentation
models <- c("ksvm", "ctree", "mlp", "lr")

#Variable prep
C0_t <- vector(mode="character", length=0)
C1_t <- vector(mode="numeric", length=0)
C2_t <- vector(mode="numeric", length=0)
C3_t <- vector(mode="numeric", length=0)
C4_t <- vector(mode="numeric", length=0)
C5_t <- vector(mode="character", length=0)

#----------------Modeling with Rolling Window--------------------#
t <- system.time(
  
for (i in models) 
  {
    for (n in 2:10)  #ngroups = cross validation, minimum is 2 groups/groups shouldnt be more then n=30. 
      {  
      # reset model
      M <- 0
      # create model
      M <- crossvaldata(y~.,bank_time,fit,predict,ngroup=n,model=i, task="prob")
      
      
      cat("---Cross validation model", i, "with", n, "groups---", "\n")
      C1=mmetric(bank_time$y,M$cv.fit,metric="AUC")
      C2=mmetric(bank_time$y,M$cv.fit,metric="ALIFT")
      C3=mmetric(bank_time$y,M$cv.fit,metric="ACC")
      C0=print(n)
      
      #print findings
      cat("AUC of", i, ":", C1, "\n")
      cat("ALIFT of", i, ":", C2, "\n")
      cat("ACC of", i, ":", C3, "\n")
      
      
      # Model label
      C4 <- i
      # Stack values
      C0_t <- c(C0_t, C0)
      C1_t <- c(C1_t, C1)
      C2_t <- c(C2_t, C2)
      C3_t <- c(C3_t, C3)
      C4_t <- c(C4_t, print(paste(i)))
      C5_t <- c(C5_t, "DBSCAN")
      
      # Memory wipe for scalability
      gc()
      
      } }
)

cat("---time---")
print(t)

#Combine Data Frame
crossval_sum <- cbind(C0_t,C4_t,C1_t,C2_t,C3_t.C5_t)
#Label Data Frame
colnames(crossval_sum) <- c("Groups","Model","AUC of ROC", "ALIFT", "ACC", "clustering")

#Show Table (back check)
head(crossval_sum)

# Write file 
write.table(crossval_sum, "crossvalidation_DBSCAN.txt", sep=";")

#Memory wipe
gc()


