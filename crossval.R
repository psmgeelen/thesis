#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
install.packages(rminer)
library(rminer)
library(ggplot2)

#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time_1 <- cbind(bank, time_axis)

#develop subset (quickruns)
bank_time <- bank_time_1[1:1000,]

#Set modeling techniques, for more information see description in rminer documentation
models <- c("ksvm", "ctree", "mlpe", "lr")

#Variable prep
C0_t <- vector(mode="character", length=0)
C1_t <- vector(mode="numeric", length=0)
C2_t <- vector(mode="numeric", length=0)
C3_t <- vector(mode="numeric", length=0)
C4_t <- vector(mode="numeric", length=0)

#----------------Modeling with Rolling Window--------------------#
t <- system.time(
for (i in models) 
  {
    for (n in 2:6)  #ngroups = cross validation, minimum is 2 groups
      {  
    
      M=crossvaldata(y~.,bank_time,fit,predict,ngroup=n,model=i, task="prob")
      
      cat("---Cross validation model", i, "with", n, "groups---", "\n")
      C1=mmetric(bank_time$y,M$cv.fit,metric="AUC")
      C2=mmetric(bank_time$y,M$cv.fit,metric="ALIFT")
      C3=mmetric(bank_time$y,M$cv.fit,metric="ACC")
      C4=mmetric(bank_time$y,M$cv.fit,metric="CONF")
      C0=print(n)
      
      #print findings
      cat("AUC of", i, ":", C1, "\n")
      cat("ALIFT of", i, ":", C2, "\n")
      cat("ACC of", i, ":", C3, "\n")
      cat("---Confusion Matrix---", "\n")
      print(C4)  
      
      # Model label
      C4 <- print(paste(i))
      # Stack values
      C0_t <- c(C0_t, C0)
      C1_t <- c(C1_t, C1)
      C2_t <- c(C2_t, C2)
      C3_t <- c(C3_t, C3)
      C4_t <- c(C4_t, print(paste(i)))
      
      # Memory wipe for scalability
      gc()
      
      } }
)

cat("---time---")
print(t)

#Combine Data Frame
crossval_sum <- cbind(C0_t,C4_t,C1_t,C2_t,C3_t)
#Label Data Frame
colnames(crossval_sum) <- c("Groups","Model","AUC of ROC", "ALIFT", "ACC")

#Show Table (back check)
head(crossval_sum)


#Memory wipe
gc()


