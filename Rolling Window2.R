#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
install.packages(rminer)
library(rminer)
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
C10_t <- vector(mode="character", length=0)

#----------------Modeling with Rolling Window--------------------#
windowsize <- c(5000, 2000, 1500, 1000)
increments <- 500

for (ws in windowsize) {
  
#itt <- ( (length(bank_time$y)-(2*ws)) %/% increments)
#print(itt)


for (i in models)
  {
  for(c in 1:((nrow(bank_time)-(2*ws)) %/% increments)) # iterations rolling window
    {
    
      tr1 <- (1+(c-1)*increments)
      tr2 <- ((1+(c-1)*increments)+ws)
      ts1 <- (((1+(c-1)*increments)+ws)+1)
      ts2 <- (((1+(c-1)*increments)+2*ws)+1)
    
      #subsets for training and testing
      bank_time_ss_tr <- subset(bank_time[which(bank_time$time_axis >= tr1 & bank_time$time_axis <= tr2), ])
      bank_time_ss_ts <- subset(bank_time[which(bank_time$time_axis >= ts1 & bank_time$time_axis <= ts2), ])
    
      #Modeling and Predictions 
      M <- fit(y~.,bank_time_ss_tr,model=i, task = "prob")
      P <- predict(M, bank_time_ss_ts, type = "prob")
    
      #Perfomance measure
      cat("---Rolling Window model", i, "with", c, "th iteration","@ window-size",ws,"---", "\n")
      C1=mmetric(bank_time_ss_ts$y,P,metric="AUC")
      C2=mmetric(bank_time_ss_ts$y,P,metric="ALIFT")
      C3=mmetric(bank_time_ss_ts$y,P,metric="ACC")
      C4=mmetric(bank_time_ss_ts$y,P,metric="CONF")
      
    
      #Print findings
      cat("AUC of", i, ":", C1, "\n")
      cat("ALIFT of", i, ":", C2, "\n")
      cat("ACC of", i, ":", C3, "\n")
     
      # Stack values
      C0_t <- c(C0_t, c)
      C1_t <- c(C1_t, C1)
      C2_t <- c(C2_t, C2)
      C3_t <- c(C3_t, C3)
      C4_t <- c(C4_t, i)
      C5_t <- c(C5_t, tr1)
      C6_t <- c(C6_t, tr2)
      C7_t <- c(C7_t, ts1)
      C8_t <- c(C8_t, ts2)
      C9_t <- c(C9_t, ws)
      C10_t <- c(C10_t, "no")
    gc()

  }  } }


cat("---time---")
print(t)

#Combine Data Frame
rolling_window_sum <- cbind(C0_t,C4_t,C1_t,C2_t,C3_t,C5_t,C6_t,C7_t,C8_t, C9_t, C10_t)
#Label Data Frame
colnames(rolling_window_sum) <- c("Itteration","Model","AUC", "ALIFT", "ACC", "TR1", "TR2", "TS1", "TS2", "Window-size", "Clustering")

#Show Table (back check)
head(rolling_window_sum)

# Write file 
write.table(rolling_window_sum, "c:/users/qnect/desktop/rolling_window.txt", sep=";")

gc()





