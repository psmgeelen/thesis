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


for (i in models)
  {
  for(c in 1:((nrow(bank_time)-(2*ws)) %/% increments)) # iterations rolling window
    {
      # Window
      w1 <- (1+(c-1)*increments)
      w2 <- ((1+(c-1)*increments)+ws)
      data <- subset(bank_time[which(bank_time$time_axis >= w1 & bank_time$time_axis <= w2), ])
      
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
     
      # Stack values
      C0_t <- c(C0_t, c)
      C1_t <- c(C1_t, C1)
      C2_t <- c(C2_t, C2)
      C3_t <- c(C3_t, C3)
      C4_t <- c(C4_t, i)
      C5_t <- c(C5_t, w1)
      C6_t <- c(C6_t, w2)
      C9_t <- c(C9_t, ws)
      C10_t <- c(C10_t, "no")
    
      data <- 0
      data_tr <- 0
      data_ts <- 0
      
      gc()

  }  } }


cat("---time---")
print(t)

#Combine Data Frame
rolling_window_sum <- cbind(C0_t,C4_t,C1_t,C2_t,C3_t,C5_t,C6_t,C9_t, C10_t)
#Label Data Frame
colnames(rolling_window_sum) <- c("Itteration","Model","AUC", "ALIFT", "ACC", "Lower", "Upper", "Window-size", "Clustering")

#Show Table (back check)
head(rolling_window_sum)

# Write file 
write.table(rolling_window_sum, "rolling_window.txt", sep=";")

gc()





