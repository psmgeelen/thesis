#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
install.packages(rminer)
library(rminer)

#Data Prep Phase 
boxplot(bank$age)
boxplot(bank$duration)
boxplot(bank$emp.var.rate)
boxplot(bank$cons.conf.idx)
boxplot(bank$euribor3m)

#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time <- cbind(bank, time_axis)

#Set modeling techniques, for more information see description in rminer documentation
models <- c("ctree", "ksvm", "mlpe", "lr")

# Hold-out (train, test sets)
H=holdout(bank_time$y,ratio=2/3)

#----------------------Modeling----------------------------#

for (i in models) {

  M=fit(y~.,bank_time[H$tr,],model=i, task="class")
  M2=fit(y~.,bank_time[H$tr,],model=i, task="prob")
  
  # Creating variables model
  P=predict(M,bank_time[H$ts,])
  P2=predict(M2,bank_time[H$ts,])
  
  
  #Title
  cat(paste("----- Results for model", i, "-----"))
  # Print stats. 
  # CONF Matrix 
  print(paste("Confusion Matrix Predicition/Probability", i))
  print(mmetric(bank_time$y[H$ts],P,"CONF"))
  print(mmetric(bank_time$y[H$ts],P,"CONF"))
  # AUC of ROC
  cat(paste("AUC for ROC for model", i, "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"AUC"), "\n")
  # AUC ALIFT
  cat(paste("AUC for ALIFT model", i, "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"ALIFT"), "\n")
  # Accuracy
  cat(paste("Accuracy Pred. model", i, "= "))
  cat(mmetric(bank_time$y[H$ts],P,"ACC"), "\n")
  cat(paste("Accuracy Prob. model", i, "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"ACC"), "\n") 
  
  #ROC
  mgraph(bank_time$y[H$ts],P2,graph="ROC",TC=2,main=paste("ROC Curve for",i),
       baseline=TRUE,leg=i,Grid=10)
  #LIFT
  mgraph(bank_time$y[H$ts],P2,graph="LIFT",TC=2,main=paste("LIFT Curve for", i),
       baseline=TRUE,leg=i,Grid=10)
}



########Reset Memory
gc()





