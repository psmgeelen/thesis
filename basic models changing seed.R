#Load File, Load Packages
bank<-read.csv("bank-additional-full.csv",header=TRUE,sep=";")
library(rminer)

#Create artificial time-axis beforehand though. The website noted that the values were chronologi-
#cally sorted. Therefore a simple itemnumber identifies a chronology. 
time_axis <- as.numeric(rownames(bank))
bank_time <- cbind(bank, time_axis)


#Set modeling techniques, for more information see description in rminer documentation
models_name <- rbind("Decision Tree", "Support Vector Machine", "Neural Network", "LOGIT")
models_specs <-rbind("ctree","ksvm","mlp","lr")

models <- cbind(models_name, models_specs)
colnames(models) <- c("Name", "Model")

#Variable prep
C0_t <- vector(mode="numeric", length=0)
C1_t <- vector(mode="numeric", length=0)
C2_t <- vector(mode="numeric", length=0)
C3_t <- vector(mode="numeric", length=0)
C4_t <- vector(mode="character", length=0)
C5_t <- vector(mode="character", length=0)
C6_t <- vector(mode="numeric", length=0)


# Hold-out (train, test sets)
H=holdout(bank_time$y,ratio=2/3)

#----------------------Modeling----------------------------#

for (a in 1:20) {
  for (i in seq_len(nrow(models))) {

  # Setting seed 
  set.seed(a)
  
  #Modeling 
  M=fit(y~.,bank_time[H$tr,], model = models[i,2],  task="class")
  M2=fit(y~.,bank_time[H$tr,], model = models[i,2], task="prob")
  
  # Creating variables model
  P=predict(M,bank_time[H$ts,])
  P2=predict(M2,bank_time[H$ts,])
  
    #Title
  cat(paste("----- Results for model", models[i,1], "-----", "\n"))
  # AUC of ROC
  cat(paste("AUC for ROC for model", models[i,1], "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"AUC"), "\n")
  # AUC ALIFT
  cat(paste("AUC for ALIFT model", models[i,1], "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"ALIFT"), "\n")
    # Accuracy
  cat(paste("Accuracy Pred. model", models[i,1], "= "))
  cat(mmetric(bank_time$y[H$ts],P,"ACC"), "\n")
  
  C1=mmetric(bank_time$y[H$ts],P2,metric="AUC")
  C2=mmetric(bank_time$y[H$ts],P2,metric="ALIFT")
  C3=mmetric(bank_time$y[H$ts],P,metric="ACC")
  C4=models[i,1]
  
  C1_t = c(C1_t,C1)
  C2_t = c(C2_t,C2)
  C3_t = c(C3_t,C3)
  C4_t = c(C4_t,C4)
  C5_t = c(C5_t,"none")
  C6_t = c(C6_t,a)
  
  }
}
holdout_dataset <- cbind(C1_t,C2_t,C3_t,C4_t,C5_t,C6_t)
colnames(holdout_dataset)<-c("AUC", "ALIFT", "ACC", "model", "Clustering", "seedNr.")

head(holdout_dataset)

write.table(holdout_dataset, "holdout_normal.txt", sep=";")

########Reset Memory
gc()





