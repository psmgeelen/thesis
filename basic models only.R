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
bank_time1 <- bank_time[1:10000,]

#Set modeling techniques, for more information see description in rminer documentation
models_name <- rbind("Decision Tree", "Support Vector Machine", "Neural Network", "Naive", "LOGIT")
models_specs <-rbind("ctree","ksvm","mlpe","naive","lr")
#models_additional_settings <- rbind(stuff)
models <- cbind(models_name, models_specs)
colnames(models) <- c("Name", "Specs")

# Hold-out (train, test sets)
H=holdout(bank_time$y,ratio=1/3)

#----------------------Modeling----------------------------#

for (i in seq_len(nrow(models))) {

  M=fit(y~.,bank_time[H$tr,], model = models[i,2],  task="class")
  M2=fit(y~.,bank_time[H$tr,], model = models[i,2], task="prob")
  
  # Creating variables model
  P=predict(M,bank_time[H$ts,])
  P2=predict(M2,bank_time[H$ts,])
  
    #Title
  cat(paste("----- Results for model", models[i,1], "-----"))
  # Stats. 
  
  # CONF Matrix 
  print(paste("Confusion Matrix Predicition/Probability", models[i,1] ))
  print(mmetric(bank_time$y[H$ts],P,"CONF"))
  # AUC of ROC
  cat(paste("AUC for ROC for model", models[i,1], "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"AUC"), "\n")
  # AUC ALIFT
  cat(paste("AUC for ALIFT model", models[i,1], "= "))
  cat(mmetric(bank_time$y[H$ts],P2,"ALIFT"), "\n")
    # Accuracy
  cat(paste("Accuracy Pred. model", models[i,1], "= "))
  cat(mmetric(bank_time$y[H$ts],P,"ACC"), "\n")
  
  #ROC
  mgraph(bank_time$y[H$ts],P2,graph="ROC",TC=2,main=paste("ROC Curve for",models[i,1]),
       baseline=TRUE,leg=models[i,1],Grid=10)
  #LIFT
  mgraph(bank_time$y[H$ts],P2,graph="LIFT",TC=2,main=paste("LIFT Curve for", models[i,1]),
       baseline=TRUE,leg=models[i,1],Grid=10)
  
    }


########Reset Memory
gc()





