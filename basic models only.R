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
H=holdout(bank_time$y,ratio=2/3)

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
  #Perfomance measure
  cat("---Rolling Window model", i, "with", c, "th iteration---", "\n")
  C1=mmetric(data_ts$y,P,metric="AUC")
  C2=mmetric(data_ts$y,P,metric="ALIFT")
  C3=mmetric(data_ts$y,P,metric="ACC")
  C4=mmetric(data_ts$y,P,metric="CONF")
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
  
  #ROC
  mgraph(bank_time$y[H$ts],P2,graph="ROC",TC=2,main=paste("ROC Curve for",models[i,1]),
       baseline=TRUE,leg=models[i,1],Grid=10)
  #LIFT
  mgraph(bank_time$y[H$ts],P2,graph="LIFT",TC=2,main=paste("LIFT Curve for", models[i,1]),
       baseline=TRUE,leg=models[i,1],Grid=10)
  
    }




########Reset Memory
gc()





