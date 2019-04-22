
#Loading required Library to be used
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")


#Reading data from URL, the data is taken from Kaggle and Stored in Github
url <- 'https://raw.githubusercontent.com/haducvinh/customer_retention/master/WA_Fn-UseC_-Telco-Customer-Churn.csv'
url_dat <- getURL(url)

all_data <- read.csv('C:/Users/vinh.haduc/Downloads/telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))
all_data <- read_csv( url_dat)

#Look And feel of data
head(all_data)
sapply(all_data, typeof)
str(all_data)


#Data Cleaning/Grouping/ Data preparation

#Remove NA/ Show the percentage of NA data
sapply(all_data[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

#Probing data values for grouping
paste("The Minimum value is: ",min(all_data$tenure)," and the Maximum value is: ",max(all_data$tenure))




# create the goupping function based on maxium and minimum value discovered above
F_Grouping <- function(tn){
  if (tn >= 0 & tn <= 12){
    return('0-12')
  }else if(tn > 12 & tn <= 24){
    return('12-24')
  }else if (tn > 24 & tn <= 48){
    return('24-48')
  }else if (tn > 48 & tn <=60){
    return('48-60')
  }else if (tn > 60){
    return('> 60')
  }
}

# Apply grouping function
all_data$GrpTenure <- sapply(all_data$tenure,F_Grouping)
# Set the new column as factor
all_data$GrpTenure <- as.factor(all_data$GrpTenure)
# Checking the grouping bins frequency
table(all_data$GrpTenure)
barplot(table(all_data$GrpTenure), col=c("#d0d0d0"))

# Change value to factor for "Senior Citizen"
all_data$SeniorCitizen <- as.factor(
  mapvalues(all_data$SeniorCitizen,
            from=c("0","1"),
            to=c("No", "Yes"))
)

# Change value to factor for "Churn"
all_data$Churn  <- factor(all_data$Churn)

# Function for Plotting
F_Plot <- function(dst, column, name) {
  plt <- ggplot(dst, aes(x=column, fill=(Churn))) + 
    ggtitle(name) + 
    xlab(name) +
    ylab("Percentage")  +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.7) + 
    theme_minimal() +
    theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values=c("#d0d0d0", "#E69F00"))
  return(plt)
}


# Plot 1 by gender 
p1 <- F_Plot(all_data, all_data$gender, "Gender")                      
# plot 2 by Senior Citizen
p2 <- F_Plot(all_data, all_data$SeniorCitizen, "Senior Citizen")
# plot 3 by Partner
p3 <- F_Plot(all_data, all_data$Partner, "Partner")
# plot 4 by Dependents
p4 <- F_Plot(all_data, all_data$Dependents, "Dependents")
# plot 5 by Phone Service
p5 <- F_Plot(all_data, all_data$PhoneService, "Phone Service")
# plot 6 by Multiple Lines
p6 <- F_Plot(all_data, all_data$MultipleLines, "Multiple Lines")
# plot 7 by Internet Service
p7 <- F_Plot(all_data, all_data$InternetService, "Internet Service")
# plot 8 by Online Security
p8 <- F_Plot(all_data, all_data$OnlineSecurity, "Online Security")

# draw the plot grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=4)




# Plot 1 by OnlineBackup 
p1 <- F_Plot(all_data, all_data$OnlineBackup, "Online Backup")                      
# plot 2 by DeviceProtection
p2 <- F_Plot(all_data, all_data$DeviceProtection, "Device Protection")
# plot 3 by TechSupport
p3 <- F_Plot(all_data, all_data$TechSupport, "Tech Support")
# plot 4 by StreamingTV
p4 <- F_Plot(all_data, all_data$StreamingTV, "Streaming TV")
# plot 5 by StreamingMovies
p5 <- F_Plot(all_data, all_data$StreamingMovies, "Streaming Movies")
# plot 6 by PaperlessBilling
p6 <- F_Plot(all_data, all_data$PaperlessBilling, "Paperless Billing")
# plot 7 by PaymentMethod
p7 <- F_Plot(all_data, all_data$PaymentMethod, "Payment Method")
# plot 8 by GrpTenure
p8 <- F_Plot(all_data, all_data$GrpTenure, "Grp. Tenure")

# draw the plot grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=4)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"



# create the test dataset with only the testing columns
factor_to_keep <- c('Churn','SeniorCitizen','Partner','Dependents','GrpTenure','PhoneService',
                'MultipleLines','InternetService','OnlineSecurity','OnlineBackup','DeviceProtection',
                'TechSupport','StreamingTV','StreamingMovies','Contract','PaperlessBilling',
                'PaymentMethod','MonthlyCharges')

# let's split the dataset into two
DstTrainTest <- all_data[,factor_to_keep]
idxSplit <- createDataPartition(all_data$Churn, p = 0.75, list=FALSE)
DstTrainModel <- DstTrainTest[idxSplit,]
DstTestModel <- DstTrainTest[-idxSplit,]


trainX <- DstTrainModel[,names(DstTrainModel) != "Churn"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues



# logistic regression
set.seed(7)
fit.glm <- train(Churn ~ ., data=DstTrainModel, method="glm", metric=metric, trControl=control)

# linear algorithms
set.seed(7)
fit.lda <- train(Churn ~ ., data=DstTrainModel, method="lda", metric=metric, trControl=control)

# CART
set.seed(7)
fit.cart <- train(Churn ~ ., data=DstTrainModel, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(Churn ~ ., data=DstTrainModel, method="knn", 
                 metric=metric, trControl=control, preProcess = c("center","scale"), tuneLength = 5)

# SVM
set.seed(7)
fit.svm <- train(Churn ~ ., data=DstTrainModel, method="svmRadial", metric=metric, trControl=control)


# Random Forest
set.seed(7)
fit.rf <- train(Churn ~ ., data=DstTrainModel, method="rf", metric=metric, trControl=control)

# Gradient Boost Machine (GBM)
set.seed(7)
fit.gbm <- train(Churn ~ ., data=DstTrainModel, method="gbm", 
                 metric=metric, trControl=control, verbose=FALSE)


# summarize accuracy of models
results <- resamples(list(
  glm=fit.glm, 
  lda=fit.lda, 
  cart=fit.cart, 
  knn=fit.knn, 
  svm=fit.svm, 
  rf=fit.rf,
  gbm=fit.gbm
))
#summary(results)
# compare accuracy of models
dotplot(results)





calculate_accuracy <- function(TestFit, name) {
  # prediction 
  DstTestModelClean <- DstTestModel
  DstTestModelClean$Churn <- NA
  predictedval <- predict(TestFit, newdata=DstTestModelClean)
  
  # summarize results with confusion matrix
  cm <- confusionMatrix(predictedval, DstTestModel$Churn)
  
  # calculate accuracy of the model
  Accuracy<-round(cm$overall[1],2)
  acc <- as.data.frame(Accuracy)
  
  roc_obj <- roc(DstTestModel$Churn, as.numeric(predictedval))
  acc$Auc <- auc(roc_obj)
  
  acc$FitName <- name
  return(acc)
}



accuracy_all <- calculate_accuracy(fit.glm, "glm")
accuracy_all <- rbind(accuracy_all, calculate_accuracy(fit.lda, "lda"))
accuracy_all <- rbind(accuracy_all, calculate_accuracy(fit.cart, "cart"))
accuracy_all <- rbind(accuracy_all, calculate_accuracy(fit.knn, "knn"))
accuracy_all <- rbind(accuracy_all, calculate_accuracy(fit.svm, "svm"))
accuracy_all <- rbind(accuracy_all, calculate_accuracy(fit.rf, "rf"))
accuracy_all <- rbind(accuracy_all, calculate_accuracy(fit.gbm, "gbm"))
rownames(accuracy_all) <- c()
arrange(accuracy_all,desc(Accuracy))

