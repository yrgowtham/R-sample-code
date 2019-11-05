#Loan prediction dataset 
library(dplyr)
library(ggplot2)
library(rpart)

train <- read.csv("train_ctrUa4K.csv",na.strings = c("","NaN"," "))
test <- read.csv("test_lAUu6dG.csv",na.strings = c("","NaN"," "))

#Add target variable in test
test_lAUu6dG$Loan_Status<-as.factor("NA")
#combine train and test dataset 
df.loan<-rbind(train_ctrUa4K,test_lAUu6dG)

#Missing values Summary
Variable <- colnames(df.loan)
NA_count <- sapply(df.loan, function(x) sum(is.na(x)))
miss_summ <- data.frame(Variable,NA_count,row.names = NULL)
miss_summ<-miss_summ %>%
  arrange(desc(NA_count))

#Bar plot of missing data
z<-ggplot(data=miss_summ,aes(x=reorder(Variable,NA_count),y=NA_count))+
  geom_bar(stat="identity", color="blue", fill="yellow")
z + coord_flip()

#Target Variable distibution 
barplot(prop.table(table(train$Loan_Status)))
summary(train)
par(mfrow=c(1,2))
boxplot(train$ApplicantIncome,train$CoapplicantIncome,names=c("App Income","Coapp Income"),main="train set")
boxplot(test$ApplicantIncome,test$CoapplicantIncome,names=c("App Income","Coapp Income"),main="test set")
