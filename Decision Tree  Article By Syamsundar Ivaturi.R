### Setting Working Directory
setwd("E:\\Business Analytics\\Article\\Decision Tree  Article By Syamsundar Ivaturi.R")
## Load library "Party" for running the Decision Tree
library(partykit)
##Loading required package: grid
##Loading required package: libcoin
##Loading required package: mvtnorm
##Loading required package: rpart
##Warning messages:
##1: package 'partykit' was built under R version 3.4.3 
##2: package 'libcoin' was built under R version 3.4.3 
##3: package 'rpart' was built under R version 3.4.3
cust_date<-read.csv("E:\\Business Analytics\\Article\\Default On Payment.csv")
cust_data<-cust_data[complete.cases(cust_data),]
cust_data$Default_Payment<-factor(ifelse(cust_data$Default_On_Payment==1,"Default","Non Default"))
#lets create distrubtion on new variable "Default_Payment"
table(cust_data$Default_Payment)
#Default Non Default 
#12001       28118 
pie(table(cust_data$Default_Payment))
summary(cust_data) 
# Lets prepare CART model
library(rpart)
cust_data<-read.csv("E:\\Business Analytics\\Article\\Default On Payment.csv")
fit<-rpart(Default_On_Payment~Status_Checking_Acc+Credit_History, data=cust_data, method="class", 
           control=rpart.control(minsplit=50, cp=0.001))
summary(fit)
printcp(fit)
pfit<-prune(fit,cp=0.001)
plot(pfit, uniform=TRUE, main="Classification Tree for Default_on_payment")
###label the decision tree plot 
text(pfit,splits = TRUE, use.n=TRUE, all=TRUE, cex=0.5, pretty=1)
labels(pfit)
install.packages(rpart)
library(rpart)
prp(pfit,type=3,extra=2,under=true)
library(rpart.plot)
