#read cvs
mydata<-read.csv("E:\\Business Analytics\\Machine Learning\\Assesment\\bank-full-cleaned.csv")
#structure of data
str(mydata)
#no of row of data
NROW(mydata)
#table for y column
table(mydata$y)
#unique data from total no of data(unknown data)
unique(mydata)
#table for particular job column 
table(mydata$job)
#subset job not equal to unknown into new dataframe
mydata1<-subset(mydata,subset=(job!="unknown"))
mydata1
#recheck unknown is present or not
table(mydata1$job)
#table for particular education column and repeat
table(mydata1$education)
mydata2<-subset(mydata1,subset=(education!="unknown"))
table(mydata2$education)
table(mydata2$contact)
table(mydata2$default)
#barplot for yes or no
barplot(table(mydata2$default))
table(mydata2$poutcome)
#clean the unknown data in single line for all 3 variables(-c is excluding mentioned variables)
mydata3<-subset(mydata2, select=-c(default, duration, poutcome, contact))
table(mydata3$y)
#yvar changing to yes=1 and n0=2 (ifelse statement)
mydata3$yvar<-ifelse(mydata3$y=="yes",1,2)
mydata3
table(mydata3$yvar)
write.csv(mydata3, file= "E:\\Business Analytics\\Machine Learning\\Assesment\\cleaned bankdata.csv")
#changing as factor for data3 which is numeric
mydata3$yvar<-as.factor(mydata3$yvar)
#droping y column which is not required
mydata4<-subset(mydata3, select=-c(y))
table(mydata4$yvar)
NROW(mydata4)
#unique for pdays column
unique(mydata4$pdays)
table(mydata4$pdays)
#bar plot og yvar
barplot(table(mydata4$yvar))
#percentage of yvar
prop.table(table(mydata4$yvar))
#structure of mydata4
str(mydata4)
#library ggplot2
library(ggplot2)
##histogram for age
hist(mydata4$age)
#overlay for yvar and age
ggplot()+geom_bar(data = mydata4, aes(x=(mydata4$age), fill=factor(mydata4$yvar)), position="fill")
#table for campaign
table(mydata4$campaign)
#plot for campaign
ggplot()+geom_bar(data = mydata4, aes(x=(mydata4$campaign), fill=factor(mydata4$yvar)), position="fill")
nrow(mydata4)
#barplot for job
barplot(table(mydata4$job))
#seting
set.seed(123)
nrow(mydata4)
#to mix the data(shuffle)
datamixed=mydata4[order(runif(43193)), ]
datamixed
#data aplitting into trainig and testing data
traindata<-datamixed[1:30235, ]
testdata<-datamixed[30235:43193, ]
#modeling basic model
library(C50)
modelc5<-C5.0(traindata$yvar~.,data=traindata)
#predict
predictedc5=predict(modelc5,testdata[,1:12])
# install pacakage caret
install.packages("caret")
library(caret)
str(testdata)
plot(modelc5)
#confusion matrix
confusionMatrix(predictedc5,testdata[,13])
#table for test and test data
table(traindata$yvar)

##ordering the dataset to 70% and 30% (-xtfrm used for ordinal data) with "No" variables )
ordereddata<-traindata [ order(-xtfrm(traindata$yvar)), ]
str(ordereddata)
table(ordereddata$yvar)

#chopping tje 1st 18655 data with "No" data:
sampletraindata1<-ordereddata[18656:30235, ]


#barplot
barplot(table(sampletraindata1$yvar))


##re run the model with c50
modelc5_u1<-C5.0(sampletraindata1$yvar~.,data=sampletraindata1)
predictedc5_u1=predict(modelc5_u1,testdata[,1:12])
plot(modelc5_u1)
confusionMatrix(predictedc5_u1,testdata[,13])
#create ROC curve and AUC value

#run at 40% and 60% data
sampletraindata2<-ordereddata[21559:30235,]
barplot(table(sampletraindata2$yvar))
modelc5_u2<-C5.0(sampletraindata2$yvar~.,data=sampletraindata2)
predictedc5_u2=predict(modelc5_u2,testdata[,1:12])
plot(modelc5_u2)
confusionMatrix(predictedc5_u2,testdata[,13])
##nnet model(neural network)
install.packages("NeuralNetTools")
library(NeuralNetTools)
#model for nnet
modelnnet<-nnet(sampletraindata1$yvar~.,size=7,data=sampletraindata1)

##no need load package. we can use :: to use that library:
NeuralNetTools::plotnet(modelnnet)
#predict class 
predictednnet=predict(modelnnet,testdata[,1:12], type="class")
confusionMatrix(predictednnet,testdata[,13])

library(C50)

#all models:
m<-train(sampletraindata1[,1:11], y=sampletraindata1$yvar, method="C5.0")
str(sampletraindata1)
sampletraindata1
predictedm=predict(m,testdata[,1:12])
confusionMatrix(predictedm,testdata[,13])


#run neural net
modelc5_u4<-C5.0(sampletraindata2$yvar~.,data=sampletraindata2, trail =100, rules = TRUE)
predictedc5_u4=predict(modelc5_u4,testdata[,1:12])
confusionMatrix(predictedc5_u4,testdata[,13])
