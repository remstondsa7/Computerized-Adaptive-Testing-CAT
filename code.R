#install.packages('eRm')
#install.packages('ltm')
#install.packages("writexl")
library(eRm)
library(ltm)
library(party)
library(writexl)
options(warn=-1)

Weights <-function(probability){
  #weights it assigns.
  p=(1/((1/probability)-1))
  wt=log(p)
  return(wt)
}

Probability<-function(x){

  #To device an optimization strategy for the model 
  model=rasch(x,IRT.param=TRUE)
  weight<-coef(model,prob=TRUE)[,3]
  weights(as.data.frame(weight)) 
  probability=as.data.frame(weight)
  return(probability)
}

Self_Adaptive<-function(x,y){
  return(rbind(x,y)) #To make the model self-reliant and adaptive to diverse response sets
}

# To read a set of responses per question, for the entire set, as input.
Q_data_train<-read.csv('AI-DataTrain.csv',stringsAsFactors=FALSE)

# To read a set of questions from the set and their response, correct or wrong, as test-input.
Q_data_test<-read.csv('AI-DataTest.csv',stringsAsFactors=FALSE)
A<-Q_data_train[10:34]
col=colnames(A)

#To train the model on said training-input and assign difficulty weights per question.
model=rasch(A,IRT.param=TRUE)#RASCH model
difficulty_discrimination_probability=coef(model,prob=TRUE)
probability<-coef(model,prob=TRUE)[,3]
weights<-Weights(as.data.frame(probability))
write_xlsx(data.frame(Question=col,Weights=weights),"result.xls") 


#To record responses and add it to your training dataset..
A=Self_Adaptive(A,A[3,])
probability=Probability(A)
model=rasch(A,IRT.param=TRUE)#RASCH model

#Plots
Q_d_tr=RM(A,sum0=FALSE) #Training data
plotjointICC(Q_d_tr, cex = .6) #ICC plot
plotPImap(Q_d_tr, cex.gen = .55, sorted = TRUE) #Person-Item Map
Q_2pl<-ltm(A ~ z1) #2PL
plot(Q_2pl) #Item Characteristics Curves-Unordered
plot(model) #Item Characteristics Curves-Ordered
plotPWmap(Q_d_tr) #Item Map
plot(Q_2pl, type = "IIC", ylim = c(0, 0)) #Item Information Curves

#Model Fit
anova(model, Q_2pl) #Likelihood Ratio Table
cor(coef(model)[, 1], coef(Q_2pl)[, 1]) #Correlation
Waldtest(Q_d_tr) #z-values

#MAP and EAP
pp_ml <- coef(person.parameter(Q_d_tr))
pp_map <- factor.scores(model, method = "EB", resp.patterns = A)
pp_eap <- factor.scores(model, method = "EAP", resp.patterns =A)
tmp1 <- data.frame(ML = pp_ml, MAP = pp_map$score.dat$z1, EAP = pp_eap$score.dat$z1)
round(cor(tmp1), 4)


#The test-input should be used as the last 100 rows(students: 900-999) of the training set, and any 10 columns (questions: Qx)
m=dim(A)[2]/2-floor(dim(A)[2]/2)
f="floor"
m
if(m!=0){
  f="ceiling"
  
}
toString(f)
if (toString(f)==toString("ceiling")){
  g=floor
  f=ceiling
}else{
  g=ceiling
  f=floor
}

start=1
train_mid1=g(dim(A)[2]/4)
train_mid2=f(dim(A)[2]-dim(A)[2]/4)
end=dim(A)[2]

test_mid1=f(dim(A)[2]/4)
test_mid2=g(dim(A)[2]-dim(A)[2]/4)


(end-train_mid2)+(train_mid1)==test_mid2-test_mid1+ceiling(m)
(end-train_mid2)+(train_mid1)
test_mid2-test_mid1+ceiling(m)
train=1
train_s=floor(dim(A)[1]*0.9)
test_s=floor(dim(A)[1]*0.9+1)
test=dim(A)

weights<-(Weights(probability))
write_xlsx(data.frame(Question=col,new_Weights=weights),"output.xls") 

library(class)
x_train<-subset(A,select=c(start:train_mid1,train_mid2:end))[train:train_s,]
dim(x_train)
x_test<- subset(A,select=c(test_mid1:(test_mid2+ceiling(m))))[test_s:test,]
dim(x_test)


for (i in test_mid1:test_mid2){
  y_train<-A[train:train_s,i]
  y_test<-A[test_s:test,i]
  pred<-knn(train=x_train, cl=y_train , k=21, test=x_test)
  print(pred)
  print(y_test)
  frequency=as.matrix(data.frame(table(pred,y_test)))
  actual_preds<-data.frame(cbind(actuals=y_test,predicted=pred))
  actual_preds[,'predicted']=as.numeric(actual_preds[,'predicted']==2)
  print(frequency)
}


