
# FORECASTING DEMAND MODELS
#By: Natalia Clivio
#nclivio@gmail.com
#
#---------------------------Load and clean data---------------------------------
#The file loaded is the telecommunication companies´s subscribers by   ten years,
#for each service provided by this companies. The service offering are:
#Internet residential and business, Video on demand and VoIP.

library(caret)
library(forecast)
library(growthmodels)

subs<-read.csv("C:/Users/NataliaA/Documents/DataR/Subscribers_CV.csv",header=TRUE,
               sep=";",na.strings="NA",dec=",")

years<-na.exclude(subs[1])
oper1<-na.exclude(subs[1:5])
oper2<-data.frame(years,na.exclude(subs[6:9]))
oper3<-data.frame(years,na.exclude(subs[10:13]))

#---------------------------Data Preparation------------------------------------
set.seed(2321)  #No random subset

#Operator 1
inTrain<-createDataPartition(y=oper1$Year, p=0.75, list=FALSE)
train1<-oper1[inTrain,]
test1<-oper1[-inTrain,]

#Operator 2
inTrain<-createDataPartition(y=oper2$Year, p=0.75, list=FALSE)
train2<-oper2[inTrain,]
test2<-oper2[-inTrain,]

#Operator 3
inTrain<-createDataPartition(y=oper3$Year, p=0.75, list=FALSE)
train3<-oper3[inTrain,]
test3<-oper3[-inTrain,]

year<-years$Year
start<-year[1]
hi<-dim(years)[1]
end<-year[hi]
year1<-year[round(hi*0.70)]

seth<-8   ##Set predictions quantity
f1<-end+1 #Q forecast
f2<-end+5 #Q forecast

time<-1:hi

#----------------Demand Models for Internet  Residential Subscribers--------------

##Subset data
#Operator 1
trainres1<-data.frame(Year=train1$Year,Subs=train1$Internet_Res1)
testres1<-data.frame(Year=test1$Year,Subs=test1$Internet_Res1)
datares1<-data.frame(Year=year,Subs=oper1$Internet_Res1)
#Operator 2
trainres2<-data.frame(Year=train2$Year,Subs=train2$Internet_Res2)
testres2<-data.frame(Year=test2$Year,Subs=test2$Internet_Res2)
datares2<-data.frame(Year=year,Subs=oper2$Internet_Res2)
#Operator 3
trainres3<-data.frame(Year=train3$Year,Subs=train3$Internet_Res3)
testres3<-data.frame(Year=test3$Year,Subs=test3$Internet_Res3)
datares3<-data.frame(Year=year,Subs=oper3$Internet_Res3)


##1. Predicting with Random Forest InRes
#Operator 1
modrf1<-train(Subs~.,data=trainres1,method="rf",prox=TRUE)  #Model
predrf1<- predict(modrf1,datares1)                          #Prediction
forerf1<-forecast(predrf1,h=seth)                           #Forecast

#Operator2
modrf2<-train(Subs~.,data=trainres2,method="rf",prox=TRUE)  #Model
predrf2<- predict(modrf2, datares2)                         #Prediction
forerf2<-forecast(predrf2,h=seth)                           #Forecast

#Operator 3
modrf3<-train(Subs~.,data=trainres3,method="rf",prox=TRUE)  #Model
predrf3<- predict(modrf3, datares3)                         #Prediction
forerf3<-forecast(predrf3,h=seth)                           #Forecast


##2. Predicting with Boosting Generalized Additive Model InRes
#Operator 1
modbs1 <- train(Subs ~ ., method = "gamboost", data = trainres1) 
predbs1 <- predict(modbs1, datares1)
forebs1<-forecast(predbs1,h=seth) 

#Operator 2
modbs2 <- train(Subs ~ ., method = "gamboost", data = trainres2) 
predbs2 <- predict(modbs2, datares2)
forebs2<-forecast(predbs2,h=seth) 

#Operator 3
modbs3 <- train(Subs ~ ., method = "gamboost", data = trainres3) 
predbs3 <- predict(modbs3, datares3)
forebs3<-forecast(predbs3,h=seth)

#Predictions
data_pred1a<-data.frame(datares1,Rf1=predrf1,Bs1=predbs1)           
data_pred2a<-data.frame(datares2,Rf2=predrf2,Bs2=predbs2)
data_pred3a<-data.frame(datares3,Rf3=predrf3,Bs3=predbs3) 

##Forecasting for 5 Years

setrf1<-data.frame(forerf1)  #randon forest predictions Op1
rfa<-data.frame(Fore_Rf1=setrf1$Point.Forecast[4:8])
setrf2<-data.frame(forerf2)  #randon forest predictions Op2
rfb<-data.frame(Fore_Rf2=setrf2$Point.Forecast[4:8])
setrf3<-data.frame(forerf3)  #randon forest predictions Op3
rfc<-data.frame(Fore_Rf3=setrf3$Point.Forecast[4:8])

setbs1<-data.frame(forebs1)  #Boosting predictions Op1
bsa<-data.frame(Fore_Bs1=setbs1$Point.Forecast[4:8])
setbs2<-data.frame(forebs2)  #Boosting predictions Op2
bsb<-data.frame(Fore_Bs2=setbs2$Point.Forecast[4:8])
setbs3<-data.frame(forebs3)  #Boosting predictions Op3
bsc<-data.frame(Fore_Bs3=setbs3$Point.Forecast[4:8])

fore1a<-data.frame(Year=f1:f2,rfa,bsa)   #Operator 1
fore1b<-data.frame(Year=f1:f2,rfb,bsb)   #Operator 2
fore1c<-data.frame(Year=f1:f2,rfc,bsc)   #Operator 3


##3. Predicting with Growth curves InRes
#Simple random sampling of time series is probably not the best way to resample 
#times series data. Hyndman and Athanasopoulos (2013)) discuss rolling forecasting 
#origin techniques that move the training and test sets in time .

#Operator1
tssubs1<-ts(oper1$Internet_Res1,start=start,end=end) #Time serie
tsset1<-tssubs1
tstrain1<-window(tsset1,start=start,end=year1)        #Training and Testing data
tstest1<-window(tsset1,start=year1,end=end)       

#Operator2
tssubs2<-ts(oper2$Internet_Res2,start=start,end=end) #Time serie
tsset2<-tssubs2/1
tstrain2<-window(tsset2,start=start,end=year1)        #Training and Testing data
tstest2<-window(tsset2,start=year1,end=end)       

#Operator3
tssubs3<-ts(oper3$Internet_Res3,start=start,end=end) #Time serie
tsset3<-tssubs3/1
tstrain3<-window(tsset3,start=start,end=year1)        #Training and Testing data
tstest3<-window(tsset3,start=year1,end=end)       

##Linear Model
#Operator 1
fitlm1<-tslm(tstrain1~trend)       #Model
predlm1<-forecast(fitlm1, h=seth)  #Prediction
#Operator 2
fitlm2<-tslm(tstrain2~trend)       #Model
predlm2<-forecast(fitlm2, h=seth)  #Prediction
#Operator 3
fitlm3<-tslm(tstrain3~trend)       #Model
predlm3<-forecast(fitlm3, h=seth)  #Prediction

##Parabolic Model
#Operator 1
fitpar1=lm(tsset1 ~ time + I(time^2))  #Model
predpar1<-predict(fitpar1)             #Prediction
forepar1<-forecast(predpar1,h=seth-3)  #Forecast
#Operator 2
fitpar2=lm(tsset2 ~ time + I(time^2))  #Model
predpar2<-predict(fitpar2)             #Prediction
forepar2<-forecast(predpar2,h=seth-3)  #Forecast
#Operator 3
fitpar3=lm(tsset3 ~ time + I(time^2))  #Model 
predpar3<-predict(fitpar3)             #Prediction
forepar3<-forecast(predpar3,h=seth-3)  #Forecast

##Exponential Model
#Operator 1
Yexp1=lm(log(tsset1) ~ time)          #Transform
parmexp1<-as.list(Yexp1$coeff)
betae1<- exp(parmexp1$"(Intercept)")  #growth range (a)
ke1<- parmexp1$time                   #growth rate (b)
fitexp1<-betae1*exp(ke1*time)         #Model
predexp1<-forecast(fitexp1,h=seth-3)

#Operator 2
Yexp2=lm(log(tsset2) ~ time)          #Transform
parmexp2<-as.list(Yexp2$coeff)
betae2<- exp(parmexp2$"(Intercept)")  #growth range (a)
ke2<- parmexp2$time                   #growth rate (b)
fitexp2<-betae2*exp(ke2*time)         #Model
predexp2<-forecast(fitexp2,h=seth-3)

#Operator 3
Yexp3=lm(log(tsset3) ~ time)          #Transform
parmexp3<-as.list(Yexp3$coeff)
betae3<- exp(parmexp3$"(Intercept)")  #growth range (a)
ke3<- parmexp3$time                   #growth rate (b)
fitexp3<-betae3*exp(ke3*time)         #Model
predexp3<-forecast(fitexp3,h=seth-3)


#The predictions with growth curves are:

#Operator 1
set1a<-data.frame(predlm1)  #Linear predictions
lma<-data.frame(Fore_Lin1=set1a$Point.Forecast[4:8])
data_predlm1<-data.frame(Lin1=c(fitlm1$fitted.values,set1a$Point.Forecast[1:3]))
#Operator 2
set2a<-data.frame(predlm2)  #Linear predictions
lmb<-data.frame(Fore_Lin2=set2a$Point.Forecast[4:8])
data_predlm2<-data.frame(Lin2=c(fitlm2$fitted.values,set2a$Point.Forecast[1:3]))
#Operator 3
set3a<-data.frame(predlm3)  #Linear predictions
lmc<-data.frame(Fore_Lin3=set3a$Point.Forecast[4:8])
data_predlm3<-data.frame(Lin3=c(fitlm3$fitted.values,set3a$Point.Forecast[1:3]))


#Operator 1
data_predpar1<-data.frame(Par1=predpar1) #Parabolic predictions
set2a<-data.frame(forepar1)
para<-data.frame(Fore_Par1=set2a$Point.Forecast)
#Operator 2
data_predpar2<-data.frame(Par2=predpar2) #Parabolic predictions
set2b<-data.frame(forepar2)
parb<-data.frame(Fore_Par2=set2b$Point.Forecast)
#Operator 3
data_predpar3<-data.frame(Par3=predpar3) #Parabolic predictions
set2c<-data.frame(forepar3)
parc<-data.frame(Fore_Par3=set2c$Point.Forecast)


#Operator 1
set3a<-data.frame(predexp1)  #Exponential predictions
expa<-data.frame(Fore_Exp1=set3a$Point.Forecast)
data_predexp1<-data.frame(Exp1=fitexp1)
#Operator 2
set3b<-data.frame(predexp2)  #Exponential predictions
expb<-data.frame(Fore_Exp2=set3b$Point.Forecast)
data_predexp2<-data.frame(Exp2=fitexp2)
#Operator 1
set3c<-data.frame(predexp3)  #Exponential predictions
expc<-data.frame(Fore_Exp3=set3c$Point.Forecast)
data_predexp3<-data.frame(Exp3=fitexp3)


##Sets of predictions
data_pred1b<-data.frame(data_predlm1,data_predpar1,data_predexp1)  #Operator 1
data_pred2b<-data.frame(data_predlm2,data_predpar2,data_predexp2)  #Operator 2
data_pred3b<-data.frame(data_predlm3,data_predpar3,data_predexp3)  #Operator 3

##Forecasting for 5 Years
fore2a<-data.frame(Year=f1:f2,lma,para,expa)   #Operator 1
fore2b<-data.frame(lmb,parb,expb)              #Operator 2
fore2c<-data.frame(lmc,parc,expc)              #Operator 3


##4.Predicting with Logistic and Gompertz Model InRes

##Logistic Model
#Using the **growthmodels** package, with the *logistic* function to get the 
#logistic curve
#**Usage**
#logistic(t, alpha, beta, k)
#**Arguments**
#t time,x size, alpha upper asymptote, beta  growth range, k  growth rate

#Operator 1
alpha1<-tssubs1[hi] +0.5           #upper asymptote (M)
Y1<-log(alpha1/tssubs1-1)          #Transform
Reglm1<-tslm(Y1~trend)             #Lineal Regression
parmlm1<-as.list(Reglm1$coeff)

beta1<- exp(parmlm1$"(Intercept)")  #growth range (a)
k1<- -parmlm1$trend                 #growth rate (b)
fitlog1 <- logistic(1:10, alpha1, beta1, k1)
predlog1<-forecast(fitlog1,h=seth)

#Operator 2
alpha2<-tssubs2[hi] +0.5           #upper asymptote (M)
Y2<-log(alpha2/tssubs2-1)          #Transform
Reglm2<-tslm(Y2~trend)             #Lineal Regression
parmlm2<-as.list(Reglm2$coeff)

beta2<- exp(parmlm2$"(Intercept)")  #growth range (a)
k2<- -parmlm2$trend                 #growth rate (b)
fitlog2 <- logistic(1:10, alpha2, beta2, k2)
predlog2<-forecast(fitlog2,h=seth)

#Operator 3
alpha3<-tssubs3[hi] +0.5           #upper asymptote (M)
Y3<-log(alpha3/tssubs3-1)          #Transform
Reglm3<-tslm(Y3~trend)             #Lineal Regression
parmlm3<-as.list(Reglm3$coeff)

beta3<- exp(parmlm3$"(Intercept)")  #growth range (a)
k3<- -parmlm3$trend                 #growth rate (b)
fitlog3 <- logistic(1:10, alpha3, beta3, k3)
predlog3<-forecast(fitlog3,h=seth)

#The predictions using logistic model are:
set4a<-data.frame(predlog1)
alog<-set4a$Point.Forecast   #Operator 1 predictions
set4b<-data.frame(predlog2)
blog<-set4b$Point.Forecast   #Operator 2 predictions
set4c<-data.frame(predlog3)
clog<-set4c$Point.Forecast   #Operator 3 predictions

data_predlog1<-data.frame(Log1=fitlog1)
data_predlog2<-data.frame(Log2=fitlog2)
data_predlog3<-data.frame(Log3=fitlog3)

###Gompertz Model
#Using the **growthmodels** package, Computes the Gompertz growth model
#**Usage**
#gompertz(t, alpha, beta, k)
#**Arguments**
#t     time
#x     size
#alpha upper asymptote
#beta  growth displacement
#k     growth rate

#Operator 1
alphag1<-tssubs1[hi] +5                 #upper asymptote (M)
Y1g<-log(log(alphag1/tssubs1))          #Transform
Reglmg1<-tslm(Y1g~trend)                #Lineal Regression
parmglm1<-as.list(Reglmg1$coeff)

betag1<- exp(parmglm1$"(Intercept)")  #growth range (a)
kg1<- -parmglm1$trend                 #growth rate (b)
fitgom1 <- gompertz(1:10, alphag1, betag1, kg1)
predgom1<-forecast(fitgom1,h=seth)

#Operator 2
alphag2<-tssubs2[hi] +5                 #upper asymptote (M)
Y2g<-log(log(alphag2/tssubs2))          #Transform
Reglmg2<-tslm(Y2g~trend)                #Lineal Regression
parmglm2<-as.list(Reglmg2$coeff)

betag2<- exp(parmglm2$"(Intercept)")  #growth range (a)
kg2<- -parmglm2$trend                 #growth rate (b)
fitgom2 <- gompertz(1:10, alphag2, betag2, kg2)
predgom2<-forecast(fitgom2,h=seth)

#Operator 3
alphag3<-tssubs3[hi] +5                 #upper asymptote (M)
Y3g<-log(log(alphag3/tssubs3))          #Transform
Reglmg3<-tslm(Y3g~trend)                #Lineal Regression
parmglm3<-as.list(Reglmg3$coeff)

betag3<- exp(parmglm3$"(Intercept)")  #growth range (a)
kg3<- -parmglm3$trend                 #growth rate (b)
fitgom3 <- gompertz(1:10, alphag3, betag3, kg3)
predgom3<-forecast(fitgom3,h=seth)

#The predictions using gompertz model are:
set5a<-data.frame(predgom1)
agom<-set5a$Point.Forecast   #Operator 1 predictions
set5b<-data.frame(predgom2)
bgom<-set5b$Point.Forecast   #Operator 2 predictions
set5c<-data.frame(predgom3)
cgom<-set5c$Point.Forecast   #Operator 3 predictions

data_predgom1<-data.frame(Gom1=fitgom1)
data_predgom2<-data.frame(Gom2=fitgom2)
data_predgom3<-data.frame(Gom3=fitgom3)

#Forecasting for 5 years
fore3a<-data.frame(Fore_Log1=alog[1:5],Fore_Gom1=agom[1:5])
fore3b<-data.frame(Fore_Log2=blog[1:5],Fore_Gom1=bgom[1:5])
fore3c<-data.frame(Fore_Log3=clog[1:5],Fore_Gom1=cgom[1:5])


##5.Performance Models for Internet Residential
#Using the forecast package, the performance models are:
#**(ME)**:Mean Error 
#**(RMSE)**:Root Mean Square Error
#**(MAE)**: Mean Absolute Error
#**(MPE)**: Mean Porcentual Error
#**(MAPE)**: Mean Absolute Porcentual Error

# Random Forest Model
acc_a1<-accuracy(predrf1,datares1$Subs, test=NULL)  #Operator 1
acc_a2<-accuracy(predrf2,datares2$Subs, test=NULL)  #Operator 2
acc_a3<-accuracy(predrf3,datares3$Subs, test=NULL)  #Operator 3

#Boosted Generalized Model
acc_b1<-accuracy(predbs1,datares1$Subs, test=NULL) #Operator 1
acc_b2<-accuracy(predbs2,datares2$Subs, test=NULL) #Operator 2
acc_b3<-accuracy(predbs3,datares3$Subs, test=NULL) #Operator 3

#Modelo Lineal
acc_d1<-accuracy(predlm1) #Operator 1
acc_d2<-accuracy(predlm2) #Operator 2
acc_d3<-accuracy(predlm3) #Operator 3

#Modelo Parabólico
acc_e1<-accuracy(predpar1,tsset1)  #Operator 1
acc_e2<-accuracy(predpar2,tsset2)  #Operator 2
acc_e3<-accuracy(predpar3,tsset3)  #Operator 3

#Modelo Exponencial
acc_f1<-accuracy(predexp1)  #Operator 1 
acc_f2<-accuracy(predexp2)  #Operator 2 
acc_f3<-accuracy(predexp3)  #Operator 3 

#Modelo Logístico
acc_g1<-accuracy(predlog1) #Operator 1 
acc_g2<-accuracy(predlog2) #Operator 2 
acc_g3<-accuracy(predlog3) #Operator 3 

#Modelo Gompertz
acc_h1<-accuracy(predgom1) #Operator 1 
acc_h2<-accuracy(predgom2) #Operator 2 
acc_h3<-accuracy(predgom3) #Operator 3 

#The performance sets are:
Models<-c("Random forest","Boosting","Linear","Parabolic","Exponential","Logistic","Gompertz")

#Opereator 1 
acc_all1a<-rbind(acc_a1,acc_b1) #Random forest and Boosted
accd1<-acc_d1[1,1:5]  #Linear
acce1<-acc_e1[1,1:5]  #Parabolic
accf1<-acc_f1[1,1:5]  #Exponential
accg1<-acc_g1[1,1:5]  #Logistic
acch1<-acc_h1[1,1:5]  #Gompertz
acc_all1b<-round(rbind(acc_all1a,accd1,acce1,accf1,accg1,acch1),2)
performIRes1<-data.frame(Models,acc_all1b)

#Operator 2
acc_all2a<-rbind(acc_a2,acc_b2)
accd2<-acc_d2[1,1:5]
acce2<-acc_e2[1,1:5]
accf2<-acc_f2[1,1:5]
accg2<-acc_g2[1,1:5]
acch2<-acc_h2[1,1:5]
acc_all2b<-round(rbind(acc_all2a,accd2,acce2,accf2,accg2,acch2),2)
performIRes2<-data.frame(Models,acc_all2b)

#Operator 3
acc_all3a<-rbind(acc_a3,acc_b3)
accd3<-acc_d3[1,1:5]
acce3<-acc_e3[1,1:5]
accf3<-acc_f3[1,1:5]
accg3<-acc_g3[1,1:5]
acch3<-acc_h3[1,1:5]
acc_all3b<-round(rbind(acc_all3a,accd3,acce3,accf3,accg3,acch3),2)
performIRes3<-data.frame(Models,acc_all3b)


##Predictions Reports
#Internet Residential Predictions by service provider
predIRes1<-data.frame(data_pred1a,data_pred1b,data_predlog1,data_predgom1) #Operator 1 
predIRes2<-data.frame(data_pred2a,data_pred2b,data_predlog2,data_predgom2) #Operator 2 
predIRes3<-data.frame(data_pred3a,data_pred3b,data_predlog3,data_predgom3) #Operator 3 

##Forecast Reports
##Forecasting for 5 Years Internet Residential 
foreIRes1<-data.frame(fore1a,fore3a,fore2a)  #Operator 1
foreIRes2<-data.frame(fore1b,fore3b,fore2b)  #Operator 2
foreIRes3<-data.frame(fore1c,fore3c,fore2c)  #Operator 3

#----------------Demand Models for Internet Bussines Subscribers---------------
##Subset data
#Operator 1
trainbus1<-data.frame(Year=train1$Year,Subs=train1$Internet_Bus1)
testbus1<-data.frame(Year=test1$Year,Subs=test1$Internet_Bus1)
databus1<-data.frame(Year=year,Subs=oper1$Internet_Bus1)
#Operator 2
trainbus2<-data.frame(Year=train2$Year,Subs=train2$Internet_Bus2)
testbus2<-data.frame(Year=test2$Year,Subs=test2$Internet_Bus2)
databus2<-data.frame(Year=year,Subs=oper2$Internet_Bus2)
#Operator 3
trainbus3<-data.frame(Year=train3$Year,Subs=train3$Internet_Bus3)
testbus3<-data.frame(Year=test3$Year,Subs=test3$Internet_Bus3)
databus3<-data.frame(Year=year,Subs=oper3$Internet_Bus3)


##1. Predicting with Random Forest InRes
#Operator 1
modrf1<-train(Subs~.,data=trainbus1,method="rf",prox=TRUE)  #Model
predrf1<- predict(modrf1,databus1)                          #Prediction
forerf1<-forecast(predrf1,h=seth)                           #Forecast

#Operator2
modrf2<-train(Subs~.,data=trainbus2,method="rf",prox=TRUE)  #Model
predrf2<- predict(modrf2, databus2)                         #Prediction
forerf2<-forecast(predrf2,h=seth)                           #Forecast

#Operator 3
modrf3<-train(Subs~.,data=trainbus3,method="rf",prox=TRUE)  #Model
predrf3<- predict(modrf3, databus3)                         #Prediction
forerf3<-forecast(predrf3,h=seth)                           #Forecast


##2. Predicting with Boosting Generalized Additive Model InRes
#Operator 1
modbs1 <- train(Subs ~ ., method = "gamboost", data = trainbus1) 
predbs1 <- predict(modbs1, databus1)
forebs1<-forecast(predbs1,h=seth) 

#Operator 2
modbs2 <- train(Subs ~ ., method = "gamboost", data = trainbus2) 
predbs2 <- predict(modbs2, databus2)
forebs2<-forecast(predbs2,h=seth) 

#Operator 3
modbs3 <- train(Subs ~ ., method = "gamboost", data = trainbus3) 
predbs3 <- predict(modbs3, databus3)
forebs3<-forecast(predbs3,h=seth)

#Predictions
data_pred1a<-data.frame(databus1,Rf1=predrf1,Bs1=predbs1)          
data_pred2a<-data.frame(databus2,Rf2=predrf2,Bs2=predbs2)
data_pred3a<-data.frame(databus3,Rf3=predrf3,Bs3=predbs3) 

##Forecasting for 5 Years

setrf1<-data.frame(forerf1)  #randon forest predictions Op1
rfa<-data.frame(Fore_Rf1=setrf1$Point.Forecast[4:8])
setrf2<-data.frame(forerf2)  #randon forest predictions Op2
rfb<-data.frame(Fore_Rf2=setrf2$Point.Forecast[4:8])
setrf3<-data.frame(forerf3)  #randon forest predictions Op3
rfc<-data.frame(Fore_Rf3=setrf3$Point.Forecast[4:8])

setbs1<-data.frame(forebs1)  #Boosting predictions Op1
bsa<-data.frame(Fore_Bs1=setbs1$Point.Forecast[4:8])
setbs2<-data.frame(forebs2)  #Boosting predictions Op2
bsb<-data.frame(Fore_Bs2=setbs2$Point.Forecast[4:8])
setbs3<-data.frame(forebs3)  #Boosting predictions Op3
bsc<-data.frame(Fore_Bs3=setbs3$Point.Forecast[4:8])

fore1a<-data.frame(Year=f1:f2,rfa,bsa)   #Operator 1
fore1b<-data.frame(Year=f1:f2,rfb,bsb)   #Operator 2
fore1c<-data.frame(Year=f1:f2,rfc,bsc)   #Operator 3


##3. Predicting with Growth curves InBus
#Simple random sampling of time series is probably not the best way to resample 
#times series data. Hyndman and Athanasopoulos (2013)) discuss rolling forecasting 
#origin techniques that move the training and test sets in time .

#Operator1
tssubs1<-ts(oper1$Internet_Bus1,start=start,end=end) #Time serie
tsset1<-tssubs1
tstrain1<-window(tsset1,start=start,end=year1)        #Training and Testing data
tstest1<-window(tsset1,start=year1,end=end)       

#Operator2
tssubs2<-ts(oper2$Internet_Bus2,start=start,end=end) #Time serie
tsset2<-tssubs2/1
tstrain2<-window(tsset2,start=start,end=year1)        #Training and Testing data
tstest2<-window(tsset2,start=year1,end=end)       

#Operator3
tssubs3<-ts(oper3$Internet_Bus3,start=start,end=end) #Time serie
tsset3<-tssubs3/1
tstrain3<-window(tsset3,start=start,end=year1)        #Training and Testing data
tstest3<-window(tsset3,start=year1,end=end)       

##Linear Model
#Operator 1
fitlm1<-tslm(tstrain1~trend)       #Model
predlm1<-forecast(fitlm1, h=seth)  #Prediction
#Operator 2
fitlm2<-tslm(tstrain2~trend)       #Model
predlm2<-forecast(fitlm2, h=seth)  #Prediction
#Operator 3
fitlm3<-tslm(tstrain3~trend)       #Model
predlm3<-forecast(fitlm3, h=seth)  #Prediction

##Parabolic Model
#Operator 1
fitpar1=lm(tsset1 ~ time + I(time^2))  #Model
predpar1<-predict(fitpar1)             #Prediction
forepar1<-forecast(predpar1,h=seth-3)  #Forecast
#Operator 2
fitpar2=lm(tsset2 ~ time + I(time^2))  #Model
predpar2<-predict(fitpar2)             #Prediction
forepar2<-forecast(predpar2,h=seth-3)  #Forecast
#Operator 3
fitpar3=lm(tsset3 ~ time + I(time^2))  #Model 
predpar3<-predict(fitpar3)             #Prediction
forepar3<-forecast(predpar3,h=seth-3)  #Forecast

##Exponential Model
#Operator 1
Yexp1=lm(log(tsset1) ~ time)          #Transform
parmexp1<-as.list(Yexp1$coeff)
betae1<- exp(parmexp1$"(Intercept)")  #growth range (a)
ke1<- parmexp1$time                   #growth rate (b)
fitexp1<-betae1*exp(ke1*time)         #Model
predexp1<-forecast(fitexp1,h=seth-3)

#Operator 2
Yexp2=lm(log(tsset2) ~ time)          #Transform
parmexp2<-as.list(Yexp2$coeff)
betae2<- exp(parmexp2$"(Intercept)")  #growth range (a)
ke2<- parmexp2$time                   #growth rate (b)
fitexp2<-betae2*exp(ke2*time)         #Model
predexp2<-forecast(fitexp2,h=seth-3)

#Operator 3
Yexp3=lm(log(tsset3) ~ time)          #Transform
parmexp3<-as.list(Yexp3$coeff)
betae3<- exp(parmexp3$"(Intercept)")  #growth range (a)
ke3<- parmexp3$time                   #growth rate (b)
fitexp3<-betae3*exp(ke3*time)         #Model
predexp3<-forecast(fitexp3,h=seth-3)


#The predictions with growth curves are:

#Operator 1
set1a<-data.frame(predlm1)  #Linear predictions
lma<-data.frame(Fore_Lin1=set1a$Point.Forecast[4:8])
data_predlm1<-data.frame(Lin1=c(fitlm1$fitted.values,set1a$Point.Forecast[1:3]))
#Operator 2
set2a<-data.frame(predlm2)  #Linear predictions
lmb<-data.frame(Fore_Lin2=set2a$Point.Forecast[4:8])
data_predlm2<-data.frame(Lin2=c(fitlm2$fitted.values,set2a$Point.Forecast[1:3]))
#Operator 3
set3a<-data.frame(predlm3)  #Linear predictions
lmc<-data.frame(Fore_Lin3=set3a$Point.Forecast[4:8])
data_predlm3<-data.frame(Lin3=c(fitlm3$fitted.values,set3a$Point.Forecast[1:3]))


#Operator 1
data_predpar1<-data.frame(Par1=predpar1) #Parabolic predictions
set2a<-data.frame(forepar1)
para<-data.frame(Fore_Par1=set2a$Point.Forecast)
#Operator 2
data_predpar2<-data.frame(Par2=predpar2) #Parabolic predictions
set2b<-data.frame(forepar2)
parb<-data.frame(Fore_Par2=set2b$Point.Forecast)
#Operator 3
data_predpar3<-data.frame(Par3=predpar3) #Parabolic predictions
set2c<-data.frame(forepar3)
parc<-data.frame(Fore_Par3=set2c$Point.Forecast)


#Operator 1
set3a<-data.frame(predexp1)  #Exponential predictions
expa<-data.frame(Fore_Exp1=set3a$Point.Forecast)
data_predexp1<-data.frame(Exp1=fitexp1)
#Operator 2
set3b<-data.frame(predexp2)  #Exponential predictions
expb<-data.frame(Fore_Exp2=set3b$Point.Forecast)
data_predexp2<-data.frame(Exp2=fitexp2)
#Operator 1
set3c<-data.frame(predexp3)  #Exponential predictions
expc<-data.frame(Fore_Exp3=set3c$Point.Forecast)
data_predexp3<-data.frame(Exp3=fitexp3)


##Sets of predictions
data_pred1b<-data.frame(data_predlm1,data_predpar1,data_predexp1)  #Operator 1
data_pred2b<-data.frame(data_predlm2,data_predpar2,data_predexp2)  #Operator 2
data_pred3b<-data.frame(data_predlm3,data_predpar3,data_predexp3)  #Operator 3

##Forecasting for 5 Years
fore2a<-data.frame(Year=f1:f2,lma,para,expa)   #Operator 1
fore2b<-data.frame(lmb,parb,expb)              #Operator 2
fore2c<-data.frame(lmc,parc,expc)              #Operator 3


##4.Predicting with Logistic and Gompertz Model InBus

##Logistic Model
#Operator 1
alpha1<-tssubs1[hi] +0.5           #upper asymptote (M)
Y1<-log(alpha1/tssubs1-1)          #Transform
Reglm1<-tslm(Y1~trend)             #Lineal Regression
parmlm1<-as.list(Reglm1$coeff)

beta1<- exp(parmlm1$"(Intercept)")  #growth range (a)
k1<- -parmlm1$trend                 #growth rate (b)
fitlog1 <- logistic(1:10, alpha1, beta1, k1)
predlog1<-forecast(fitlog1,h=seth)

#Operator 2
alpha2<-tssubs2[hi] +0.5           #upper asymptote (M)
Y2<-log(alpha2/tssubs2-1)          #Transform
Reglm2<-tslm(Y2~trend)             #Lineal Regression
parmlm2<-as.list(Reglm2$coeff)

beta2<- exp(parmlm2$"(Intercept)")  #growth range (a)
k2<- -parmlm2$trend                 #growth rate (b)
fitlog2 <- logistic(1:10, alpha2, beta2, k2)
predlog2<-forecast(fitlog2,h=seth)

#Operator 3
alpha3<-tssubs3[hi] +0.5           #upper asymptote (M)
Y3<-log(alpha3/tssubs3-1)          #Transform
Reglm3<-tslm(Y3~trend)             #Lineal Regression
parmlm3<-as.list(Reglm3$coeff)

beta3<- exp(parmlm3$"(Intercept)")  #growth range (a)
k3<- -parmlm3$trend                 #growth rate (b)
fitlog3 <- logistic(1:10, alpha3, beta3, k3)
predlog3<-forecast(fitlog3,h=seth)

#The predictions using logistic model are:
set4a<-data.frame(predlog1)
alog<-set4a$Point.Forecast   #Operator 1 predictions
set4b<-data.frame(predlog2)
blog<-set4b$Point.Forecast   #Operator 2 predictions
set4c<-data.frame(predlog3)
clog<-set4c$Point.Forecast   #Operator 3 predictions

data_predlog1<-data.frame(Log1=fitlog1)
data_predlog2<-data.frame(Log2=fitlog2)
data_predlog3<-data.frame(Log3=fitlog3)

###Gompertz Model

#Operator 1
alphag1<-tssubs1[hi] +5                 #upper asymptote (M)
Y1g<-log(log(alphag1/tssubs1))          #Transform
Reglmg1<-tslm(Y1g~trend)                #Lineal Regression
parmglm1<-as.list(Reglmg1$coeff)

betag1<- exp(parmglm1$"(Intercept)")  #growth range (a)
kg1<- -parmglm1$trend                 #growth rate (b)
fitgom1 <- gompertz(1:10, alphag1, betag1, kg1)
predgom1<-forecast(fitgom1,h=seth)

#Operator 2
alphag2<-tssubs2[hi] +5                 #upper asymptote (M)
Y2g<-log(log(alphag2/tssubs2))          #Transform
Reglmg2<-tslm(Y2g~trend)                #Lineal Regression
parmglm2<-as.list(Reglmg2$coeff)

betag2<- exp(parmglm2$"(Intercept)")  #growth range (a)
kg2<- -parmglm2$trend                 #growth rate (b)
fitgom2 <- gompertz(1:10, alphag2, betag2, kg2)
predgom2<-forecast(fitgom2,h=seth)

#Operator 3
alphag3<-tssubs3[hi] +5                 #upper asymptote (M)
Y3g<-log(log(alphag3/tssubs3))          #Transform
Reglmg3<-tslm(Y3g~trend)                #Lineal Regression
parmglm3<-as.list(Reglmg3$coeff)

betag3<- exp(parmglm3$"(Intercept)")  #growth range (a)
kg3<- -parmglm3$trend                 #growth rate (b)
fitgom3 <- gompertz(1:10, alphag3, betag3, kg3)
predgom3<-forecast(fitgom3,h=seth)

#The predictions using gompertz model are:
set5a<-data.frame(predgom1)
agom<-set5a$Point.Forecast   #Operator 1 predictions
set5b<-data.frame(predgom2)
bgom<-set5b$Point.Forecast   #Operator 2 predictions
set5c<-data.frame(predgom3)
cgom<-set5c$Point.Forecast   #Operator 3 predictions

data_predgom1<-data.frame(Gom1=fitgom1)
data_predgom2<-data.frame(Gom2=fitgom2)
data_predgom3<-data.frame(Gom3=fitgom3)

#Forecasting for 5 years
fore3a<-data.frame(Fore_Log1=alog[1:5],Fore_Gom1=agom[1:5])
fore3b<-data.frame(Fore_Log2=blog[1:5],Fore_Gom1=bgom[1:5])
fore3c<-data.frame(Fore_Log3=clog[1:5],Fore_Gom1=cgom[1:5])


##5.Performance Models for Internet Bussines
#Using the forecast package, the performance models are:
#**(ME)**:Mean Error 
#**(RMSE)**:Root Mean Square Error
#**(MAE)**: Mean Absolute Error
#**(MPE)**: Mean Porcentual Error
#**(MAPE)**: Mean Absolute Porcentual Error

# Random Forest Model
acc_a1<-accuracy(predrf1,databus1$Subs, test=NULL)  #Operator 1
acc_a2<-accuracy(predrf2,databus2$Subs, test=NULL)  #Operator 2
acc_a3<-accuracy(predrf3,databus3$Subs, test=NULL)  #Operator 3

#Boosted Generalized Model
acc_b1<-accuracy(predbs1,databus1$Subs, test=NULL) #Operator 1
acc_b2<-accuracy(predbs2,databus2$Subs, test=NULL) #Operator 2
acc_b3<-accuracy(predbs3,databus3$Subs, test=NULL) #Operator 3

#Modelo Lineal
acc_d1<-accuracy(predlm1) #Operator 1
acc_d2<-accuracy(predlm2) #Operator 2
acc_d3<-accuracy(predlm3) #Operator 3

#Modelo Parabólico
acc_e1<-accuracy(predpar1,tsset1)  #Operator 1
acc_e2<-accuracy(predpar2,tsset2)  #Operator 2
acc_e3<-accuracy(predpar3,tsset3)  #Operator 3

#Modelo Exponencial
acc_f1<-accuracy(predexp1)  #Operator 1 
acc_f2<-accuracy(predexp2)  #Operator 2 
acc_f3<-accuracy(predexp3)  #Operator 3 

#Modelo Logístico
acc_g1<-accuracy(predlog1) #Operator 1 
acc_g2<-accuracy(predlog2) #Operator 2 
acc_g3<-accuracy(predlog3) #Operator 3 

#Modelo Gompertz
acc_h1<-accuracy(predgom1) #Operator 1 
acc_h2<-accuracy(predgom2) #Operator 2 
acc_h3<-accuracy(predgom3) #Operator 3 

#The performance sets are:
Models<-c("Random forest","Boosting","Linear","Parabolic","Exponential","Logistic","Gompertz")

#Opereator 1 
acc_all1a<-rbind(acc_a1,acc_b1) #Random forest and Boosted
accd1<-acc_d1[1,1:5]  #Linear
acce1<-acc_e1[1,1:5]  #Parabolic
accf1<-acc_f1[1,1:5]  #Exponential
accg1<-acc_g1[1,1:5]  #Logistic
acch1<-acc_h1[1,1:5]  #Gompertz
acc_all1b<-round(rbind(acc_all1a,accd1,acce1,accf1,accg1,acch1),2)
performIBus1<-data.frame(Models,acc_all1b)

#Operator 2
acc_all2a<-rbind(acc_a2,acc_b2)
accd2<-acc_d2[1,1:5]
acce2<-acc_e2[1,1:5]
accf2<-acc_f2[1,1:5]
accg2<-acc_g2[1,1:5]
acch2<-acc_h2[1,1:5]
acc_all2b<-round(rbind(acc_all2a,accd2,acce2,accf2,accg2,acch2),2)
performIBus2<-data.frame(Models,acc_all2b)

#Operator 3
acc_all3a<-rbind(acc_a3,acc_b3)
accd3<-acc_d3[1,1:5]
acce3<-acc_e3[1,1:5]
accf3<-acc_f3[1,1:5]
accg3<-acc_g3[1,1:5]
acch3<-acc_h3[1,1:5]
acc_all3b<-round(rbind(acc_all3a,accd3,acce3,accf3,accg3,acch3),2)
performIBus3<-data.frame(Models,acc_all3b)


##Predictions Reports
#Internet Bussines Predictions by service provider
predIBus1<-data.frame(data_pred1a,data_pred1b,data_predlog1,data_predgom1) #Operator 1 
predIBus2<-data.frame(data_pred2a,data_pred2b,data_predlog2,data_predgom2) #Operator 2 
predIBus3<-data.frame(data_pred3a,data_pred3b,data_predlog3,data_predgom3) #Operator 3 

##Forecast Reports
##Forecasting for 5 Years Internet Residential 
foreIBus1<-data.frame(fore1a,fore3a,fore2a)  #Operator 1
foreIBus2<-data.frame(fore1b,fore3b,fore2b)  #Operator 2
foreIBus3<-data.frame(fore1c,fore3c,fore2c)  #Operator 3

#----------------Demand Models for VoD Subscribers-------------------
##Subset data
dataVoD1<-data.frame(Year=year,Subs=oper1$VoD1)  #Operator 1
tssubs1<-ts(oper1$VoD1,start=start,end=end) #Time serie
dataVoD2<-data.frame(Year=year,Subs=oper2$VoD2)  #Operator 2
tssubs2<-ts(oper2$VoD2,start=start,end=end) #Time serie
dataVoD3<-data.frame(Year=year,Subs=oper3$VoD3)  #Operator 3
tssubs3<-ts(oper3$VoD3,start=start,end=end) #Time serie


##4.Predicting with Logistic and Gompertz Model VoD

##Logistic Model
#Operator 1
alpha1<-tssubs1[hi] +0.5           #upper asymptote (M)
Y1<-log(alpha1/tssubs1-1)          #Transform
Reglm1<-tslm(Y1~trend)             #Lineal Regression
parmlm1<-as.list(Reglm1$coeff)

beta1<- exp(parmlm1$"(Intercept)")  #growth range (a)
k1<- -parmlm1$trend                 #growth rate (b)
fitlog1 <- logistic(1:10, alpha1, beta1, k1)
predlog1<-forecast(fitlog1,h=seth)

#Operator 2
alpha2<-tssubs2[hi] +0.5           #upper asymptote (M)
Y2<-log(alpha2/tssubs2-1)          #Transform
Reglm2<-tslm(Y2~trend)             #Lineal Regression
parmlm2<-as.list(Reglm2$coeff)

beta2<- exp(parmlm2$"(Intercept)")  #growth range (a)
k2<- -parmlm2$trend                 #growth rate (b)
fitlog2 <- logistic(1:10, alpha2, beta2, k2)
predlog2<-forecast(fitlog2,h=seth)

#Operator 3
alpha3<-tssubs3[hi] +0.5           #upper asymptote (M)
Y3<-log(alpha3/tssubs3-1)          #Transform
Reglm3<-tslm(Y3~trend)             #Lineal Regression
parmlm3<-as.list(Reglm3$coeff)

beta3<- exp(parmlm3$"(Intercept)")  #growth range (a)
k3<- -parmlm3$trend                 #growth rate (b)
fitlog3 <- logistic(1:10, alpha3, beta3, k3)
predlog3<-forecast(fitlog3,h=seth)

#The predictions using logistic model are:
set4a<-data.frame(predlog1)
alog<-set4a$Point.Forecast   #Operator 1 predictions
set4b<-data.frame(predlog2)
blog<-set4b$Point.Forecast   #Operator 2 predictions
set4c<-data.frame(predlog3)
clog<-set4c$Point.Forecast   #Operator 3 predictions

data_predlog1<-data.frame(Log1=fitlog1)
data_predlog2<-data.frame(Log2=fitlog2)
data_predlog3<-data.frame(Log3=fitlog3)

###Gompertz Model
#Using the **growthmodels** package, Computes the Gompertz growth model
#**Usage**
#gompertz(t, alpha, beta, k)
#**Arguments**
#t     time
#x     size
#alpha upper asymptote
#beta  growth displacement
#k     growth rate

#Operator 1
alphag1<-tssubs1[hi] +5                 #upper asymptote (M)
Y1g<-log(log(alphag1/tssubs1))          #Transform
Reglmg1<-tslm(Y1g~trend)                #Lineal Regression
parmglm1<-as.list(Reglmg1$coeff)

betag1<- exp(parmglm1$"(Intercept)")  #growth range (a)
kg1<- -parmglm1$trend                 #growth rate (b)
fitgom1 <- gompertz(1:10, alphag1, betag1, kg1)
predgom1<-forecast(fitgom1,h=seth)

#Operator 2
alphag2<-tssubs2[hi] +5                 #upper asymptote (M)
Y2g<-log(log(alphag2/tssubs2))          #Transform
Reglmg2<-tslm(Y2g~trend)                #Lineal Regression
parmglm2<-as.list(Reglmg2$coeff)

betag2<- exp(parmglm2$"(Intercept)")  #growth range (a)
kg2<- -parmglm2$trend                 #growth rate (b)
fitgom2 <- gompertz(1:10, alphag2, betag2, kg2)
predgom2<-forecast(fitgom2,h=seth)

#Operator 3
alphag3<-tssubs3[hi] +5                 #upper asymptote (M)
Y3g<-log(log(alphag3/tssubs3))          #Transform
Reglmg3<-tslm(Y3g~trend)                #Lineal Regression
parmglm3<-as.list(Reglmg3$coeff)

betag3<- exp(parmglm3$"(Intercept)")  #growth range (a)
kg3<- -parmglm3$trend                 #growth rate (b)
fitgom3 <- gompertz(1:10, alphag3, betag3, kg3)
predgom3<-forecast(fitgom3,h=seth)

#The predictions using gompertz model are:
set5a<-data.frame(predgom1)
agom<-set5a$Point.Forecast   #Operator 1 predictions
set5b<-data.frame(predgom2)
bgom<-set5b$Point.Forecast   #Operator 2 predictions
set5c<-data.frame(predgom3)
cgom<-set5c$Point.Forecast   #Operator 3 predictions

data_predgom1<-data.frame(Gom1=fitgom1)
data_predgom2<-data.frame(Gom2=fitgom2)
data_predgom3<-data.frame(Gom3=fitgom3)

#Forecasting for 5 years
fore3a<-data.frame(Fore_Log1=alog[1:5],Fore_Gom1=agom[1:5])
fore3b<-data.frame(Fore_Log2=blog[1:5],Fore_Gom2=bgom[1:5])
fore3c<-data.frame(Fore_Log3=clog[1:5],Fore_Gom3=cgom[1:5])


##11.Predicting with Fisher Pry
#Model applied When substitution is driven by superior technology. 
#The new product or service presents some technological advantage over the old one.

#Operator 1
L1<-tssubs1[hi]+5                    #upper asymptote (L)
transf1<-log((L1-tssubs1)/tssubs1)   #transform

fitpry1<-tslm(transf1~trend)        #model
set5a<-fitpry1$fitted.values
predpry1<-L1/(1+exp(set5a))         #Prediction
forepry1<-forecast(predpry1,h=seth) #Forecast

#Operator 2
L2<-tssubs2[hi]+5                    #upper asymptote (L)
transf2<-log((L2-tssubs2)/tssubs2)   #transform

fitpry2<-tslm(transf2~trend)        #model
set5b<-fitpry2$fitted.values
predpry2<-L2/(1+exp(set5b))         #Prediction
forepry2<-forecast(predpry2,h=seth) #Forecast

#Operator 3
L3<-tssubs3[hi]+5                    #upper asymptote (L)
transf3<-log((L3-tssubs3)/tssubs3)   #transform

fitpry3<-tslm(transf3~trend)        #model
set5c<-fitpry3$fitted.values
predpry3<-L3/(1+exp(set5c))         #Prediction
forepry3<-forecast(predpry3,h=seth) #Forecast

##12. Bass Model                                              
#**m** Total number of potential buyers of the new product
#**p** The coefficient of innovation
#**q** The coefficient of imitation

#Operator1
setbas1<-subset(dataVoD1,Subs>=0.101)
setbas2<-subset(dataVoD1,Subs<0.101)
demand1<-tssubs1[(tssubs1)>=0.101]
time<-1:length(demand1)
Tdelt <- time     #Accuracy, size predictions

Bass.nls <- nls(demand1 ~ M * (((P + Q)^2/P) * exp(-(P + Q) * time))/(1 + (Q/P) 
            *exp(-(P + Q) * time))^2, start = list(M = 60630, P = 0.03, Q = 0.38))
Bcoef <- coef(Bass.nls) # get coefficient operator 1
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)  #Setting the starting values for M to the Subs

fitbas1 <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2   #Model
setbas3<-data.frame(Year=setbas1$Year,Subs=fitbas1)          #Prediction
x<-nrow(setbas2)
cero<-data.frame(Year=setbas2$Year,Subs=c(1:x*0))
predbas1<-rbind(cero,setbas3)
forebas1<-forecast(predbas1$Subs,h=seth)                     #Forecast

#Operator2
setbas1<-subset(dataVoD2,Subs>=0.101)
setbas2<-subset(dataVoD2,Subs<0.101)
demand2<-tssubs2[(tssubs2)>=0.101]
time<-1:length(demand2)
Tdelt <- time     #Accuracy, size predictions

Bass.nls <- nls(demand2 ~ M * (((P + Q)^2/P) * exp(-(P + Q) * time))/(1 + (Q/P) 
                                                                      *exp(-(P + Q) * time))^2, start = list(M = 60630, P = 0.03, Q = 0.38))
Bcoef <- coef(Bass.nls) # get coefficient operator 1
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)  #Setting the starting values for M to the Subs

fitbas2 <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2   #Model
setbas3<-data.frame(Year=setbas1$Year,Subs=fitbas2)          #Prediction
x<-nrow(setbas2)
cero<-data.frame(Year=setbas2$Year,Subs=c(1:x*0))
predbas2<-rbind(cero,setbas3)
forebas2<-forecast(predbas2$Subs,h=seth)                     #Forecast

#Operator3
setbas1<-subset(dataVoD3,Subs>=0.101)
setbas2<-subset(dataVoD3,Subs<0.101)
demand3<-tssubs3[(tssubs3)>=0.101]
time<-1:length(demand3)
Tdelt <- time     #Accuracy, size predictions

Bass.nls <- nls(demand3 ~ M * (((P + Q)^2/P) * exp(-(P + Q) * time))/(1 + (Q/P) 
                                                                      *exp(-(P + Q) * time))^2, start = list(M = 60630, P = 0.03, Q = 0.38))
Bcoef <- coef(Bass.nls) # get coefficient operator 1
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)  #Setting the starting values for M to the Subs

fitbas3 <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2   #Model
setbas3<-data.frame(Year=setbas1$Year,Subs=fitbas3)             #Prediction
x<-nrow(setbas2)
cero<-data.frame(Year=setbas2$Year,Subs=c(1:x*0))
predbas3<-rbind(cero,setbas3)
forebas3<-forecast(predbas3$Subs,h=seth)                     #Forecast


#The predictions using Bass and Fisher Pry model are:
set6a<-data.frame(forepry1)
apry<-set6a$Point.Forecast   #Operator 1 predictions
set6b<-data.frame(forepry2)
bpry<-set6b$Point.Forecast   #Operator 2 predictions
set6c<-data.frame(forepry3)
cpry<-set6c$Point.Forecast   #Operator 3 predictions

set7a<-data.frame(forebas1)
abas<-set7a$Point.Forecast   #Operator 1 predictions
set7b<-data.frame(forebas2)
bbas<-set7b$Point.Forecast   #Operator 2 predictions
set7c<-data.frame(forebas3)
cbas<-set7c$Point.Forecast   #Operator 3 predictions

data_pred1c<-data.frame(Pry1=predpry1,Bass1=predbas1$Subs)
data_pred2c<-data.frame(Pry2=predpry2,Bass2=predbas2$Subs)
data_pred3c<-data.frame(Pry3=predpry3,Bass3=predbas3$Subs)

#Forecasting for 5 years
fore4a<-data.frame(Fore_Pry1=apry[1:5],Fore_Bas1=abas[1:5])
fore4b<-data.frame(Fore_Pry2=bpry[1:5],Fore_Bas2=bbas[1:5])
fore4c<-data.frame(Fore_Pry3=cpry[1:5],Fore_Bas3=cbas[1:5])


##13.Performance Models for VoD
#Using the forecast package, the performance models are:
#**(ME)**:Mean Error 
#**(RMSE)**:Root Mean Square Error
#**(MAE)**: Mean Absolute Error
#**(MPE)**: Mean Porcentual Error
#**(MAPE)**: Mean Absolute Porcentual Error

#Modelo Logístico
acc_g1<-accuracy(predlog1) #Operator 1 
acc_g2<-accuracy(predlog2) #Operator 2 
acc_g3<-accuracy(predlog3) #Operator 3 

#Modelo Gompertz
acc_h1<-accuracy(predgom1) #Operator 1 
acc_h2<-accuracy(predgom2) #Operator 2 
acc_h3<-accuracy(predgom3) #Operator 3 

#Modelo Fisher-Pry
acc_i1<-accuracy(predpry1,tsset1) #Operator 1 
acc_i2<-accuracy(predpry2,tsset2) #Operator 2
acc_i3<-accuracy(predpry3,tsset3) #Operator 3

#Modelo Bass
acc_j1<-accuracy(predbas1$Subs,dataVoD1$Subs)   #Duda Operator 1
acc_j2<-accuracy(predbas2$Subs,dataVoD2$Subs)   #Duda Operator 2
acc_j3<-accuracy(predbas3$Subs,dataVoD3$Subs)   #Duda Operator 3

#The performance sets are:
Models<-c("Logistic","Gompertz","Fisher-Pry","Bass")

#Opereator 1
accg1<-acc_g1[1,1:5]  #Logistic
acch1<-acc_h1[1,1:5]  #Gompertz
acci1<-acc_i1[1,1:5]  #Fisher-Pry
accj1<-acc_j1[1,1:5]  #Bass
acc_all1b<-round(rbind(accg1,acch1,acci1,accj1),2)
performVoD1<-data.frame(Models,acc_all1b)

#Operator 2
accg2<-acc_g2[1,1:5]
acch2<-acc_h2[1,1:5]
acci2<-acc_i2[1,1:5]
accj2<-acc_j2[1,1:5]
acc_all2b<-round(rbind(accg2,acch2,acci2,accj2),2)
performVoD2<-data.frame(Models,acc_all2b)

#Operator 3
accg3<-acc_g3[1,1:5]
acch3<-acc_h3[1,1:5]
acci3<-acc_i3[1,1:5]
accj3<-acc_j3[1,1:5]
acc_all3b<-round(rbind(accg3,acch3,acci3,accj3),2)
performVoD3<-data.frame(Models,acc_all3b)


##Predictions Reports
#Internet Residential Predictions by service provider
predVoD1<-data.frame(data_predlog1,data_predgom1,data_pred1c) #Operator 1 
predVoD2<-data.frame(data_predlog2,data_predgom2,data_pred2c) #Operator 2 
predVoD3<-data.frame(data_predlog3,data_predgom3,data_pred3c) #Operator 3 

##Forecast Reports
##Forecasting for 5 Years Video on Demand 
foreVoD1<-data.frame(fore3a,fore4a)  #Operator 1
foreVoD2<-data.frame(fore3b,fore4b)  #Operator 2
foreVoD3<-data.frame(fore3c,fore4c)  #Operator 3

#----------------Demand Models for VoIP Subscribers------------------
##Subset data
dataVoIP1<-data.frame(Year=year,Subs=oper1$VoIP1)  #Operator 1
tssubs1<-ts(oper1$VoIP1,start=start,end=end) #Time serie
dataVoIP2<-data.frame(Year=year,Subs=oper2$VoIP2)  #Operator 2
tssubs2<-ts(oper2$VoIP2,start=start,end=end) #Time serie
dataVoIP3<-data.frame(Year=year,Subs=oper3$VoIP3)  #Operator 3
tssubs3<-ts(oper3$VoIP3,start=start,end=end) #Time serie


##4.Predicting with Logistic and Gompertz Model VoIP

##Logistic Model
#Operator 1
alpha1<-tssubs1[hi] +0.5           #upper asymptote (M)
Y1<-log(alpha1/tssubs1-1)          #Transform
Reglm1<-tslm(Y1~trend)             #Lineal Regression
parmlm1<-as.list(Reglm1$coeff)

beta1<- exp(parmlm1$"(Intercept)")  #growth range (a)
k1<- -parmlm1$trend                 #growth rate (b)
fitlog1 <- logistic(1:10, alpha1, beta1, k1)
predlog1<-forecast(fitlog1,h=seth)

#Operator 2
alpha2<-tssubs2[hi] +0.5           #upper asymptote (M)
Y2<-log(alpha2/tssubs2-1)          #Transform
Reglm2<-tslm(Y2~trend)             #Lineal Regression
parmlm2<-as.list(Reglm2$coeff)

beta2<- exp(parmlm2$"(Intercept)")  #growth range (a)
k2<- -parmlm2$trend                 #growth rate (b)
fitlog2 <- logistic(1:10, alpha2, beta2, k2)
predlog2<-forecast(fitlog2,h=seth)

#Operator 3
alpha3<-tssubs3[hi] +0.5           #upper asymptote (M)
Y3<-log(alpha3/tssubs3-1)          #Transform
Reglm3<-tslm(Y3~trend)             #Lineal Regression
parmlm3<-as.list(Reglm3$coeff)

beta3<- exp(parmlm3$"(Intercept)")  #growth range (a)
k3<- -parmlm3$trend                 #growth rate (b)
fitlog3 <- logistic(1:10, alpha3, beta3, k3)
predlog3<-forecast(fitlog3,h=seth)

#The predictions using logistic model are:
set4a<-data.frame(predlog1)
alog<-set4a$Point.Forecast   #Operator 1 predictions
set4b<-data.frame(predlog2)
blog<-set4b$Point.Forecast   #Operator 2 predictions
set4c<-data.frame(predlog3)
clog<-set4c$Point.Forecast   #Operator 3 predictions

data_predlog1<-data.frame(Log1=fitlog1)
data_predlog2<-data.frame(Log2=fitlog2)
data_predlog3<-data.frame(Log3=fitlog3)

###Gompertz Model

#Operator 1
alphag1<-tssubs1[hi] +5                 #upper asymptote (M)
Y1g<-log(log(alphag1/tssubs1))          #Transform
Reglmg1<-tslm(Y1g~trend)                #Lineal Regression
parmglm1<-as.list(Reglmg1$coeff)

betag1<- exp(parmglm1$"(Intercept)")  #growth range (a)
kg1<- -parmglm1$trend                 #growth rate (b)
fitgom1 <- gompertz(1:10, alphag1, betag1, kg1)
predgom1<-forecast(fitgom1,h=seth)

#Operator 2
alphag2<-tssubs2[hi] +5                 #upper asymptote (M)
Y2g<-log(log(alphag2/tssubs2))          #Transform
Reglmg2<-tslm(Y2g~trend)                #Lineal Regression
parmglm2<-as.list(Reglmg2$coeff)

betag2<- exp(parmglm2$"(Intercept)")  #growth range (a)
kg2<- -parmglm2$trend                 #growth rate (b)
fitgom2 <- gompertz(1:10, alphag2, betag2, kg2)
predgom2<-forecast(fitgom2,h=seth)

#Operator 3
alphag3<-tssubs3[hi] +5                 #upper asymptote (M)
Y3g<-log(log(alphag3/tssubs3))          #Transform
Reglmg3<-tslm(Y3g~trend)                #Lineal Regression
parmglm3<-as.list(Reglmg3$coeff)

betag3<- exp(parmglm3$"(Intercept)")  #growth range (a)
kg3<- -parmglm3$trend                 #growth rate (b)
fitgom3 <- gompertz(1:10, alphag3, betag3, kg3)
predgom3<-forecast(fitgom3,h=seth)

#The predictions using gompertz model are:
set5a<-data.frame(predgom1)
agom<-set5a$Point.Forecast   #Operator 1 predictions
set5b<-data.frame(predgom2)
bgom<-set5b$Point.Forecast   #Operator 2 predictions
set5c<-data.frame(predgom3)
cgom<-set5c$Point.Forecast   #Operator 3 predictions

data_predgom1<-data.frame(Gom1=fitgom1)
data_predgom2<-data.frame(Gom2=fitgom2)
data_predgom3<-data.frame(Gom3=fitgom3)

#Forecasting for 5 years
fore3a<-data.frame(Fore_Log1=alog[1:5],Fore_Gom1=agom[1:5])
fore3b<-data.frame(Fore_Log2=blog[1:5],Fore_Gom2=bgom[1:5])
fore3c<-data.frame(Fore_Log3=clog[1:5],Fore_Gom3=cgom[1:5])


##11.Predicting with Fisher Pry
#Model applied When substitution is driven by superior technology. 
#The new product or service presents some technological advantage over the old one.

#Operator 1
L1<-tssubs1[hi]+5                    #upper asymptote (L)
transf1<-log((L1-tssubs1)/tssubs1)   #transform

fitpry1<-tslm(transf1~trend)        #model
set5a<-fitpry1$fitted.values
predpry1<-L1/(1+exp(set5a))         #Prediction
forepry1<-forecast(predpry1,h=seth) #Forecast

#Operator 2
L2<-tssubs2[hi]+5                    #upper asymptote (L)
transf2<-log((L2-tssubs2)/tssubs2)   #transform

fitpry2<-tslm(transf2~trend)        #model
set5b<-fitpry2$fitted.values
predpry2<-L2/(1+exp(set5b))         #Prediction
forepry2<-forecast(predpry2,h=seth) #Forecast

#Operator 3
L3<-tssubs3[hi]+5                    #upper asymptote (L)
transf3<-log((L3-tssubs3)/tssubs3)   #transform

fitpry3<-tslm(transf3~trend)        #model
set5c<-fitpry3$fitted.values
predpry3<-L3/(1+exp(set5c))         #Prediction
forepry3<-forecast(predpry3,h=seth) #Forecast

##12. Bass Model                                              
#**m** Total number of potential buyers of the new product
#**p** The coefficient of innovation
#**q** The coefficient of imitation

#Operator1
setbas1<-subset(dataVoIP1,Subs>=0.11)
setbas2<-subset(dataVoIP1,Subs<0.11)
demand1<-tssubs1[(tssubs1)>=0.11]
time<-1:length(demand1)
Tdelt <- time     #Accuracy, size predictions

Bass.nls <- nls(demand1 ~ M * (((P + Q)^2/P) * exp(-(P + Q) * time))/(1 + (Q/P) 
                                                                      *exp(-(P + Q) * time))^2, start = list(M = 60630, P = 0.03, Q = 0.38))
Bcoef <- coef(Bass.nls) # get coefficient operator 1
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)  #Setting the starting values for M to the Subs

fitbas1 <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2   #Model
setbas3<-data.frame(Year=setbas1$Year,Subs=fitbas1)             #Prediction
x<-nrow(setbas2)
cero<-data.frame(Year=setbas2$Year,Subs=c(1:x*0))
predbas1<-rbind(cero,setbas3)
forebas1<-forecast(predbas1$Subs,h=seth)                     #Forecast

#Operator2
setbas1<-subset(dataVoIP2,Subs>=0.11)
setbas2<-subset(dataVoIP2,Subs<0.11)
demand2<-tssubs2[(tssubs2)>=0.11]
time<-1:length(demand2)
Tdelt <- time     #Accuracy, size predictions

Bass.nls <- nls(demand2 ~ M * (((P + Q)^2/P) * exp(-(P + Q) * time))/(1 + (Q/P) 
                                                                      *exp(-(P + Q) * time))^2, start = list(M = 60630, P = 0.03, Q = 0.38))
Bcoef <- coef(Bass.nls) # get coefficient operator 1
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)  #Setting the starting values for M to the Subs

fitbas2 <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2   #Model
setbas3<-data.frame(Year=setbas1$Year,Subs=fitbas2)             #Prediction
x<-nrow(setbas2)
cero<-data.frame(Year=setbas2$Year,Subs=c(1:x*0))
predbas2<-rbind(cero,setbas3)
forebas2<-forecast(predbas2$Subs,h=seth)                     #Forecast

#Operator3
setbas1<-subset(dataVoIP3,Subs>=0.11)
setbas2<-subset(dataVoIP3,Subs<0.11)
demand3<-tssubs3[(tssubs3)>=0.11]
time<-1:length(demand3)
Tdelt <- time     #Accuracy, size predictions

Bass.nls <- nls(demand3 ~ M * (((P + Q)^2/P) * exp(-(P + Q) * time))/(1 + (Q/P) 
                                                                      *exp(-(P + Q) * time))^2, start = list(M = 60630, P = 0.03, Q = 0.38))
Bcoef <- coef(Bass.nls) # get coefficient operator 1
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)  #Setting the starting values for M to the Subs

fitbas3 <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2   #Model
setbas3<-data.frame(Year=setbas1$Year,Subs=fitbas3)             #Prediction
x<-nrow(setbas2)
cero<-data.frame(Year=setbas2$Year,Subs=c(1:x*0))
predbas3<-rbind(cero,setbas3)
forebas3<-forecast(predbas3$Subs,h=seth)                     #Forecast


#The predictions using Bass and Fisher Pry model are:
set6a<-data.frame(forepry1)
apry<-set6a$Point.Forecast   #Operator 1 predictions
set6b<-data.frame(forepry2)
bpry<-set6b$Point.Forecast   #Operator 2 predictions
set6c<-data.frame(forepry3)
cpry<-set6c$Point.Forecast   #Operator 3 predictions

set7a<-data.frame(forebas1)
abas<-set7a$Point.Forecast   #Operator 1 predictions
set7b<-data.frame(forebas2)
bbas<-set7b$Point.Forecast   #Operator 2 predictions
set7c<-data.frame(forebas3)
cbas<-set7c$Point.Forecast   #Operator 3 predictions

data_pred1c<-data.frame(Pry1=predpry1,Bass1=predbas1$Subs)
data_pred2c<-data.frame(Pry2=predpry2,Bass2=predbas2$Subs)
data_pred3c<-data.frame(Pry3=predpry3,Bass3=predbas3$Subs)

#Forecasting for 5 years
fore4a<-data.frame(Fore_Pry1=apry[1:5],Fore_Bas1=abas[1:5])
fore4b<-data.frame(Fore_Pry2=bpry[1:5],Fore_Bas2=bbas[1:5])
fore4c<-data.frame(Fore_Pry3=cpry[1:5],Fore_Bas3=cbas[1:5])


##13.Performance Models for VoIP
#Using the forecast package, the performance models are:
#**(ME)**:Mean Error 
#**(RMSE)**:Root Mean Square Error
#**(MAE)**: Mean Absolute Error
#**(MPE)**: Mean Porcentual Error
#**(MAPE)**: Mean Absolute Porcentual Error

#Modelo Logístico
acc_g1<-accuracy(predlog1) #Operator 1 
acc_g2<-accuracy(predlog2) #Operator 2 
acc_g3<-accuracy(predlog3) #Operator 3 

#Modelo Gompertz
acc_h1<-accuracy(predgom1) #Operator 1 
acc_h2<-accuracy(predgom2) #Operator 2 
acc_h3<-accuracy(predgom3) #Operator 3 

#Modelo Fisher-Pry
acc_i1<-accuracy(predpry1,tsset1) #Operator 1 
acc_i2<-accuracy(predpry2,tsset2) #Operator 2
acc_i3<-accuracy(predpry3,tsset3) #Operator 3

#Modelo Bass
acc_j1<-accuracy(predbas1$Subs,dataVoD1$Subs)   #Duda Operator 1
acc_j2<-accuracy(predbas2$Subs,dataVoD2$Subs)   #Duda Operator 2
acc_j3<-accuracy(predbas3$Subs,dataVoD3$Subs)   #Duda Operator 3

#The performance sets are:
Models<-c("Logistic","Gompertz","Fisher-Pry","Bass")

#Opereator 1
accg1<-acc_g1[1,1:5]  #Logistic
acch1<-acc_h1[1,1:5]  #Gompertz
acci1<-acc_i1[1,1:5]  #Fisher-Pry
accj1<-acc_j1[1,1:5]  #Bass
acc_all1b<-round(rbind(accg1,acch1,acci1,accj1),2)
performVoIP1<-data.frame(Models,acc_all1b)

#Operator 2
accg2<-acc_g2[1,1:5]
acch2<-acc_h2[1,1:5]
acci2<-acc_i2[1,1:5]
accj2<-acc_j2[1,1:5]
acc_all2b<-round(rbind(accg2,acch2,acci2,accj2),2)
performVoIP2<-data.frame(Models,acc_all2b)

#Operator 3
accg3<-acc_g3[1,1:5]
acch3<-acc_h3[1,1:5]
acci3<-acc_i3[1,1:5]
accj3<-acc_j3[1,1:5]
acc_all3b<-round(rbind(accg3,acch3,acci3,accj3),2)
performVoIP3<-data.frame(Models,acc_all3b)


##Predictions Reports
#Internet Residential Predictions by service provider
predVoIP1<-data.frame(data_predlog1,data_predgom1,data_pred1c) #Operator 1 
predVoIP2<-data.frame(data_predlog2,data_predgom2,data_pred2c) #Operator 2 
predVoIP3<-data.frame(data_predlog3,data_predgom3,data_pred3c) #Operator 3 

##Forecast Reports
##Forecasting for 5 Years Video on Demand 
foreVoIP1<-data.frame(fore3a,fore4a)  #Operator 1
foreVoIP2<-data.frame(fore3b,fore4b)  #Operator 2
foreVoIP3<-data.frame(fore3c,fore4c)  #Operator 3

#----------------Predictions, Forecast and Performance Reports-----------------------------------------

##Predictions Report
predIRes<-round(data.frame(predIRes1,predIRes2,predIRes3),2)
predIBus<-round(data.frame(predIBus1,predIBus2,predIBus3),2)
predVoD<-round(data.frame(predVoD1,predVoD2,predVoD3),2)
predVoIP<-round(data.frame(predVoIP1,predVoIP2,predVoIP3),2)

predict<-data.frame(predIRes,predIBus,predVoD,predVoIP)
write.csv2(predict,"Predictions_Report.csv")

##Forecast Report
foreIRes<-round(data.frame(foreIRes1,foreIRes2,foreIRes3),2)
foreIBus<-round(data.frame(foreIBus1,foreIBus2,foreIBus3),2)
foreVoD<-round(data.frame(foreVoD1,foreVoD2,foreVoD3),2)
foreVoIP<-round(data.frame(foreVoIP1,foreVoIP2,foreVoIP3),2)

forecast<-data.frame(foreIRes,foreIBus,foreVoD,foreVoIP)
write.csv2(forecast,"Forecasting_Report.csv")

##Performance Report
performIRes<-data.frame(performIRes1,performIRes2[2:6],performIRes3[2:6]) #Internet Residential
performIBus<-data.frame(performIBus1,performIBus2[2:6],performIBus3[2:6]) #Internet Bussines
performVoD<-data.frame(performVoD1,performVoD2[2:6],performVoD3[2:6])     #Video on Demand
performVoIP<-data.frame(performVoIP1,performVoIP2[2:6],performVoIP3[2:6]) #Voice over IP

perform<-data.frame(performIRes,performIBus,performVoD,performVoIP)
write.csv2(performall,"Performance_Report.csv")

