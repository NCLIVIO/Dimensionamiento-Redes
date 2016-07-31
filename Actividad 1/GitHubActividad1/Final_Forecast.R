# FORECASTING DEMAND 
#By: Natalia Clivio
#nclivio@gmail.com
#
#---------------------------Load and clean data---------------------------------
#####
#The file loaded is the telecommunication companies´s subscribers by ten years,
#for each service provided by this companies. The service offering are:
#Internet residential and business, Video on demand and VoIP.

library(caret)
library(forecast)
library(growthmodels)

subs<-read.csv("C:/Users/NataliaA/Documents/DataR/Subscribers_CV.csv",header=TRUE,
               sep=";",na.strings="NA",dec=",")

years<-na.exclude(subs[1])
oper1<-na.exclude(subs[1:5]) #Subs operador 1
oper2<-data.frame(years,na.exclude(subs[6:9])) #Subs operador 2
oper3<-data.frame(years,na.exclude(subs[10:13])) #Subs operador 3
#####
#---------------------------Data Preparation------------------------------------

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

##1.1 Predicting with Growth curves InRes
#########
#Simple random sampling of time series is probably not the best way to resample 
#times series data. Hyndman and Athanasopoulos (2013)) discuss rolling forecasting 
#origin techniques that move the training and test sets in time.

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
#########

