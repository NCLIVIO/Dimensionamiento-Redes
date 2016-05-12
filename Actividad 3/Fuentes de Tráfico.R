#---
#title: "Estimación estadística del Ancho de Banda Efectivo"
#author: "Natalia Clivio"
#date: '2015'
#---

#Caracterización fuente de datos
#Incoming=Downstream

traza1<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/5039556D06A0.csv",
               header=TRUE,sep=",",na.strings="NA",dec=".")
traza2<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0011AEA17B96.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza3<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001BD70A67A8.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza4<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/386BBBDE3EF4.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza5<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/7CBFB1B96514.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza6<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0011E659D0FD.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza7<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/34BDFA8C66F3.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza8<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0023BED75BEA.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza9<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001BD7A7BDE4.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")

traza10<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001BD7BBF6CA.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza11<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/00159A6160B0.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza12<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/CC7D37981A49.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza13<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0014E8295DC2.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza14<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001ADE631B8A.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza15<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0023BED18526.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza16<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/145BD1FD79DD.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza17<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001BD70A514A.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza18<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0025F2BD04AD.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza19<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001BD710FADC.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")


traza20<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/94CCB91B032C.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza21<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/088039A22100.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza22<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/0022CE977359C.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza23<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001ADE98D58A.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza24<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/386BBB15666A.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza25<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/001CEA64E4AC.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza26<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/00223AEEC591.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza27<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/503955693D40.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza28<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/5039556995F6.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza29<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/34BDFA8A9080.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza30<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/6 Mbps Res/00159A422F7C.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")

C<-6000000  #Capacidad del canal Mbps
B<-5
u<-35.09
t2<-25.53
H<-0.75

s<-(B+(C+u)*t)/(t2*t^(2*H)) 
t<-(B/(C-u))*(H/(1-H))      

Bw_MFB<-u+(((s*t2)/2)*(t^(2*H-1)))

mean(traza30$Total.Incoming.bps, na.rm = TRUE)

