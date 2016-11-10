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
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#Trazas medidas con SASS
traza1<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_ 5039554F580D.csv",
                header=TRUE,sep=",",na.strings="NA",dec=".")
traza2<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_ 5039556D04A7.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza3<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_ 5039556D0074.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza4<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_ 50395547B04E .csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza5<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_001ADE98D58A.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza6<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_001BD7A7BDE4.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza7<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_001BD710FADC.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza8<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_001CEA64E4AC.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza9<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_7CBFB1B96514.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")

traza10<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_0014E8295DC2.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza11<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_0022CE977359.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza12<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_0023BED75BEA.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza13<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_34BDFA8A9080.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza14<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_34BDFA8BB6A9.csv",
                 header=TRUE,sep=",",na.strings="NA",dec=".")
traza15<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_34BDFA8C66F3.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza16<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_94CCB91B032C.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza17<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_145BD1FD79DD.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza18<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_00159A4228E8.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza19<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_00159A6160B0.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")

traza20<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_00223AEEC591.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza21<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_386BBB15666A.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza22<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_088039A22100.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza23<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_5039554DEF9A.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza24<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_5039556B3A1E.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza25<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_5039556D0074.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza26<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_50395547CC55.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza27<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_503955444D9B.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza28<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_503955693D40.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza29<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_5039554421B8.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")
traza30<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_E48399206392.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")


for (i in 1:30) {
  cat("u",i,"<-","mean(traza",i,"$CM.DS.KBPS, na.rm = TRUE)","\n",sep = "")
}

u1<-mean(traza1$CM.DS.KBPS, na.rm = TRUE)
u2<-mean(traza2$CM.DS.KBPS, na.rm = TRUE)
u3<-mean(traza3$CM.DS.KBPS, na.rm = TRUE)
u4<-mean(traza4$CM.DS.KBPS, na.rm = TRUE)
u5<-mean(traza5$CM.DS.KBPS, na.rm = TRUE)
u6<-mean(traza6$CM.DS.KBPS, na.rm = TRUE)
u7<-mean(traza7$CM.DS.KBPS, na.rm = TRUE)
u8<-mean(traza8$CM.DS.KBPS, na.rm = TRUE)
u9<-mean(traza9$CM.DS.KBPS, na.rm = TRUE)
u10<-mean(traza10$CM.DS.KBPS, na.rm = TRUE)
u11<-mean(traza11$CM.DS.KBPS, na.rm = TRUE)
u12<-mean(traza12$CM.DS.KBPS, na.rm = TRUE)
u13<-mean(traza13$CM.DS.KBPS, na.rm = TRUE)
u14<-mean(traza14$CM.DS.KBPS, na.rm = TRUE)
u15<-mean(traza15$CM.DS.KBPS, na.rm = TRUE)
u16<-mean(traza16$CM.DS.KBPS, na.rm = TRUE)
u17<-mean(traza17$CM.DS.KBPS, na.rm = TRUE)
u18<-mean(traza18$CM.DS.KBPS, na.rm = TRUE)
u19<-mean(traza19$CM.DS.KBPS, na.rm = TRUE)
u20<-mean(traza20$CM.DS.KBPS, na.rm = TRUE)
u21<-mean(traza21$CM.DS.KBPS, na.rm = TRUE)
u22<-mean(traza22$CM.DS.KBPS, na.rm = TRUE)
u23<-mean(traza23$CM.DS.KBPS, na.rm = TRUE)
u24<-mean(traza24$CM.DS.KBPS, na.rm = TRUE)
u25<-mean(traza25$CM.DS.KBPS, na.rm = TRUE)
u26<-mean(traza26$CM.DS.KBPS, na.rm = TRUE)
u27<-mean(traza27$CM.DS.KBPS, na.rm = TRUE)
u28<-mean(traza28$CM.DS.KBPS, na.rm = TRUE)
u29<-mean(traza29$CM.DS.KBPS, na.rm = TRUE)
u30<-mean(traza30$CM.DS.KBPS, na.rm = TRUE)

for (i in 1:30) {
  cat("V",i,"<-","var(traza",i,"$CM.DS.KBPS, na.rm = TRUE)","\n",sep = "")
}
#########
V1<-var(traza1$CM.DS.KBPS, na.rm = TRUE)
V2<-var(traza2$CM.DS.KBPS, na.rm = TRUE)
V3<-var(traza3$CM.DS.KBPS, na.rm = TRUE)
V4<-var(traza4$CM.DS.KBPS, na.rm = TRUE)
V5<-var(traza5$CM.DS.KBPS, na.rm = TRUE)
V6<-var(traza6$CM.DS.KBPS, na.rm = TRUE)
V7<-var(traza7$CM.DS.KBPS, na.rm = TRUE)
V8<-var(traza8$CM.DS.KBPS, na.rm = TRUE)
V9<-var(traza9$CM.DS.KBPS, na.rm = TRUE)
V10<-var(traza10$CM.DS.KBPS, na.rm = TRUE)
V11<-var(traza11$CM.DS.KBPS, na.rm = TRUE)
V12<-var(traza12$CM.DS.KBPS, na.rm = TRUE)
V13<-var(traza13$CM.DS.KBPS, na.rm = TRUE)
V14<-var(traza14$CM.DS.KBPS, na.rm = TRUE)
V15<-var(traza15$CM.DS.KBPS, na.rm = TRUE)
V16<-var(traza16$CM.DS.KBPS, na.rm = TRUE)
V17<-var(traza17$CM.DS.KBPS, na.rm = TRUE)
V18<-var(traza18$CM.DS.KBPS, na.rm = TRUE)
V19<-var(traza19$CM.DS.KBPS, na.rm = TRUE)
V20<-var(traza20$CM.DS.KBPS, na.rm = TRUE)
V21<-var(traza21$CM.DS.KBPS, na.rm = TRUE)
V22<-var(traza22$CM.DS.KBPS, na.rm = TRUE)
V23<-var(traza23$CM.DS.KBPS, na.rm = TRUE)
V24<-var(traza24$CM.DS.KBPS, na.rm = TRUE)
V25<-var(traza25$CM.DS.KBPS, na.rm = TRUE)
V26<-var(traza26$CM.DS.KBPS, na.rm = TRUE)
V27<-var(traza27$CM.DS.KBPS, na.rm = TRUE)
V28<-var(traza28$CM.DS.KBPS, na.rm = TRUE)
V29<-var(traza29$CM.DS.KBPS, na.rm = TRUE)
V30<-var(traza30$CM.DS.KBPS, na.rm = TRUE)
########

u<-round(c(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21,u22,u23,u24,u25,u26,u27,u28,u29,u30),2)
v<-round(c(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V24,V25,V26,V27,V28,V29,V30),2)

tabla<-data.frame("TazaArribo_Kbps"=u,"VarianzaArribo_Kbps"=v)
tabla

DS<-data.frame(Time=traza1$Time.Description,DS=traza1$CM.DS.KBPS)
setDS<-na.omit(DS)
plot(setDS$DS,type="l",ylab="Kbps",xlab="Tiempo",main="Tráfico Fuente Internet 6M")

#cálculo parámetro H
for (i in 1:30) {
  cat("DS",i,"<-","na.omit(traza",i,"$CM.DS.KBPS, na.rm = TRUE)","\n",sep = "")
}

######
DS1<-na.omit(traza1$CM.DS.KBPS, na.rm = TRUE)
DS2<-na.omit(traza2$CM.DS.KBPS, na.rm = TRUE)
DS3<-na.omit(traza3$CM.DS.KBPS, na.rm = TRUE)
DS4<-na.omit(traza4$CM.DS.KBPS, na.rm = TRUE)
DS5<-na.omit(traza5$CM.DS.KBPS, na.rm = TRUE)
DS6<-na.omit(traza6$CM.DS.KBPS, na.rm = TRUE)
DS7<-na.omit(traza7$CM.DS.KBPS, na.rm = TRUE)
DS8<-na.omit(traza8$CM.DS.KBPS, na.rm = TRUE)
DS9<-na.omit(traza9$CM.DS.KBPS, na.rm = TRUE)
DS10<-na.omit(traza10$CM.DS.KBPS, na.rm = TRUE)
DS11<-na.omit(traza11$CM.DS.KBPS, na.rm = TRUE)
DS12<-na.omit(traza12$CM.DS.KBPS, na.rm = TRUE)
DS13<-na.omit(traza13$CM.DS.KBPS, na.rm = TRUE)
DS14<-na.omit(traza14$CM.DS.KBPS, na.rm = TRUE)
DS15<-na.omit(traza15$CM.DS.KBPS, na.rm = TRUE)
DS16<-na.omit(traza16$CM.DS.KBPS, na.rm = TRUE)
DS17<-na.omit(traza17$CM.DS.KBPS, na.rm = TRUE)
DS18<-na.omit(traza18$CM.DS.KBPS, na.rm = TRUE)
DS19<-na.omit(traza19$CM.DS.KBPS, na.rm = TRUE)
DS20<-na.omit(traza20$CM.DS.KBPS, na.rm = TRUE)
DS21<-na.omit(traza21$CM.DS.KBPS, na.rm = TRUE)
DS22<-na.omit(traza22$CM.DS.KBPS, na.rm = TRUE)
DS23<-na.omit(traza23$CM.DS.KBPS, na.rm = TRUE)
DS24<-na.omit(traza24$CM.DS.KBPS, na.rm = TRUE)
DS25<-na.omit(traza25$CM.DS.KBPS, na.rm = TRUE)
DS26<-na.omit(traza26$CM.DS.KBPS, na.rm = TRUE)
DS27<-na.omit(traza27$CM.DS.KBPS, na.rm = TRUE)
DS28<-na.omit(traza28$CM.DS.KBPS, na.rm = TRUE)
DS29<-na.omit(traza29$CM.DS.KBPS, na.rm = TRUE)
DS30<-na.omit(traza30$CM.DS.KBPS, na.rm = TRUE)
######
for (i in 1:30) {
  cat("h",i,"<-","as.ts(DS",i,")","\n",sep = "")
}

######
h1<-as.ts(DS1)
h2<-as.ts(DS2)
h3<-as.ts(DS3)
h4<-as.ts(DS4)
h5<-as.ts(DS5)
h6<-as.ts(DS6)
h7<-as.ts(DS7)
h8<-as.ts(DS8)
h9<-as.ts(DS9)
h10<-as.ts(DS10)
h11<-as.ts(DS11)
h12<-as.ts(DS12)
h13<-as.ts(DS13)
h14<-as.ts(DS14)
h15<-as.ts(DS15)
h16<-as.ts(DS16)
h17<-as.ts(DS17)
h18<-as.ts(DS18)
h19<-as.ts(DS19)
h20<-as.ts(DS20)
h21<-as.ts(DS21)
h22<-as.ts(DS22)
h23<-as.ts(DS23)
h24<-as.ts(DS24)
h25<-as.ts(DS25)
h26<-as.ts(DS26)
h27<-as.ts(DS27)
h28<-as.ts(DS28)
h29<-as.ts(DS29)
h30<-as.ts(DS30)
######

for (i in 1:30) {
  cat("RS",i,"<-","rsFit(h",i,",levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)","\n",sep = "")
}

######
RS1<-rsFit(h1,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS2<-rsFit(h2,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS3<-rsFit(h3,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS4<-rsFit(h4,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS5<-rsFit(h5,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS6<-rsFit(h6,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS7<-rsFit(h7,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS8<-rsFit(h8,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS9<-rsFit(h9,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS10<-rsFit(h10,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS11<-rsFit(h11,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS12<-rsFit(h12,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS13<-rsFit(h13,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS14<-rsFit(h14,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS15<-rsFit(h15,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS16<-rsFit(h16,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS17<-rsFit(h17,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS18<-rsFit(h18,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS19<-rsFit(h19,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS20<-rsFit(h20,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS21<-rsFit(h21,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS22<-rsFit(h22,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS23<-rsFit(h23,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS24<-rsFit(h24,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS25<-rsFit(h25,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS26<-rsFit(h26,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS27<-rsFit(h27,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS28<-rsFit(h28,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS29<-rsFit(h29,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
RS30<-rsFit(h30,levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),doplot = TRUE, trace = FALSE, title = NULL, description = NULL)

for (i in 1:30) {
  cat("H",i,"<-","data.frame(RS",i,"@hurst)$H","[1]","\n",sep = "")
}
#########
H1<-data.frame(RS1@hurst)$H[1]
H2<-data.frame(RS2@hurst)$H[1]
H3<-data.frame(RS3@hurst)$H[1]
H4<-data.frame(RS4@hurst)$H[1]
H5<-data.frame(RS5@hurst)$H[1]
H6<-data.frame(RS6@hurst)$H[1]
H7<-data.frame(RS7@hurst)$H[1]
H8<-data.frame(RS8@hurst)$H[1]
H9<-data.frame(RS9@hurst)$H[1]
H10<-data.frame(RS10@hurst)$H[1]
H11<-data.frame(RS11@hurst)$H[1]
H12<-data.frame(RS12@hurst)$H[1]
H13<-data.frame(RS13@hurst)$H[1]
H14<-data.frame(RS14@hurst)$H[1]
H15<-data.frame(RS15@hurst)$H[1]
H16<-data.frame(RS16@hurst)$H[1]
H17<-data.frame(RS17@hurst)$H[1]
H18<-data.frame(RS18@hurst)$H[1]
H19<-data.frame(RS19@hurst)$H[1]
H20<-data.frame(RS20@hurst)$H[1]
H21<-data.frame(RS21@hurst)$H[1]
H22<-data.frame(RS22@hurst)$H[1]
H23<-data.frame(RS23@hurst)$H[1]
H24<-data.frame(RS24@hurst)$H[1]
H25<-data.frame(RS25@hurst)$H[1]
H26<-data.frame(RS26@hurst)$H[1]
H27<-data.frame(RS27@hurst)$H[1]
H28<-data.frame(RS28@hurst)$H[1]
H29<-data.frame(RS29@hurst)$H[1]
H30<-data.frame(RS30@hurst)$H[1]

#falta 24

for (i in 1:30) {
  cat("H",i,"=0.82",",",sep = "")
}

H24<-H1
H<-c(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,
     H20,H21,H22,H23,H24,H25,H26,H27,H28,H29,H30)
seth<-data.frame(Hurst=H)

H<-c(H1=0.82,H2=0.82,H3=0.82,H4=0.82,H5=0.82,H6=0.82,H7=0.82,H8=0.82,H9=0.82,H10=0.82,
     H11=0.82,H12=0.82,H13=0.82,H14=0.82,H15=0.82,H16=0.82,H17=0.82,H18=0.82,H19=0.82,
     H20=0.82,H21=0.82,H22=0.82,H23=0.82,H24=0.82,H25=0.82,H26=0.82,H27=0.82,H28=0.82,
     H29=0.82,H30=0.82)

#Capacidad máxima de cada fuente

for (i in 1:30) {
  cat("c",i,"<-","max(DS",i,")","\n",sep = "")
}

for (i in 1:30) {
  cat("c",i,",",sep = "")
}
#######
c1<-max(DS1)
c2<-max(DS2)
c3<-max(DS3)
c4<-max(DS4)
c5<-max(DS5)
c6<-max(DS6)
c7<-max(DS7)
c8<-max(DS8)
c9<-max(DS9)
c10<-max(DS10)
c11<-max(DS11)
c12<-max(DS12)
c13<-max(DS13)
c14<-max(DS14)
c15<-max(DS15)
c16<-max(DS16)
c17<-max(DS17)
c18<-max(DS18)
c19<-max(DS19)
c20<-max(DS20)
c21<-max(DS21)
c22<-max(DS22)
c23<-max(DS23)
c24<-max(DS24)
c25<-max(DS25)
c26<-max(DS26)
c27<-max(DS27)
c28<-max(DS28)
c29<-max(DS29)
c30<-max(DS30)
########

#Ejemplo traza 1
data<-setDS$DS
h<-as.ts(data)
RS<-rsFit(h, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
          doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
H<-data.frame(RS@hurst)$H[1]
RS

#-------------------------------------------------------------------------------------------------
#Ejemplo para la traza 1
library(fArma)
traza<-read.csv("C:/Users/NataliaA/Documents/Maestria/Tráfico L3/Subscribers_SA/Bitrate+for+CM_ 5039554F580D.csv",
                  header=TRUE,sep=",",na.strings="NA",dec=".")

DS<-data.frame(Time=traza$Time.Description,DS=traza$CM.DS.KBPS)
setDS<-na.omit(DS)
plot(setDS$DS,type="l",ylab="Kbps",xlab="Tiempo",main="Tráfico Fuente Internet")

data<-setDS$DS

#Cálculo parámetro Hurst
h<-as.ts(data)
RS<-rsFit(h, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
          doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
H<-data.frame(RS@hurst)$H[1]
RS

C<-max(data)#6000  #Capacidad del canal Mbps
u<-mean(data, na.rm = TRUE)
v<-var(data, na.rm = TRUE)


time<-log10(seq(length=100, from=1.1, to=100))    #time parameter (ms)
t<-time
Buffer<-(C-u)*((t*(1-H))/H) #ms
plot(Buffer)
B<-(Buffer)
t<-round((B/(C-u))*(H/(1-H)),3)      #time parameter (us)
s<-round((B+(C-u)*t)/(v*t^(2*H)),3)   #space parameter (bitss^-1)
BWE<-round(u+(((s*v)/2)*(t^(2*H-1))),2)

#Optimización para s
s.nls<-nls(BWE~u+(((s*v)/2)*(t^(2*H-1))),start = list(s=0.004))
space<-as.numeric(coef(s.nls))

#Optimización para t
t.nls<-nls(BWE~u+(((s*v)/2)*(t^(2*H-1))),start = list(t=1))
time<-as.numeric(coef(t.nls))


#cálculo de Ancho de banda efectivo
t<-time
s<-space
BWE<-round(u+(((s*v)/2)*(t^(2*H-1))),2)
BWE

x<-u
n<-BWE
plot(setDS$DS,type="l",ylab="Kbps",xlab="Tiempo",main="Tráfico Fuente Internet 6M",ylim=c(0,6000))
abline(h=n,col="red")
abline(h=x,col="blue")
grid()


