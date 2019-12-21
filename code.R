#Ursprung der Daten

#Die Daten sind auf der Website des DEUTSCHEN WETTERDIENSTES (DWD) abzurufen
#ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/wind/historical/ (st?ndliche Winddaten Deutschlands)

#Der DWD stellt Daten verschiedener Klimaelemente ?ber den CDC FTP-Server (Climate Data Center) zur Verfuegung
#Dort findet man auch eine detaillierte Leistungsbeschreibung der einzelnen Datensaetze
#https://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html

#Working Direction und Daten einlesen
#Mac
setwd("~/ownCloud/data")
#Windows
setwd("C:/Users/lmueller/Desktop/MPI/data")

#Data Variable deklarieren
wind <- read.csv2(file = "dataz.csv", sep=";")

#transformieren der messdaten und zeiten

a = substr(wind$MESS_DATUM,1,4) #variable a=jahreszahl
b = substr(wind$MESS_DATUM,5,6) #variable b=monat
c = substr(wind$MESS_DATUM,7,8) #variable c=tag
d = substr(wind$MESS_DATUM,9,10)#variable d=stunde


abcd = paste(c,b,a,d) #ordnen nach europ?ischem format dmy h

date = as.POSIXct(abcd, tz = "Europe/Berlin", format = '%d %m %Y %H') #character in zeitdatum

wind = cbind(date,wind) #zusammenf?gen von den spalten date und datensatz

rm(a,b,c,d,abcd, date) #l?schen unn?tzer datasets in global environment

wind = wind[,-c(2,3,4,6,7)] #unnuetze spalten werden entfernt

wind$F <- as.numeric(as.character(wind$F)) #Werte nummerisch speichern

wind = data.frame(wind[which(wind$date>="2017-02-01 00:00:00"),] ) #Messdaten vor 2017-02-01 werden gelöscht
wind = data.frame(wind[which(wind$date<="2018-02-01 00:00:00"),] ) #Messdaten vor 2017-01-21 werden gelöscht


# n=8760 = 364 Tage = 1 Jahr

#-----------------------------------------------------------------------

#timestamp einrichten, um fehlende werte zu filtern (erscheinen mit -1 )

ts1 <- ts(wind$F, freq = 24)


wind$timestamp <- as.numeric(as.POSIXct(wind$date)); 
for(i in 1:13047) {
  next_timestamp <- as.numeric(as.POSIXct("2017-01-11 00:00:00"))+(i-1)*3600;
  
  if(next_timestamp %in% wind$timestamp) {
    
  }
  else {
    time_string <- as.character(as.POSIXlt(as.numeric(next_timestamp),origin="1970-01-01"))
    wind[nrow(wind)+1,] <- c(time_string,-1,as.numeric(next_timestamp))
  }
  
}


wind$F <- as.numeric(as.character(wind$F)) #Werte nummerisch speichern

#-----------------------------------------------------------------------
wind$F[wind$F == -1] <- NA
sum(is.na(wind$F)) #Anzahl NAs: 154 
(154/13201)*100 # Anteil fehlender Daten: 1.17 %
                # 2017-01-13 17:00:00 - 2017-01-20 01:00:00 
                # 2018-01-29 02:00:00 

#-----------------------------------------------------------------------
  
mean(replace(wind$F, wind$F== -1.0, NA), na.rm = TRUE) #Mittelwert ohne NAs
x <- mean(wind$F[which(wind$F!=-1.0)])

wind$F[wind$F == -1.0] <- x #Werte mit Mittelwert aus restlichen Daten belegen

options(digits=2) #Werte auf eine Nachkommastelle runden

mean(wind$F) #neuer Mittelwert (=alter Mittelwert)


#-----------------------------------------------------------------------

#TSA: Zeitverlauf Windgeschwindigkeiten der letzten 364 Tage

Geschwindigkeitz <- ts(wind$F, freq = 24)

 
plot(Geschwindigkeitz, main= "Zeitreihe Windgeschwindigkeiten Hornisgrinde",	ylim = c(0,30), xlab = "Tage", ylab = "Windgeschwindigkeit [m/s]", type = "l")

hist(Geschwindigkeitz, main = "Häufigkeitsverteilung Windgeschwindigkeiten", xlab = "Windgeschwindigkeit [m/s]", ylab = "Häufigkeit")

boxplot(Geschwindigkeitz, main = "Boxplot der Windgeschwindigkeiten", horizontal = TRUE)

summary(Geschwindigkeitz)

max(wind$F) #Hoechstgeschwindigkeit
min(wind$F) #Niedrigstgeschwindigkeit
mean(wind$F) #Mittelwert
sd(wind$F) #Standartabweichung
var(wind$F) #Varianz

fit <- stl(Geschwindigkeitz, s.window = "period")
plot(fit, main = "loess- Verfahren der Zeitreihe")

install.packages("forecast")
library(forecast)
seasonplot(Geschwindigkeitz,col=c("blue"))

#-----------------------------------------------------------------------

#Pruefung der Volatilitaeten 

diffz <- diff(Geschwindigkeitz)

mean(diffz) #0-0007535107

summary(diffz)
max(diffz)
min(diffz)
sd(diffz)

plot(diffz, xlab="Tage", ylab = "Windgeschwindigkeit [m/s]",
      main="erste Differenzen")
abline(h= 0 + sd(diffz),col="red")
abline(h= 0 - sd(diffz),col="red")

hist(diffz, xlab = "Windgeschwindigkeit [m/s]", ylab = "Häuufigkeit", main = "Histogram der ersten Differenzen")
abline(v=mean(diffz),col="blue")
abline(v= 0 + sd(diffz),col="red")
abline(v= 0 - sd(diffz),col="red")


#Sd von den ersten Differenzen der gerundeten Werte
r.winz <- round(Geschwindigkeitz,0)
r.winz
wind1<-which(r.winz==1)
wind1
diff.wind<-diff(Geschwindigkeitz)
diff.wind[wind1]
wind1<-sd(diff.wind[wind1])

wind2<-which(r.winz==2)
wind2<-sd(diff.wind[wind2])

wind3<-which(r.winz==3)
wind3<-sd(diff.wind[wind3])

wind4<-which(r.winz==4)
wind4<-sd(diff.wind[wind4])

wind5<-which(r.winz==5)
wind5<-sd(diff.wind[wind5])

wind6<-which(r.winz==6)
wind6<-sd(diff.wind[wind6]) 

wind7<-which(r.winz==7)
wind7<-sd(diff.wind[wind7])

wind8<-which(r.winz==8)
wind8<-sd(diff.wind[wind8])

wind9<-which(r.winz==9)
wind9<-sd(diff.wind[wind9])

wind10<-which(r.winz==10)
wind10<-sd(diff.wind[wind10])

wind11<-which(r.winz==11)
wind11<-sd(diff.wind[wind11])  

wind12<-which(r.winz==12)
wind12<-sd(diff.wind[wind12])

wind13<-which(r.winz==13)
wind13<-sd(diff.wind[wind13])

wind14<-which(r.winz==14)
wind14<-sd(diff.wind[wind14])

wind15<-which(r.winz==15)
wind15<-sd(diff.wind[wind15])

wind16<-which(r.winz==16)
wind16<-sd(diff.wind[wind16])  

wind17<-which(r.winz==17)
wind17<-sd(diff.wind[wind17])

wind18<-which(r.winz==18)
wind18<-sd(diff.wind[wind18])

wind19<-which(r.winz==19)
wind19<-sd(diff.wind[wind19])

wind20<-which(r.winz==20)
wind20<-sd(diff.wind[wind20])

wind21<-which(r.winz==21)
wind21<-sd(diff.wind[wind21])  

wind22<-which(r.winz==22)
wind22<-sd(diff.wind[wind22])

wind23<-which(r.winz==23)
wind23<-sd(diff.wind[wind23])

wind24<-which(r.winz==24)
wind24<-sd(diff.wind[wind24])

wind25<-which(r.winz==25)
wind25<-sd(diff.wind[wind20])

wind26<-which(r.winz==26)
wind26<-sd(diff.wind[wind26])  

wind27<-which(r.winz==27)
wind27<-sd(diff.wind[wind17])


#speichern der SA der ersten Differenz in Tabelle und Darstellung

w <- factor(c(1:27))
x <- c(wind1,wind2,wind3,wind4,wind5,wind6,wind7,wind8,wind9,wind10,wind11,wind12,wind13,wind14,wind15,wind16,wind17,wind18,wind19,wind20,wind21,wind22,wind23,wind24,wind25,wind26,wind27)
SAD <- data.frame(w,x)
colnames(SAD) <- c("Windgeschwindigkeiten (gerundet)", "Standartabweichung der 1. Differenz")

plot(SAD, main ="Standartabweichungen der 1. Differenzen", type = "p\")")


#Haufigkeitsverteilung der 1. Differenz

y <- table(diffz)
plot(y, main = "Volatilität der 1. Differenz der Windgeschwindigkeiten [m/s]", xlab = "Windgeschwindigkeit [m/s]", ylab= "Anzahl der Beobachtungen [N]", type = "p\")")

#-----------------------------------------------------------------------

#Weibull Funktion 

#Fitdist Ansatz und Plot (fitdistrplus) mit Maximum Goodness of fit

library(MASS)
library(carData)
library(car)
library(survival)
library(fitdistrplus)
library(stats4)
library(bbmle)

fit_w  <- fitdist(wind$F, "weibull", method = "mge")
plot(fit_w)

summary(fit_w)  #Parameter shape und scale angeben

#R-Code fuer MLE und Histogramm (EnvStats)

install.packages("EnvStats")
library(EnvStats)mle

mle_Weibull=eweibull(wind$F, method="mle")
x=seq(0,max(wind$F), by=0.1)[-1]
hist(Geschwindigkeitz,main = "rel. Häufigkeitsverteilung der Windgeschwindigkeiten", xlab = "Windgeschwindigkeit [m/s]", ylab = "rel. Häufigkeit", probability = TRUE)
curve(dweibull(x, shape=mle_Weibull$parameters[1], scale=mle_Weibull$parameters[2]),from =0, to=30, add=TRUE, col="red")

mle_Weibull$parameters #Parameter shape und scale angeben

#-----------------------------------------------------------------------

#Leistungskurven der WKA in Schwarzwald-Hornisgrinde

lk <- data.frame(read.csv2(file = "A_E70.csv", sep=";"))
colnames(lk) <- c("wg", "kwh")

wg <- lk$wg
kwh <-lk$kwh


#Graphische Darstellung der Leistungskurve der WKA
plot(wg,kwh, xlab="Windgeschwindigkeit [m/s]", ylab="Leistung [kw/h]", main="Leistungskurve E 70")
lines(wg,kwh)

#-----------------------------------------------------------------------

#Leistung ueber gesamte Zeitreihe 

#Subset der gerundeten Windgeschwindigkeiten Insgesamt
t <- data.frame(round(wind$F))
colnames(t) <- c("wg")


#Merge gerundete Windgeschwindigkeiten und Werte Leistungskurve
tlk <- merge(t,lk)

#Haeufigkeitsverteilung der Leistungen Insgesamt 
h = hist(tlk$kwh, freq = FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "Prozentuale Häufigkeit der Leistungen Insgesamt", xlab = "Leistung [kw/h]", ylab = "Häufigkeit")

#Kwh Insgesamt
sum(tlk$kwh)

#Zeitlicher Verlauf der Leistungen Insgesamt

tz <- data.frame(wind$date,round(wind$F))
colnames(tz) <- c("date","wg")

z <- merge(tz,lk)

library(ggplot2)

ggplot(z) + 
  geom_line(data=z, aes(x=z$date, y=z$kwh))+
  labs(title="Zeitlicher Verlauf der Leistungen Insgesamt", x= "Zeit [h]", y= "Leistung [kw/h]")

#Nutzungsgrad: (5267322/20253600)*100

#-----------------------------------------------------------------------

#Subset der Winddaten Tag 01.02. (Loop (?))

d1 <- subset.data.frame(wind, wind$date=="2017-02-01 00:00:00"
                       |wind$date=="2017-02-01 02:00:00"
                       |wind$date=="2017-02-01 03:00:00"
                       |wind$date=="2017-02-01 04:00:00"
                       |wind$date=="2017-02-01 05:00:00"
                       |wind$date=="2017-02-01 06:00:00"
                       |wind$date=="2017-02-01 07:00:00"
                       |wind$date=="2017-02-01 08:00:00"
                       |wind$date=="2017-02-01 09:00:00"
                       |wind$date=="2017-02-01 10:00:00"
                       |wind$date=="2017-02-01 11:00:00"
                       |wind$date=="2017-02-01 12:00:00"
                       |wind$date=="2017-02-01 13:00:00"
                       |wind$date=="2017-02-01 14:00:00"
                       |wind$date=="2017-02-01 15:00:00"
                       |wind$date=="2017-02-01 16:00:00"
                       |wind$date=="2017-02-01 17:00:00"
                       |wind$date=="2017-02-01 18:00:00"
                       |wind$date=="2017-02-01 19:00:00"
                       |wind$date=="2017-02-01 20:00:00"
                       |wind$date=="2017-02-01 21:00:00"
                       |wind$date=="2017-02-01 22:00:00"
                       |wind$date=="2017-02-01 23:00:00"
)

#Subset der gerundeten Windgeschwindigkeiten Tag 01.02.
d1r <- data.frame(round(d1$F))
colnames(d1r) <- c("wg")

#Graphische Darstellung der Windgeschwindigkeiten Tag 01.02.
hist(d1r$wg, main="Häufigkeiten der Wingeschwindigkeiten 01-02-2017", xlab = "Windgeschwindigkeit [m/s]", ylab = "Häufigkeit")

#Merge gerundete Windgeschwindigkeiten und Werte Leistungskurve
d1lk <- merge(lk,d1r)

#Haeufigkeitsverteilung der Leistungen Tag 01.02. 
h = hist(d1lk$kwh, freq = FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "Prozentuale Häufigkeit der Leistungen 01-02-2017", xlab = "Leistung [kw/h]", ylab = "Häufigkeit")

#Graphische Darstellung als Gruppen
plot(d1lk$kwh, main = "Häufigkeiten der Leistungsgruppen am 01.02.2017", xlab = "Windgeschwindigkeit [m/s]", ylab= "Leistung [kw/h]")

#Kwh gesamt Tag 01.02.
sum(d1lk$kwh)

#Maximalleistung am Tag: 2310*24 = 55440
#Nutzungsgrad
6242/55440

#Zeitlicher Verlauf der Leistungen 01.07.

d1z <- data.frame(d1$date,round(d1$F))
colnames(d1z) <- c("date","wg")

x <- merge(d1z,lk)

#FEHLT: ABSOLUTE HAEUFIGKEIT DER LEISTUNGSGRUPPEN
plot(x$date,x$kwh, ylim=c(0,2310))


library(ggplot2)

ggplot(x) + 
  geom_line(data=x, aes(x=x$date, y=x$kwh)) +
  labs(title="Zeitlicher Verlauf der Leistungen am 01.02.2017", x= "Zeit [h]", y= "Leistung [kw/h]")


#-----------------------------------------------------------------------

#Subset der Winddaten Tag 01.07.

d2 <- subset.data.frame(wind, wind$date=="2017-07-01 00:00:00"
                        |wind$date=="2017-07-01 02:00:00"
                        |wind$date=="2017-07-01 03:00:00"
                        |wind$date=="2017-07-01 04:00:00"
                        |wind$date=="2017-07-01 05:00:00"
                        |wind$date=="2017-07-01 06:00:00"
                        |wind$date=="2017-07-01 07:00:00"
                        |wind$date=="2017-07-01 08:00:00"
                        |wind$date=="2017-07-01 09:00:00"
                        |wind$date=="2017-07-01 10:00:00"
                        |wind$date=="2017-07-01 11:00:00"
                        |wind$date=="2017-07-01 12:00:00"
                        |wind$date=="2017-07-01 13:00:00"
                        |wind$date=="2017-07-01 14:00:00"
                        |wind$date=="2017-07-01 15:00:00"
                        |wind$date=="2017-07-01 16:00:00"
                        |wind$date=="2017-07-01 17:00:00"
                        |wind$date=="2017-07-01 18:00:00"
                        |wind$date=="2017-07-01 19:00:00"
                        |wind$date=="2017-07-01 20:00:00"
                        |wind$date=="2017-07-01 21:00:00"
                        |wind$date=="2017-07-01 22:00:00"
                        |wind$date=="2017-07-01 23:00:00"
)

#Subset der gerundeten Windgeschwindigkeiten Tag 01.07.
d2r <- data.frame(round(d2$F))
colnames(d2r) <- c("wg")

#Graphische Darstellung der Windgeschwindigkeiten Tag 01.07.
hist(d2r$wg, main="Häufigkeiten der Wingeschwindigkeiten 01-07-2017", xlab = "Windgeschwindigkeit [m/s]", ylab = "Häufigkeit")

#Merge gerundete Windgeschwindigkeiten und Werte Leistungskurve
d2lk <- merge(lk,d2r)

#Haeufigkeitsverteilung der Leistungen Tag 01.07. 
h = hist(d2lk$kwh, freq = FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main = "Prozentuale Häufigkeit der Leistungen 01-07-2017", xlab = "Leistung [kw/h]", ylab = "Häufigkeit")

#Graphische Darstellung als Gruppen
plot(d2lk$kwh, main = "Häufigkeiten der Leistungsgruppen", xlab = "Windgeschwindigkeit [m/s]", ylab= "Leistung [kw/h]")

#Kwh gesamt Tag 01.07.
sum(d2lk$kwh)

24429/55440

#Zeitlicher Verlauf der Leistungen 01.07.

d2z <- data.frame(d2$date,round(d2$F))
colnames(d2z) <- c("date","wg")

y <- merge(d2z,lk)

plot(y$date,y$kwh, ylim=c(0,2310))

library(ggplot2)

ggplot(y) + 
  geom_line(data=y, aes(x=y$date, y=y$kwh)) +
  labs(title="Zeitlicher Verlauf der Leistungen am 01.07.2017", x= "Zeit [h]", y= "Leistung [kw/h]")

#-----------------------------------------------------------------------