#Ursprung der Daten (WIND)

#Die Daten sind auf der Website des DEUTSCHEN WETTERDIENSTES (DWD) abzurufen
#ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/wind/historical/ (stuendliche Winddaten Deutschlands)

#Der DWD stellt Daten verschiedener Klimaelemente �ber den CDC FTP-Server (Climate Data Center) zur Verfuegung
#Dort findet man auch eine detaillierte Leistungsbeschreibung der einzelnen Datensaetze
#https://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html

#Working Direction und Daten einlesen
#Mac
setwd("~/ownCloud/data")
#Windows
setwd("C:/Users/lmueller/Desktop/MPI/data")

#Data Variable deklarieren
# wind (data frame) fuer die stuendlichen Windgeschwindigkeiten in Schwarzwald-Hornisgrinde im Jahr 2017
wind <- read.csv2(file = "dataz.csv", sep=";")

#Transformieren der Messdaten und Zeiten

a = substr(wind$MESS_DATUM,1,4) #variable a=jahreszahl
b = substr(wind$MESS_DATUM,5,6) #variable b=monat
c = substr(wind$MESS_DATUM,7,8) #variable c=tag
d = substr(wind$MESS_DATUM,9,10)#variable d=stunde


abcd = paste(c,b,a,d) #ordnen nach europaeischem format dmy h

date = as.POSIXct(abcd, tz = "Europe/Berlin", format = '%d %m %Y %H') #character in zeitdatum

wind = cbind(date,wind) #zusammenfuegen von den spalten date und datensatz

rm(a,b,c,d,abcd, date) #loeschen unnoetzer datasets in global environment

wind = wind[,-c(2,3,4,6,7)] #unnuetze spalten werden entfernt

wind$F <- as.numeric(as.character(wind$F)) #Werte nummerisch speichern


#Interpolierung: Herausfiltern der Daten fuer das Jahr 2017
#Spalten: wind$date = Messzeitpunkt (Tag, Stunde), wind$F = Windgeschwindigkeit [m/s]

wind = data.frame(wind[which(wind$date>="2017-02-01 00:00:00"),] ) #Messdaten vor 2017-02-01 werden geloescht
wind = data.frame(wind[which(wind$date<="2018-02-01 00:00:00"),] ) #Messdaten nach 2018-02-01 werden geloescht


# n=8760 = 364 Tage = 1 Jahr

#-----------------------------------------------------------------------

#Ursprung der Daten (ANLAGEN)

#Die Daten sind auf der Website "Windenergie im Binnenland" abzurufen
#http://www.windenergie-im-binnenland.de/powercurve.php
#Dort findet man die Leistungsangaben verschiedener auf dem Markt befindlicher WKA als Exceltabelle
#Daten wurden mit den Herstellerangaben in deren Produktbroschueren verglichen und schliesslich fuer die Nutzung in R aufbereitet

#Alternative Anlagen: Erstellung der Leistungskurven

#Data Variable deklarieren
#WKA (data frame) Namen der Windkraftanlagen 1-82
WKA <- data.frame(read.csv2(file = "0_NamenslisteWKA.csv", sep=";"))

#Leistungskurven einlesen und erstellen, Gesamtleistung je Anlage fuer das Jahr 2017 speichern
#Kapitel 5.4.2
#t (Variable) Vektor mit Windgeschwindigkeiten in data frame 
#lk + Nr (1-82) (Variable) für die jeweilige Leistung und zugehoeriger Windgeschwindigkeit je WKA
#test + Nr (1-82) (Variable) fuer die Zuordnung der Leistung und zugehoeriger Windgeschwindigkeit des Messzeitpunktes in wind

lk1 <- data.frame(read.csv2(file = "E1013050.csv", sep=";"))
colnames(lk1) <- c("wg", "kwh") #Aenderung Spaltenbezeichnung 
wg <- lk1$wg #Speichern der Windgeschwindigkeiten der Leistungskurve in Vektor
kwh <-lk1$kwh #Speichern der Leistungen je Messzeitpunkt in Vektor

t <- data.frame(round(wind$F)) #Speichern der gerundeten Windgeschwindigkeiten der Messzeitpunkte in Vektor
colnames(t) <- c("wg")

test1 <- merge(t,lk1) #Zuordnung der gerundeten Windgeschwindigkeit des Messzeitpunktes zu jeweiliger Leistung der WKA 
sum(test1$kwh) #Zusammenrechnung aller Leistungen des Jahres 


lk2 <- data.frame(read.csv2(file = "E101E23.5.csv", sep=";"))
colnames(lk2) <- c("wg", "kwh")
wg <- lk2$wg
kwh <-lk2$kwh

test2 <- merge(t,lk2)
sum(test2$kwh)


lk3 <- data.frame(read.csv2(file = "E1124.5.csv", sep=";"))
colnames(lk3) <- c("wg", "kwh")
wg <- lk3$wg
kwh <-lk3$kwh

test3 <- merge(t,lk3)
sum(test3$kwh)


lk4 <- data.frame(read.csv2(file = "E1152.5.csv", sep=";"))
colnames(lk4) <- c("wg", "kwh")
wg <- lk4$wg
kwh <-lk4$kwh

test4 <- merge(t,lk4)
sum(test4$kwh)


lk5 <- data.frame(read.csv2(file = "E115TES3.csv", sep=";"))
colnames(lk5) <- c("wg", "kwh")
wg <- lk5$wg
kwh <-lk5$kwh

test5 <- merge(t,lk5)
sum(test5$kwh)


lk6 <- data.frame(read.csv2(file = "E115TES3.2.csv", sep=";"))
colnames(lk6) <- c("wg", "kwh")
wg <- lk6$wg
kwh <-lk6$kwh

test6 <- merge(t,lk6)
sum(test6$kwh)


lk7 <- data.frame(read.csv2(file = "E126EP4TES4.2.csv", sep=";"))
colnames(lk7) <- c("wg", "kwh")
wg <- lk7$wg
kwh <-lk7$kwh

test7 <- merge(t,lk7)
sum(test7$kwh)


lk8 <- data.frame(read.csv2(file = "E1267.5.csv", sep=";"))
colnames(lk8) <- c("wg", "kwh")
wg <- lk8$wg
kwh <-lk8$kwh

test8 <- merge(t,lk8)
sum(test8$kwh)


lk9 <- data.frame(read.csv2(file = "E1414.2.csv", sep=";"))
colnames(lk9) <- c("wg", "kwh")
wg <- lk9$wg
kwh <-lk9$kwh

test9 <- merge(t,lk9)
sum(test9$kwh)


lk10 <- data.frame(read.csv2(file = "eno1002200.csv", sep=";"))
colnames(lk10) <- c("wg", "kwh")
wg <- lk10$wg
kwh <-lk10$kwh

test10 <- merge(t,lk10)
sum(test10$kwh)


lk11 <- data.frame(read.csv2(file = "eno1143500.csv", sep=";"))
colnames(lk11) <- c("wg", "kwh")
wg <- lk11$wg
kwh <-lk11$kwh

test11 <- merge(t,lk11)
sum(test11$kwh)


lk12 <- data.frame(read.csv2(file = "eno1263500.csv", sep=";"))
colnames(lk12) <- c("wg", "kwh")
wg <- lk12$wg
kwh <-lk12$kwh

test12 <- merge(t,lk12)
sum(test12$kwh)


lk13<- data.frame(read.csv2(file = "FLMD77.csv", sep=";"))
colnames(lk13) <- c("wg", "kwh")
wg <- lk13$wg
kwh <-lk13$kwh

test13 <- merge(t,lk13)
sum(test13$kwh)


lk14 <- data.frame(read.csv2(file = "FL200093.csv", sep=";"))
colnames(lk14) <- c("wg", "kwh")
wg <- lk14$wg
kwh <-lk14$kwh

test14 <- merge(t,lk14)
sum(test14$kwh)


lk15 <- data.frame(read.csv2(file = "FL2000100.csv", sep=";"))
colnames(lk15) <- c("wg", "kwh")
wg <- lk15$wg
kwh <-lk15$kwh

test15 <- merge(t,lk15)
sum(test15$kwh)


lk16 <- data.frame(read.csv2(file = "FL2500100.csv", sep=";"))
colnames(lk16) <- c("wg", "kwh")
wg <- lk16$wg
kwh <-lk16$kwh 

test16 <- merge(t,lk16)
sum(test16$kwh)


lk17 <- data.frame(read.csv2(file = "FL2500104.csv", sep=";"))
colnames(lk17) <- c("wg", "kwh")
wg <- lk17$wg
kwh <-lk17$kwh 

test17 <- merge(t,lk17)
sum(test17$kwh)


lk18 <- data.frame(read.csv2(file = "FL3000120.csv", sep=";"))
colnames(lk18) <- c("wg", "kwh")
wg <- lk18$wg
kwh <-lk18$kwh

test18 <- merge(t,lk18)
sum(test18$kwh)


lk19 <- data.frame(read.csv2(file = "GamesaG972MW.csv", sep=";"))
colnames(lk19) <- c("wg", "kwh")
wg <- lk19$wg
kwh <-lk19$kwh

test19 <- merge(t,lk19)
sum(test19$kwh)


lk20 <- data.frame(read.csv2(file = "GamesaG1142MW.csv", sep=";"))
colnames(lk20) <- c("wg", "kwh")
wg <- lk20$wg
kwh <-lk20$kwh

test20 <- merge(t,lk20)
sum(test20$kwh)


lk21 <- data.frame(read.csv2(file = "GamesaG1142.5MW.csv", sep=";"))
colnames(lk21) <- c("wg", "kwh")
wg <- lk21$wg
kwh <-lk21$kwh

test21 <- merge(t,lk21)
sum(test21$kwh)


lk22 <- data.frame(read.csv2(file = "GamesaG1284MW.csv", sep=";"))
colnames(lk22) <- c("wg", "kwh")
wg <- lk22$wg
kwh <-lk22$kwh

test22 <- merge(t,lk22)
sum(test22$kwh)


lk23 <- data.frame(read.csv2(file = "GamesaG1285MW.csv", sep=";"))
colnames(lk23) <- c("wg", "kwh")
wg <- lk23$wg
kwh <-lk23$kwh

test23 <- merge(t,lk23)
sum(test23$kwh)


lk24 <- data.frame(read.csv2(file = "GamesaG13233MW.csv", sep=";"))
colnames(lk24) <- c("wg", "kwh")
wg <- lk24$wg
kwh <-lk24$kwh

test24 <- merge(t,lk24)
sum(test24$kwh)


lk25 <- data.frame(read.csv2(file = "GamesaG1325MW.csv", sep=";"))
colnames(lk25) <- c("wg", "kwh")
wg <- lk25$wg
kwh <-lk25$kwh  

test25 <- merge(t,lk25)
sum(test25$kwh)


lk26 <- data.frame(read.csv2(file = "GEWind2.5120.csv", sep=";"))
colnames(lk26) <- c("wg", "kwh")
wg <- lk26$wg
kwh <-lk26$kwh

test26 <- merge(t,lk26)
sum(test26$kwh)


lk27 <- data.frame(read.csv2(file = "GEWindGE2.75-120.csv", sep=";"))
colnames(lk27) <- c("wg", "kwh")
wg <- lk27$wg
kwh <-lk27$kwh

test27 <- merge(t,lk27)
sum(test27$kwh)


lk28 <- data.frame(read.csv2(file = "GEWindGE3.2-130.csv", sep=";"))
colnames(lk28) <- c("wg", "kwh")
wg <- lk28$wg
kwh <-lk28$kwh

test28 <- merge(t,lk28)
sum(test28$kwh)


lk29 <- data.frame(read.csv2(file = "GEWindGE3.8-130.csv", sep=";"))
colnames(lk29) <- c("wg", "kwh")
wg <- lk29$wg
kwh <-lk29$kwh

test29 <- merge(t,lk29)
sum(test29$kwh)


lk30 <- data.frame(read.csv2(file = "GEWindGE3.4-137.csv", sep=";"))
colnames(lk30) <- c("wg", "kwh")
wg <- lk30$wg
kwh <-lk30$kwh

test30 <- merge(t,lk30)
sum(test30$kwh)


lk31<- data.frame(read.csv2(file = "GEWindGE3.6-137.csv", sep=";"))
colnames(lk31) <- c("wg", "kwh")
wg <- lk31$wg
kwh <-lk31$kwh

test31<- merge(t,lk31)
sum(test31$kwh)


lk32 <- data.frame(read.csv2(file = "GEWindGE4.8-158.csv", sep=";"))
colnames(lk32) <- c("wg", "kwh")
wg <- lk32$wg
kwh <-lk32$kwh

test32 <- merge(t,lk32)
sum(test32$kwh)


lk33 <- data.frame(read.csv2(file = "NordexN902500LS.csv", sep=";"))
colnames(lk33) <- c("wg", "kwh")
wg <- lk33$wg
kwh <-lk33$kwh

test33 <- merge(t,lk33)
sum(test33$kwh)


lk34 <- data.frame(read.csv2(file = "NordexN1494.5MW.csv", sep=";"))
colnames(lk34) <- c("wg", "kwh")
wg <- lk34$wg
kwh <-lk34$kwh

test34 <- merge(t,lk34)
test34$kwh <- as.numeric(as.character(test34$kwh))
sum(test34$kwh)


lk35 <- data.frame(read.csv2(file = "NordexN1313.9MW.csv", sep=";"))
colnames(lk35) <- c("wg", "kwh")
wg <- lk35$wg
kwh <-lk35$kwh

test35 <- merge(t,lk35)
test35$kwh <- as.numeric(as.character(test35$kwh))
sum(test35$kwh)


lk36 <- data.frame(read.csv2(file = "NordexN1002500.csv", sep=";"))
colnames(lk36) <- c("wg", "kwh")
wg <- lk36$wg
kwh <-lk36$kwh

test36 <- merge(t,lk36)
sum(test36$kwh)


lk37 <- data.frame(read.csv2(file = "NordexN1173.6MW.csv", sep=";"))
colnames(lk37) <- c("wg", "kwh")
wg <- lk37$wg
kwh <-lk37$kwh

test37 <- merge(t,lk37)
sum(test37$kwh)


lk38 <- data.frame(read.csv2(file = "NordexN1003300.csv", sep=";"))
colnames(lk38) <- c("wg", "kwh")
wg <- lk38$wg
kwh <-lk38$kwh

test38 <- merge(t,lk38)
sum(test38$kwh)


lk39 <- data.frame(read.csv2(file = "NordexN1313.0MW.csv", sep=";"))
colnames(lk39) <- c("wg", "kwh")
wg <- lk39$wg
kwh <-lk39$kwh

test39 <- merge(t,lk39)
sum(test39$kwh)


lk40 <- data.frame(read.csv2(file = "NordexN1172.4MW.csv", sep=";"))
colnames(lk40) <- c("wg", "kwh")
wg <- lk40$wg
kwh <-lk40$kwh

test40 <- merge(t,lk40)
sum(test40$kwh)


lk41 <- data.frame(read.csv2(file = "NordexN1313.3MW.csv", sep=";"))
colnames(lk41) <- c("wg", "kwh")
wg <- lk41$wg
kwh <-lk41$kwh

test41 <- merge(t,lk41)
test41$kwh <- as.numeric(as.character(test41$kwh))
sum(test41$kwh)


lk42 <- data.frame(read.csv2(file = "NordexN1173MW.csv", sep=";"))
colnames(lk42) <- c("wg", "kwh")
wg <- lk42$wg
kwh <-lk42$kwh

test42 <- merge(t,lk42)
sum(test42$kwh)


lk43 <- data.frame(read.csv2(file = "NordexN1313.6MW.csv", sep=";"))
colnames(lk43) <- c("wg", "kwh")
wg <- lk43$wg
kwh <-lk43$kwh

test43 <- merge(t,lk43)
test43$kwh <- as.numeric(as.character(test43$kwh))
sum(test43$kwh)


lk44 <- data.frame(read.csv2(file = "SenvionMM1002000.csv", sep=";"))
colnames(lk44) <- c("wg", "kwh")
wg <- lk44$wg
kwh <-lk44$kwh

test44 <- merge(t,lk44)
sum(test44$kwh)


lk45 <- data.frame(read.csv2(file = "Senvion3.2M114VG.csv", sep=";"))
colnames(lk45) <- c("wg", "kwh")
wg <- lk45$wg
kwh <-lk45$kwh

test45 <- merge(t,lk45)
sum(test45$kwh)


lk46 <- data.frame(read.csv2(file = "Senvion3.4MNES114.csv", sep=";"))
colnames(lk46) <- c("wg", "kwh")
wg <- lk46$wg
kwh <-lk46$kwh

test46 <- merge(t,lk46)
sum(test46$kwh)


lk47 <- data.frame(read.csv2(file = "Senvion3.6M114.csv", sep=";"))
colnames(lk47) <- c("wg", "kwh")
wg <- lk47$wg
kwh <-lk47$kwh

test47 <- merge(t,lk47)
sum(test47$kwh)


lk48 <- data.frame(read.csv2(file = "Senvion3.0M122.csv", sep=";"))
colnames(lk48) <- c("wg", "kwh")
wg <- lk48$wg
kwh <-lk48$kwh

test48 <- merge(t,lk48)
sum(test48$kwh)


lk49 <- data.frame(read.csv2(file = "Senvion3.2M122NES.csv", sep=";"))
colnames(lk49) <- c("wg", "kwh")
wg <- lk49$wg
kwh <-lk49$kwh

test49 <- merge(t,lk49)
sum(test49$kwh)


lk50 <- data.frame(read.csv2(file = "Senvion 3.4M140EBC.csv", sep=";"))
colnames(lk50) <- c("wg", "kwh")
wg <- lk50$wg
kwh <-lk50$kwh

test50 <- merge(t,lk50)
sum(test50$kwh)


lk51 <- data.frame(read.csv2(file = "Senvion3.6M140EBC.csv", sep=";"))
colnames(lk51) <- c("wg", "kwh")
wg <- lk51$wg
kwh <-lk51$kwh

test51 <- merge(t,lk51)
sum(test51$kwh)


lk52 <- data.frame(read.csv2(file = "SiemensSWT2.3113.csv", sep=";"))
colnames(lk52) <- c("wg", "kwh")
wg <- lk52$wg
kwh <-lk52$kwh

test52 <- merge(t,lk52)
sum(test52$kwh)


lk53 <- data.frame(read.csv2(file = "SiemensSWT3.21132A.csv", sep=";"))
colnames(lk53) <- c("wg", "kwh")
wg <- lk53$wg
kwh <-lk53$kwh

test53 <- merge(t,lk53)
sum(test53$kwh)


lk54 <- data.frame(read.csv2(file = "SiemensSWT3.21132B.csv", sep=";"))
colnames(lk54) <- c("wg", "kwh")
wg <- lk54$wg
kwh <-lk54$kwh

test54 <- merge(t,lk54)
sum(test54$kwh)


lk55 <- data.frame(read.csv2(file = "SiemensSWT3.3130.csv", sep=";"))
colnames(lk55) <- c("wg", "kwh")
wg <- lk55$wg
kwh <-lk55$kwh

test55 <- merge(t,lk55)
sum(test55$kwh)


lk56 <- data.frame(read.csv2(file = "SiemensSWT3.3130LN.csv", sep=";"))
colnames(lk56) <- c("wg", "kwh")
wg <- lk56$wg
kwh <-lk56$kwh

test56 <- merge(t,lk56)
sum(test56$kwh)


lk57 <- data.frame(read.csv2(file = "SiemensSWT3.6120.csv", sep=";"))
colnames(lk57) <- c("wg", "kwh")
wg <- lk57$wg
kwh <-lk57$kwh

test57 <- merge(t,lk57)
sum(test57$kwh)


lk58 <- data.frame(read.csv2(file = "SiemensSWT3.6130.csv", sep=";"))
colnames(lk58) <- c("wg", "kwh")
wg <- lk58$wg
kwh <-lk58$kwh

test58 <- merge(t,lk58)
sum(test58$kwh)


lk59 <- data.frame(read.csv2(file = "SiemensSWT3.15142.csv", sep=";"))
colnames(lk59) <- c("wg", "kwh")
wg <- lk59$wg
kwh <-lk59$kwh

test59 <- merge(t,lk59)
sum(test59$kwh)


lk60 <- data.frame(read.csv2(file = "SiemensSWTDD130.csv", sep=";"))
colnames(lk60) <- c("wg", "kwh")
wg <- lk60$wg
kwh <-lk60$kwh

test60 <- merge(t,lk60)
sum(test60$kwh)


lk61 <- data.frame(read.csv2(file = "SiemensSWTDD142.csv", sep=";"))
colnames(lk61) <- c("wg", "kwh")
wg <- lk61$wg
kwh <-lk61$kwh

test61 <- merge(t,lk61)
sum(test61$kwh)


lk62 <- data.frame(read.csv2(file = "Vensys771500kW.csv", sep=";"))
colnames(lk62) <- c("wg", "kwh")
wg <- lk62$wg
kwh <-lk62$kwh

test62 <- merge(t,lk62)
sum(test62$kwh)


lk63<- data.frame(read.csv2(file = "Vensys821500kW.csv", sep=";"))
colnames(lk63) <- c("wg", "kwh")
wg <- lk63$wg
kwh <-lk63$kwh

test63 <- merge(t,lk63)
sum(test63$kwh)


lk64 <- data.frame(read.csv2(file = "Vensys1002500kW.csv", sep=";"))
colnames(lk64) <- c("wg", "kwh")
wg <- lk64$wg
kwh <-lk64$kwh

test64 <- merge(t,lk64)
sum(test64$kwh)


lk65 <- data.frame(read.csv2(file = "Vensys1092500kW.csv", sep=";"))
colnames(lk65) <- c("wg", "kwh")
wg <- lk65$wg
kwh <-lk65$kwh

test65 <- merge(t,lk65)
sum(test65$kwh)


lk66 <- data.frame(read.csv2(file = "Vensys1122500kW.csv", sep=";"))
colnames(lk66) <- c("wg", "kwh")
wg <- lk66$wg
kwh <-lk66$kwh

test66 <- merge(t,lk66)
sum(test66$kwh)


lk67 <- data.frame(read.csv2(file = "Vensys1203000kW.csv", sep=";"))
colnames(lk67) <- c("wg", "kwh")
wg <- lk67$wg
kwh <-lk67$kwh

test67 <- merge(t,lk67)
sum(test67$kwh)


lk68 <- data.frame(read.csv2(file = "VestasV902000GS.csv", sep=";"))
colnames(lk68) <- c("wg", "kwh")
wg <- lk68$wg
kwh <-lk68$kwh

test68 <- merge(t,lk68)
sum(test68$kwh)


lk69 <- data.frame(read.csv2(file = "VestasV1001.8.csv", sep=";"))
colnames(lk69) <- c("wg", "kwh")
wg <- lk69$wg
kwh <-lk69$kwh

test69 <- merge(t,lk69)
sum(test69$kwh)


lk70 <- data.frame(read.csv2(file = "VestasV1001.8GS.csv", sep=";"))
colnames(lk70) <- c("wg", "kwh")
wg <- lk70$wg
kwh <-lk70$kwh

test70 <- merge(t,lk70)
sum(test70$kwh)


lk71 <- data.frame(read.csv2(file = "VestasV1123075.csv", sep=";"))
colnames(lk71) <- c("wg", "kwh")
wg <- lk71$wg
kwh <-lk71$kwh

test71 <- merge(t,lk71)
sum(test71$kwh)


lk72 <- data.frame(read.csv2(file = "VestasV1123.3.csv", sep=";"))
colnames(lk72) <- c("wg", "kwh")
wg <- lk72$wg
kwh <-lk72$kwh

test72 <- merge(t,lk72)
sum(test72$kwh)


lk73 <- data.frame(read.csv2(file = "VestasV1123.45.csv", sep=";"))
colnames(lk73) <- c("wg", "kwh")
wg <- lk73$wg
kwh <-lk73$kwh

test73 <- merge(t,lk73)
sum(test73$kwh)


lk74 <- data.frame(read.csv2(file = "VestasV1173.3.csv", sep=";"))
colnames(lk74) <- c("wg", "kwh")
wg <- lk74$wg
kwh <-lk74$kwh

test74 <- merge(t,lk74)
sum(test74$kwh)


lk75 <- data.frame(read.csv2(file = "VestasV1173.45.csv", sep=";"))
colnames(lk75) <- c("wg", "kwh")
wg <- lk75$wg
kwh <-lk75$kwh

test75 <- merge(t,lk75)
sum(test75$kwh)


lk76 <- data.frame(read.csv2(file = "VestasV1173.6.csv", sep=";"))
colnames(lk76) <- c("wg", "kwh")
wg <- lk76$wg
kwh <-lk76$kwh

test76 <- merge(t,lk76)
sum(test76$kwh)


lk77 <- data.frame(read.csv2(file = "VestasV1263.0.csv", sep=";"))
colnames(lk77) <- c("wg", "kwh")
wg <- lk77$wg
kwh <-lk77$kwh

test77 <- merge(t,lk77)
sum(test77$kwh)


lk78 <- data.frame(read.csv2(file = "VestasV1263.3.csv", sep=";"))
colnames(lk78) <- c("wg", "kwh")
wg <- lk78$wg
kwh <-lk78$kwh

test78 <- merge(t,lk78)
sum(test78$kwh)


lk79 <- data.frame(read.csv2(file = "VestasV1263.45.csv", sep=";"))
colnames(lk79) <- c("wg", "kwh")
wg <- lk79$wg
kwh <-lk79$kwh

test79 <- merge(t,lk79)
sum(test79$kwh)


lk80 <- data.frame(read.csv2(file = "VestasV1363.45.csv", sep=";"))
colnames(lk80) <- c("wg", "kwh")
wg <- lk80$wg
kwh <-lk80$kwh

test80 <- merge(t,lk80)
sum(test80$kwh)


lk81 <- data.frame(read.csv2(file = "VestasV1364.04.2.csv", sep=";"))
colnames(lk81) <- c("wg", "kwh")
wg <- lk81$wg
kwh <-lk81$kwh

test81 <- merge(t,lk81)
sum(test81$kwh)


lk82 <- data.frame(read.csv2(file = "VestasV1504.2.csv", sep=";"))
colnames(lk82) <- c("wg", "kwh")
wg <- lk82$wg
kwh <-lk82$kwh

test82 <- merge(t,lk82)
sum(test82$kwh)


#Erbrachte Leistungen der Anlagen der letzten 364 Tage
#Speichern der Werte in x (Variable)

x <- c(
  sum(test1$kwh),sum(test2$kwh),sum(test3$kwh),sum(test4$kwh),sum(test5$kwh),
  sum(test6$kwh),sum(test7$kwh),sum(test8$kwh),sum(test9$kwh),sum(test10$kwh),
  sum(test11$kwh),sum(test12$kwh),sum(test13$kwh),sum(test14$kwh),sum(test15$kwh),
  sum(test16$kwh),sum(test17$kwh),sum(test18$kwh),sum(test19$kwh),sum(test20$kwh),
  sum(test21$kwh),sum(test22$kwh),sum(test23$kwh),sum(test24$kwh),sum(test25$kwh),
  sum(test26$kwh),sum(test27$kwh),sum(test28$kwh),sum(test29$kwh),sum(test30$kwh),
  sum(test31$kwh),sum(test32$kwh),sum(test33$kwh),sum(test34$kwh),sum(test35$kwh),
  sum(test36$kwh),sum(test37$kwh),sum(test38$kwh),sum(test39$kwh),sum(test40$kwh),
  sum(test41$kwh),sum(test42$kwh),sum(test43$kwh),sum(test44$kwh),sum(test45$kwh),
  sum(test46$kwh),sum(test47$kwh),sum(test48$kwh),sum(test49$kwh),sum(test50$kwh),
  sum(test51$kwh),sum(test52$kwh),sum(test53$kwh),sum(test54$kwh),sum(test55$kwh),
  sum(test56$kwh),sum(test57$kwh),sum(test58$kwh),sum(test59$kwh),sum(test60$kwh),
  sum(test61$kwh),sum(test62$kwh),sum(test63$kwh),sum(test64$kwh),sum(test65$kwh),
  sum(test66$kwh),sum(test67$kwh),sum(test68$kwh),sum(test69$kwh),sum(test70$kwh),
  sum(test71$kwh),sum(test72$kwh),sum(test73$kwh),sum(test74$kwh),sum(test75$kwh),
  sum(test76$kwh),sum(test77$kwh),sum(test78$kwh),sum(test79$kwh),sum(test80$kwh),
  sum(test81$kwh),sum(test82$kwh)
)


#Nutzungsgrad der Anlagen auf Basis der Leistung der letzten 365 Tage
#Rechnung n = (Gesamte Leistung des Jahres / (Nennleistung*24h*365) entsprechend der theoretischen Leistung bei maximaler Leistung in jeder Stunde)*100 
#Der Wert 0 weist darauf hin, dass der Nutzungsgrad von R nicht ausgerechnet werden konnte. Die Anlagen wurden trotzdem in die Tabelle aufgenommen um gleiche Zeilenanzahl zu gewährleisten
#Speichern der Werte in y (Variable)

y <-c(
  sum((test1$kwh)/(max(lk1$kwh)*24*365)*100),
  sum((test2$kwh)/(max(lk2$kwh)*24*365)*100),
  sum((test3$kwh)/(max(lk3$kwh)*24*365)*100),
  sum((test4$kwh)/(max(lk4$kwh)*24*365)*100),
  sum((test5$kwh)/(max(lk5$kwh)*24*365)*100),
  sum((test6$kwh)/(max(lk6$kwh)*24*365)*100),
  sum((test7$kwh)/(max(lk7$kwh)*24*365)*100),
  sum((test8$kwh)/(max(lk8$kwh)*24*365)*100),
  sum((test9$kwh)/(max(lk9$kwh)*24*365)*100),
  sum((test10$kwh)/(max(lk10$kwh)*24*365)*100),
  sum((test11$kwh)/(max(lk11$kwh)*24*365)*100),
  sum((test12$kwh)/(max(lk12$kwh)*24*365)*100),
  sum((test13$kwh)/(max(lk13$kwh)*24*365)*100),
  sum((test14$kwh)/(max(lk14$kwh)*24*365)*100),
  sum((test15$kwh)/(max(lk15$kwh)*24*365)*100),
  sum((test16$kwh)/(max(lk16$kwh)*24*365)*100),
  sum((test17$kwh)/(max(lk17$kwh)*24*365)*100),
  sum((test18$kwh)/(max(lk18$kwh)*24*365)*100),
  sum((test19$kwh)/(max(lk19$kwh)*24*365)*100),
  sum((test20$kwh)/(max(lk20$kwh)*24*365)*100),
  sum((test21$kwh)/(max(lk21$kwh)*24*365)*100),
  sum((test22$kwh)/(max(lk22$kwh)*24*365)*100),
  sum((test23$kwh)/(max(lk23$kwh)*24*365)*100),
  sum((test24$kwh)/(max(lk24$kwh)*24*365)*100),
  sum((test25$kwh)/(max(lk25$kwh)*24*365)*100),
  sum((test26$kwh)/(max(lk26$kwh)*24*365)*100),
  sum((test27$kwh)/(max(lk27$kwh)*24*365)*100),
  sum((test28$kwh)/(max(lk28$kwh)*24*365)*100),
  sum((test29$kwh)/(max(lk29$kwh)*24*365)*100),
  sum((test30$kwh)/(max(lk30$kwh)*24*365)*100),
  sum((test31$kwh)/(max(lk31$kwh)*24*365)*100),
  sum((test32$kwh)/(max(lk32$kwh)*24*365)*100),
  sum((test33$kwh)/(max(lk33$kwh)*24*365)*100),
  0,
  #sum(test34$kwh)/(max(lk34$kwh)*24*365)
  0,
  #sum(test35$kwh)/(max(lk35$kwh)*24*365)
  sum((test36$kwh)/(max(lk36$kwh)*24*365)*100),
  sum((test37$kwh)/(max(lk37$kwh)*24*365)*100),
  sum((test38$kwh)/(max(lk38$kwh)*24*365)*100),
  sum((test39$kwh)/(max(lk39$kwh)*24*365)*100),
  sum((test40$kwh)/(max(lk40$kwh)*24*365)*100),
  0,
  #sum(test41$kwh)/(max(lk41$kwh)*24*700)
  sum((test42$kwh)/(max(lk42$kwh)*24*365)*100),
  0,
  #sum(test43$kwh)/(max(lk43$kwh)*24*700)
  sum((test44$kwh)/(max(lk44$kwh)*24*365)*100),
  sum((test45$kwh)/(max(lk45$kwh)*24*365)*100),
  sum((test46$kwh)/(max(lk46$kwh)*24*365)*100),
  sum((test47$kwh)/(max(lk47$kwh)*24*365)*100),
  sum((test48$kwh)/(max(lk48$kwh)*24*365)*100),
  sum((test49$kwh)/(max(lk49$kwh)*24*365)*100),
  sum((test50$kwh)/(max(lk50$kwh)*24*365)*100),
  sum((test51$kwh)/(max(lk51$kwh)*24*365)*100),
  sum((test52$kwh)/(max(lk52$kwh)*24*365)*100),
  sum((test53$kwh)/(max(lk53$kwh)*24*365)*100),
  sum((test54$kwh)/(max(lk54$kwh)*24*365)*100),
  sum((test55$kwh)/(max(lk55$kwh)*24*365)*100),
  sum((test56$kwh)/(max(lk56$kwh)*24*365)*100),
  sum((test57$kwh)/(max(lk57$kwh)*24*365)*100),
  sum((test58$kwh)/(max(lk58$kwh)*24*365)*100),
  sum((test59$kwh)/(max(lk59$kwh)*24*365)*100),
  sum((test60$kwh)/(max(lk60$kwh)*24*365)*100),
  sum((test61$kwh)/(max(lk61$kwh)*24*365)*100),
  sum((test62$kwh)/(max(lk62$kwh)*24*365)*100),
  sum((test63$kwh)/(max(lk63$kwh)*24*365)*100),
  sum((test64$kwh)/(max(lk64$kwh)*24*365)*100),
  sum((test65$kwh)/(max(lk65$kwh)*24*365)*100),
  sum((test66$kwh)/(max(lk66$kwh)*24*365)*100),
  sum((test67$kwh)/(max(lk67$kwh)*24*365)*100),
  sum((test68$kwh)/(max(lk68$kwh)*24*365)*100),
  sum((test69$kwh)/(max(lk69$kwh)*24*365)*100),
  sum((test70$kwh)/(max(lk70$kwh)*24*365)*100),
  sum((test71$kwh)/(max(lk71$kwh)*24*365)*100),
  sum((test72$kwh)/(max(lk72$kwh)*24*365)*100),
  sum((test73$kwh)/(max(lk73$kwh)*24*365)*100),
  sum((test74$kwh)/(max(lk74$kwh)*24*365)*100),
  sum((test75$kwh)/(max(lk75$kwh)*24*365)*100),
  sum((test76$kwh)/(max(lk76$kwh)*24*365)*100),
  sum((test77$kwh)/(max(lk77$kwh)*24*365)*100),
  sum((test78$kwh)/(max(lk78$kwh)*24*365)*100),
  sum((test79$kwh)/(max(lk79$kwh)*24*365)*100),
  sum((test80$kwh)/(max(lk80$kwh)*24*365)*100),
  sum((test81$kwh)/(max(lk81$kwh)*24*365)*100),
  sum((test82$kwh)/(max(lk82$kwh)*24*365)*100)
)


#Speichern der errechneten Werten in WKA als ergaenzende Spalten (Name WKA, Leistung, Nutzungsgrad)
WKA <- cbind(WKA,x,y)
colnames(WKA) <- c("WKA", "Leistung [kW]","Nutzungsgrad [%]") #Aenderung Spaltenbezeichnung 


#Loeschen unnuetzer Variablen aus Global Enviornment
rm(lk1,lk2,lk3,lk4,lk5,lk6,lk7,lk8,lk9,lk10,lk11,lk12,lk13,lk14,lk15,lk16,lk17,lk18,lk19,lk20,lk21,lk22,lk23,lk24,lk25,lk26,lk27,lk28,lk29,lk30,lk31,lk32,lk33,lk34,lk35,lk36,lk37,lk38,lk39,lk40,lk41,lk42,lk43,lk44,lk45,lk46,lk47,lk48,lk49,lk50,lk51,lk52,lk53,lk54,lk55,lk56,lk57,lk58,lk59,lk60,lk61,lk62,lk63,lk64,lk65,lk66,lk67,lk68,lk69,lk70,lk71,lk72,lk73,lk74,lk75,lk76,lk77,lk78,lk79,lk80,lk81,lk82)
rm(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25,test26,test27,test28,test29,test30,test31,test32,test33,test34,test35,test36,test37,test38,test39,test40,test41,test42,test43,test44,test45,test46,test47,test48,test49,test50,test51,test52,test53,test54,test55,test56,test57,test58,test59,test60,test61,test62,test63,test64,test65,test66,test67,test68,test69,test70,test71,test72,test73,test74,test75,test76,test77,test78,test79,test80,test81,test82)
rm(t,wg,x,y,kwh)

#-----------------------------------------------------------------------

## Flaechenbedarf und Turbinendichte

#Platzbedarf laut dena Netzstudie: 7,54 ha/MW
#Zugrunde liegende Rechnung Flaechenbedarf: 1 MW = 0,0754 km2 <-> 14,2857 MW = 1 km2 <-> 14.285,4 kW/km2
#Beispiel Anlage Enercon E 70: Flaechenbedarf / Nennleistung = 14.295,4 / 4.800 = 3 Turbinen / km2


#Einlesen der Angaben zu Nennleistung und Rotordurchmesser der WKA
#namen (data frame) mit Name WKA, Nennleistung, Rotordurchmesser, Nabenhoehe
namen <- read.csv2(file = "0_WKA_Messzahlen.csv", sep =";") 

namen = namen[,-c(4,6,7,8)] #L?schen unn?tzer Spalten

namen <- namen[1:82,] #L?schen Zeilen NA's


#Turbinendichte ausrechnen
#Speichern der Werte in dichte (Variable)
dichte <- (14295.4)/(namen$nennleistung)
dichte <- round(dichte) #Werte runden


#Errechnete Turbinendichte und Gesamtleistung im Verbund in Data Frame speichern
#turbinendichte (data frame) mit rotor + zusätzlicher Spalte mit Anzahl Turbinen / km^2

turbinendichte <- cbind.data.frame(WKA$WKA, dichte)
colnames(turbinendichte) <- c("WKA", "Dichte [Anlagen / km^2]")

lv <- WKA$`Leistung [kW]`*turbinendichte$`Dichte [Anlagen / km^2]`

#lv = Leistung Verbund (data frame) mit Name WKA, Rotordurchmesser, Turbinendichte, Gesamtleistung im Verbund
lv <- cbind.data.frame(turbinendichte,lv)
colnames(lv) <- c("WKA", "Dichte [Anlagen / km^2]", "Leistung [kW]") 

colnames(namen) <- c("WKA", "Rotordurchmesser [m]", "Nennleistung [kWh]", "Nabenhoehe [m]")


install.packages("xtable")
library(xtable)

  xtable(namen)
  xtable(WKA)
  xtable(lv)

#-----------------------------------------------------------------------

#Ertragsrechnung Einzelanlage
#e (Variable) = ertrag, jahr = pro Jahr, ges = Gesamte Lebensdauer
#Werte Verguetung: min 3.80 [c/kWh], max 6,30 [c/kWh], mittel 5,67 [ct/kWh]

e_min_jahr <- WKA$`Leistung [kW]`*0.038
e_max_jahr <- WKA$`Leistung [kW]`*0.063
e_d_jahr <- WKA$`Leistung [kW]`*0.0567

#Hochrechnung auf 20 Jahre
e_min_ges <- e_min_jahr*20
e_max_ges <- e_max_jahr*20
e_d_ges <- e_d_jahr*20

#ertrag (data frame) mit Name WKA, minimale/maximal/durchschnitt Verguetung pro Jahr und Gesamt der Einzelanlagen
ertrag = cbind.data.frame(WKA$WKA, e_min_jahr, e_max_jahr, e_d_jahr, e_min_ges, e_max_ges, e_d_ges)
colnames(ertrag) <- c("WKA", "Min Ertrag/Jahr [EUR]", "Max Ertrag/Jahr [EUR]", "Durchschnitt Ertrag/ Jahr [EUR]", "Min Gesamt [EUR]", "Max Gesamt [EUR]", "Durschnitt Gesamt [EUR]") #Änderung Spaltenbezeichnung 


#Ertragsrechnung der Anlagen im Verbund
#Kennzeichung ev (Variable) = Verbund

e_min_jahrV <- lv$`Leistung [kW]`*0.038
e_max_jahrV <- lv$`Leistung [kW]`*0.063
e_d_jahrV <- lv$`Leistung [kW]`*0.0567

#ertrag (data frame) mit Name WKA, minimale/maximal/durchschnitt Verguetung pro Jahr der Anlagen im Verbund
ertragV = cbind.data.frame(WKA$WKA, turbinendichte$`Dichte [Anlagen / km^2]`, e_min_jahrV, e_max_jahrV, e_d_jahrV)
colnames(ertragV) <- c("WKA", "Dichte[ Anzahl pro km^2]", "Min Ertrag/Jahr [EUR]", "Max Ertrag/Jahr [EUR]", "Durchschnitt Ertrag/ Jahr [EUR]") #Änderung Spaltenbezeichnung 


#Hochrechnung auf 20 Jahre
e_min_gesV <- e_min_jahrV*20
e_max_gesV <- e_max_jahrV*20
e_d_gesV <- e_d_jahrV*20

#ertrag (data frame) mit Name WKA, minimale/maximal/durchschnitt Verguetung Gesamt der Anlagen im Verbund
ertragV2 = cbind.data.frame(WKA$WKA, e_min_gesV, e_max_gesV, e_d_gesV)
colnames(ertragV2) <- c("WKA", "Min Gesamt [EUR]", "Max Gesamt [EUR]", "Durschnitt Gesamt [EUR]") #Aenderung Spaltenbezeichnung 

  xtable(ertrag)
  xtable(ertragV)
  xtable(ertragV2)

#-----------------------------------------------------------------------

#Kostenrechnung Einzelanlagen
#Nominale Investitionskosten: min 1100 [EUR/kW] max 1500 [EUR/kW] zzgl. Rueckbaukosten von 5%

#k (Variable) = kosten
k_min <- namen$`Nennleistung [kWh]`*1100*1.05 
k_max <- namen$`Nennleistung [kWh]`*1500*1.05

#kosten (data frame) mit Name WKA, Nennleistung, Leistung/Jahr, Minimalkosten, Maximalkosten der Einzelanlage
kosten = cbind.data.frame(WKA$WKA, namen$`Nennleistung [kWh]`, WKA$`Leistung [kW]`, k_min, k_max) #Tabelle mit Kosten
colnames(kosten) <- c("WKA", "Nennleistung [kWh]", "Leistung [kW]", "Kosten Min [EUR]", "Kosten Max [EUR]") #Aenderung Spaltenbezeichnung 

  
#Betriebskosten in der ersten Dekade 1.5 \%, in der zweiten Dekade 2.5 \% je Kosten min und max der Einzelanlage
#b (Variable) = Betriebskosten, 1 = 1. Dekade, 2 = 2. Dekade je Kosten min und max der Einzelanlage
b1_min <- k_min*0.015
b2_min <- k_min*0.025

b1_max <- k_max*0.015
b2_max <- k_max*0.025

#bk (data frame) mit Name WKA, Leistung/Jahr, Betriebskosten 1 & 2 je Kosten min und max der Einzelanlage
bk = cbind.data.frame(WKA$WKA, b1_min, b2_min, b1_max, b2_max) 
colnames(bk) <- c("WKA", "BK min 1. Dekade [EUR]", "BK min 2. Dekade [EUR]", "BK max 1. Dekade [EUR]", "BK max 2. Dekade [EUR]")

#Zusammenrechnung der Betriebskosten Ueber Lebensdauer von 20 Jahren

bges_min <- b1_min*10+b2_min*10
bges_max <- b1_max*10+b2_max*10

#bkges (data frame) mit Name WKA, Leistung/Jahr, Betriebskosten Insgesamt je Kosten min und max der Einzelanlage
bkges = cbind.data.frame(WKA$WKA, bges_min, bges_max) #Tabelle mit Betriebskosten / Lebensdauer
colnames(bkges) <- c("WKA", "BK Gesamt min [EUR]", "BK Gesamt max [EUR]")

  xtable(kosten)
  xtable(bk)
  xtable(bkges)


#Kostenrechnung der Anlagen im Verbund
#Kennzeichung ev (Variable) = Verbund

k_minV <- (namen$`Nennleistung [kWh]`*1100*1.05)*turbinendichte$`Dichte [Anlagen / km^2]` #Hinzurechnung der Anzahl der Turbinen / km^2
k_maxV <- (namen$`Nennleistung [kWh]`*1500*1.05)*turbinendichte$`Dichte [Anlagen / km^2]`

#kosten (data frame) mit Name WKA, Nennleistung, Gesamtleistung/Jahr, Minimalkosten, Maximalkosten der Kosten im Verbund 
kostenV = cbind.data.frame(WKA$WKA, namen$`Nennleistung [kWh]`, lv$`Leistung [kW]`, k_minV, k_maxV) #Tabelle mit Gesamtkosten
colnames(kostenV) <- c("WKA", "Nennleistung [kW]", "Leistung [kW]", "Kosten min [EUR]", "Kosten max [EUR]")


#Betriebskosten in der ersten Dekade 1.5 \%, in der zweiten Dekade 2.5 \% je Kosten min und max der Anlagen im Verbund
#b (Variable) = Betriebskosten, 1 = 1. Dekade, 2 = 2. Dekade je Kosten min und max der Anlagen im Verbund 

b1_minV <- k_minV*0.015
b2_minV <- k_minV*0.025

b1_maxV <- k_maxV*0.015
b2_maxV <- k_maxV*0.025


#bk (data frame) mit Name WKA, Leistung/Jahr, Betriebskosten 1 & 2 je KOsten min und max der Anlagen im Verbund
bkV = cbind.data.frame(WKA$WKA, b1_minV, b2_minV, b1_maxV, b2_maxV)
colnames(bkV) <- c("WKA", "BK min 1. Dekade [EUR]", "BK min 2. Dekade [EUR]", "BK max 1. Dekade [EUR]", "BK max 2. Dekade [EUR]")


#Zusammenrechnung der Betriebskosten ueber Lebensdauer von 20 Jahren

bges_minV <- b1_minV*10+b2_minV*10
bges_maxV <- b1_maxV*10+b2_maxV*10

#bkges (data frame) mit Name WKA, Leistung/Jahr, Betriebskosten Insgesamt je Kosten min und max der Anlagen im Verbund
bkgesV = cbind.data.frame(WKA$WKA, bges_minV, bges_maxV) #Tabelle mit Betriebskosten / Lebensdauer
colnames(bkgesV) <- c("WKA", "BK Gesamt min [EUR]", "BK Gesamt max [EUR]")

  xtable(kostenV)
  xtable(bkV)
  xtable(bkgesV)


#-----------------------------------------------------------------------

#Gewinnvergleichsrechnung (CashFlow) 
#Gewinnvergleich ueber 20 Jahre Einzelanlagen
#Ermittlung des Gewinn / Anlage

A <- kosten$`Kosten Min [EUR]` + bkges$`BK Gesamt min [EUR]` #Gesamte Kosten fuer Minimalkosten
B <- kosten$`Kosten Max [EUR]` + bkges$`BK Gesamt max [EUR]` #Gesamte Kosten fuer Maximalkosten

cashflow_minA <- ertrag$`Min Gesamt [EUR]` - A
cashflow_minB <- ertrag$`Min Gesamt [EUR]` - B

cashflow_maxA <- ertrag$`Max Gesamt [EUR]` - A 
cashflow_maxB <- ertrag$`Max Gesamt [EUR]` - B

cashflow_dA <- ertrag$`Durschnitt Gesamt [EUR]` - A
cashflow_dB <- ertrag$`Durschnitt Gesamt [EUR]` - B

#Rentabilitaet
#Rentabliitaet ueber 20 Jahre Einzelanlagen
# r = Gewinn / Kapital * 100 
# Errechnende Werte sind Prozent

r1A <- (ertrag$`Min Gesamt [EUR]` / A)*100
r1B <- (ertrag$`Min Gesamt [EUR]` / B)*100

r2A <- (ertrag$`Max Gesamt [EUR]` / A)*100
r2B <- (ertrag$`Max Gesamt [EUR]` / B)*100

r3A <- (ertrag$`Durschnitt Gesamt [EUR]` / A)*100
r3B <- (ertrag$`Durschnitt Gesamt [EUR]` / B)*100


#Szenario 1 
s1 = cbind.data.frame(WKA$WKA, cashflow_minA, r1A)

s1$cf_K_min <- ifelse(s1$cashflow_minA<0, "loose", "win")
colnames(s1) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 2 
s2 = cbind.data.frame(WKA$WKA, cashflow_minB, r1B)

s2$cf_K_max <- ifelse(s2$cashflow_minB<0, "loose", "win")
colnames(s2) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 3
s3 = cbind.data.frame(WKA$WKA, cashflow_maxA, r2A)

s3$cf_K_min <- ifelse(s3$cashflow_maxA<0, "loose", "win")
colnames(s3) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 4 
s4 = cbind.data.frame(WKA$WKA, cashflow_maxB, r2B)

s4$cf_K_max <- ifelse(s4$cashflow_maxB<0, "loose", "win")
colnames(s4) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 5 
s5 = cbind.data.frame(WKA$WKA, cashflow_dA, r3A)

s5$cf_K_min <- ifelse(s5$cashflow_dA<0, "loose", "win")
colnames(s5) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 6
s6 = cbind.data.frame(WKA$WKA, cashflow_dB, r3B)

s6$cf_K_max <- ifelse(s6$cashflow_dB<0, "loose", "win")
colnames(s6) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

  xtable(s1)
  xtable(s2)
  xtable(s3)
  xtable(s4)
  xtable(s5)
  xtable(s6)

#Gewinnvergleichs-, und Rentabilitätsrechnung über 20 Jahre der Anlagen im Verbund 
#Ermittlung des Gewinn / km^2
#V = Verbund (Variable)


AV <- kostenV$`Kosten min [EUR]` + bkgesV$`BK Gesamt min [EUR]` #Gesamte Kosten fuer Minimalkosten 
BV <- kostenV$`Kosten max [EUR]`+ bkgesV$`BK Gesamt max [EUR]` #Gesamte Kosten fuer Maximalkosten

cashflow_minAV <- ertragV2$`Min Gesamt [EUR]` - AV
cashflow_minBV <- ertragV2$`Min Gesamt [EUR]` - BV

cashflow_maxAV <- ertragV2$`Max Gesamt [EUR]` - AV 
cashflow_maxBV <- ertragV2$`Max Gesamt [EUR]` - BV

cashflow_dAV <- ertragV2$`Durschnitt Gesamt [EUR]` - AV
cashflow_dBV <- ertragV2$`Durschnitt Gesamt [EUR]` - BV

#Rentabilitaet
#Rentabliitaet ueber 20 Jahre der Anlagen im Verbund
#r = Gewinn / Kapital * 100 
#Errechnende Werte sind Prozent

r1AV <- (ertragV2$`Min Gesamt [EUR]` / AV)*100
r1BV <- (ertragV2$`Min Gesamt [EUR]` / BV)*100

r2AV <- (ertragV2$`Max Gesamt [EUR]` / AV)*100
r2BV <- (ertragV2$`Max Gesamt [EUR]` / BV)*100

r3AV <- (ertragV2$`Durschnitt Gesamt [EUR]` / AV)*100
r3BV <- (ertragV2$`Durschnitt Gesamt [EUR]` / BV)*100


#Szenario 1 V
s1V = cbind.data.frame(WKA$WKA, cashflow_minAV, r1AV)

s1V$cf_K_min <- ifelse(s1V$cashflow_minAV<0, "loose", "win")
colnames(s1V) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 2 V
s2V = cbind.data.frame(WKA$WKA, cashflow_minBV, r1BV)

s2V$cf_K_max <- ifelse(s2V$cashflow_minBV<0, "loose", "win")
colnames(s2V) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 3 V
s3V = cbind.data.frame(WKA$WKA, cashflow_maxAV, r2AV)

s3V$cf_K_min <- ifelse(s3V$cashflow_maxAV<0, "loose", "win")
colnames(s3V) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 4 V
s4V = cbind.data.frame(WKA$WKA, cashflow_maxBV, r2BV)

s4V$cf_K_max <- ifelse(s4V$cashflow_maxBV<0, "loose", "win")
colnames(s4V) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 5 V
s5V = cbind.data.frame(WKA$WKA, cashflow_dAV, r3AV)

s5V$cf_K_min <- ifelse(s5V$cashflow_dAV<0, "loose", "win")
colnames(s5V) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

#Szenario 6 V
s6V = cbind.data.frame(WKA$WKA, cashflow_dBV, r3BV)

s6V$cf_K_max <- ifelse(s6V$cashflow_dBV<0, "loose", "win")
colnames(s6V) <- c("WKA", "Gewinn [EUR]", "Rentabilitaet [%]", "Rentabel?")

  xtable(s1V)
  xtable(s2V)
  xtable(s3V)
  xtable(s4V)
  xtable(s5V)
  xtable(s5V)

#-----------------------------------------------------------------------

#Amortisationsrechnung 

#1.Schritt: Durschnittliche Betriebskosten errechnen
bn1 <- (b1_min+b2_min)/2
bn2 <- (b1_max+b2_max)/2 

#2.Schritt: Abzug der neuen Betriebskosten von den Jahreserträgen
#3.Schritt: Formel = Kosten / Ertrag (pro Jahr) 

#Szenario 1: Verguetung von 3,80 [ct/kWh] bei Kosten von 1100 [EUR/kW] 
a1 <- e_min_jahr - bn1
a1n <- k_min / a1 
a1n <-round(a1n)

#Szenario 2: Verguetung von 3,80 [ct/kWh] bei Kosten von 1500 [EUR/kW]
a2 <- e_min_jahr - bn2
a2n <- k_max / a2 
a2n <-round(a2n)

#Szenario 3: Verguetung von 6,30 [ct/kWh] bei Kosten von 1100 [EUR/kW]
a3 <- e_max_jahr - bn1
a3n <- k_min / a3
a3n <- round(a3n)

#Szenario 4: Verguetung von 6,30 [ct/kWh] bei Kosten von 1500 [EUR/kW]
a4 <- e_max_jahr - bn2
a4n <- k_max / a4
a4n <-round(a4n)

#Szenario 5: Verguetung von 5,67 [ct/kWh] bei Kosten von 1100 [EUR/kW]
a5 <- e_d_jahr - bn1
a5n <- k_min / a5
a5n <- round(a5n)

#Szenario 6: Verguetung von 5,67 [ct/kWh] bei Kosten von 1500 [EUR/kW]
a6 <- e_d_jahr - bn2
a6n <- k_max / a6
a6n <- round(a6n)


a <- cbind.data.frame(WKA$WKA, a1n, a2n, a3n, a4n, a5n, a6n)
colnames(a) <- c("WKA", "Amortisatation S1 [Jahre]", "Amortisatation S2 [Jahre]", "Amortisatation S3 [Jahre]", "Amortisatation S4 [Jahre]","Amortisatation S5 [Jahre]", "Amortisatation S6 [Jahre]")

#Loeschen unrentabler WKA aus data frame
a <- a [- c(34, 35, 41, 43),]
a = data.frame(a)

  xtable(a)


#Amortisation der Anlagen im Verbund

bn1V <- (b1_minV+b2_minV)/2
bn2V <- (b1_maxV+b2_maxV)/2 

#2.Schritt: Abzug der neuen Betriebskosten von den Jahreserträgen
#3.Schritt: Formel = Kosten / Ertrag (pro Jahr) 

#Szenario 1: Verguetung von 3,80 [ct/kWh] bei Kosten von 1100 [EUR/kW] 
a1V <- e_min_jahrV - bn1V
a1nV <- k_minV / a1V
a1nV <-round(a1nV)

#Szenario 2: Verguetung von 3,80 [ct/kWh] bei Kosten von 1500 [EUR/kW]
a2V <- e_min_jahrV - bn2V
a2nV <- k_maxV / a2V
a2nV <-round(a2nV)

#Szenario 3: Verguetung von 6,30 [ct/kWh] bei Kosten von 1100 [EUR/kW]
a3V <- e_max_jahrV - bn1V
a3nV <- k_minV / a3V
a3nV <- round(a3nV)

#Szenario 4: Vergütung von 6,30 [ct/kWh] bei Kosten von 1500 [EUR/kW]
a4V <- e_max_jahrV - bn2V
a4nV <- k_maxV / a4V
a4nV <-round(a4nV)

#Szenario 5: Verguetung von 5,67 [ct/kWh] bei Kosten von 1100 [EUR/kW]
a5V <- e_d_jahrV - bn1V
a5nV <- k_minV / a5V
a5nV <- round(a5nV)

#Szenario 6: Vergütung von 5,67 [ct/kWh] bei Kosten von 1500 [EUR/kW]
a6V <- e_d_jahrV - bn2V
a6nV <- k_maxV / a6V
a6nV <- round(a6nV)


aV <- cbind.data.frame(WKA$WKA, a1nV, a2nV, a3nV, a4nV, a5nV, a6nV)
colnames(aV) <- c("WKA", "Amortisatation S1 [Jahre]", "Amortisatation S2 [Jahre]", "Amortisatation S3 [Jahre]", "Amortisatation S4 [Jahre]","Amortisatation S5 [Jahre]", "Amortisatation S6 [Jahre]")


#Loeschen unrentabler WKA aus data frame
aV <- aV [- c(34, 35, 41, 43),]
aV = data.frame(aV)

  xtable(aV)

#-----------------------------------------------------------------------

#Ergebnisse aus R-Projektseminar zur Leistung der installierten Anlage E70 (Werte zu Vergleichszwecken uebernommen)
#Ertrags-, Kosten-, Gewinn-, Rentabilitaets-, Amortisationsrechnung analog (s.o.)

lk <- data.frame(read.csv2(file = "A_E70.csv", sep=";"))
colnames(lk) <- c("wg", "kwh")

wg <- lk$wg
kwh <-lk$kwh
t <- data.frame(round(wind$F))
colnames(t) <- c("wg")

#Merge gerundete Windgeschwindigkeiten und Werte Leistungskurve
tlk <- merge(t,lk)
sum(tlk$kwh) #Gesamte Leistung E70 im Jahr 2017
#[1] 5267322
sum((tlk$kwh)/(max(tlk$kwh)*24*365)*100) #Nutzunggrad E70
#[1] 26.02998
sum(tlk$kwh)*3 #Gesamtleistung fuer 3 Turbinen / km^2
#[1] 15801966

#Ertr?ge E 70

#e70_min_jahr
sum(tlk$kwh)*0.038

#e70_max_jahr
sum(tlk$kwh)*0.063

#e70_d_jahr
sum(tlk$kwh)*0.0567


#e70_min_ges
(sum(tlk$kwh)*0.038)*20

#e70_max_ges
(sum(tlk$kwh)*0.063)*20

#e70_d_ges
(sum(tlk$kwh)*0.0567)*20


#e70_min_jahrV
sum(tlk$kwh)*0.038*3

#e70_max_jahrV
sum(tlk$kwh)*0.063*3

#e70_d_jahrV
sum(tlk$kwh)*0.0567*3

#e70_min_gesV
(sum(tlk$kwh)*0.038)*20*3

#e70_max_gesV
(sum(tlk$kwh)*0.063)*20*3

#e70_d_gesV
(sum(tlk$kwh)*0.0567)*20*3


#ke70_min
max(tlk$kwh)*1100*1.05 
#ke70_max
max(tlk$kwh)*1500*1.05

#b1e70_min
max(tlk$kwh)*1100*1.05 *0.015
#b2e70_min
max(tlk$kwh)*1100*1.05 *0.025

#b1_max 
max(tlk$kwh)*1500*1.05 *0.015
#b2_max 
max(tlk$kwh)*1500*1.05 *0.025

#bges_min 
(max(tlk$kwh)*1100*1.05 *0.015)*10+(max(tlk$kwh)*1100*1.05 *0.025)*10
#bges_max 
(max(tlk$kwh)*1500*1.05 *0.015)*10+(max(tlk$kwh)*1500*1.05 *0.025)*10


A <- max(tlk$kwh)*1100*1.05  + (max(tlk$kwh)*1100*1.05 *0.015)*10+(max(tlk$kwh)*1100*1.05 *0.025)*10
B <- max(tlk$kwh)*1500*1.05 + (max(tlk$kwh)*1500*1.05 *0.015)*10+(max(tlk$kwh)*1500*1.05 *0.025)*10

#cashflow_minA
(sum(tlk$kwh)*0.038)*20 - A
#cashflow_minB
(sum(tlk$kwh)*0.038)*20 - B

#cashflow_maxA 
(sum(tlk$kwh)*0.063)*20 - A 
#cashflow_maxB
(sum(tlk$kwh)*0.063)*20 - B

#cashflow_dA
(sum(tlk$kwh)*0.0567)*20 - A
#cashflow_dB
(sum(tlk$kwh)*0.0567)*20 - B

#r1A 
(((sum(tlk$kwh)*0.038)*20) / A)*100
#r1B
(((sum(tlk$kwh)*0.038)*20) / B)*100

#r2A 
(((sum(tlk$kwh)*0.063)) / A)*100
#r2B
(((sum(tlk$kwh)*0.063)) / B)*100

#r3A 
(((sum(tlk$kwh)*0.0567)) / A)*100
#r3B
(((sum(tlk$kwh)*0.0567)) / B)*100

#-----------------------------------------------------------------------
#Ertrag und Gewinn in Abhaengigkeit von den mittleren Windgeschwindigkeiten
#Die Leistung [kWh/Jahr] wurde mit einem Ertragsrechner https://wind-data.ch/tools/powercalc.php auf Basis der Weibull-Verteilung mit k=2 errechnet
#Die zugehoerigen Werte zu den jeweiligen, ganzzahligen Windgeschwindigkeiten wurden manuell in Excel eingetragen

library(ggplot2) 
library(scales)

ge <- read.csv2(file = "ge.csv", sep=";") #Einlesen der Ergebnisse aus dem Ertragsrechner
colnames(ge) <- c("Windgeschwindigkeit [m/s]", "Leistung [kWh]/Jahr")

#Plotten des Leistungsverlaufes
ggplot(ge) +
  geom_line(data=ge, aes(x=ge$`Windgeschwindigkeit [m/s]`, y=ge$`Leistung [kWh]/Jahr`))+
  labs(title="Ertraege der GE Wind 4.8 158 ", x= "Windgeschwindigkeit [m/s]", y= "Leistung [kw/h]/Jahr")+
  scale_y_continuous(labels = comma, breaks = seq(0, 30000000, by = 2500000))+
  scale_x_continuous(breaks=1:18)+
  geom_vline(xintercept=6.85, colour="#FF9999")


siemens <- read.csv2(file = "siemens.csv", sep=";") #Einlesen der Ergebnisse aus dem Ertragsrechner
colnames(siemens) <- c("Windgeschwindigkeit [m/s]", "Leistung [kWh]/Jahr")
verbund <- siemens$`Leistung [kWh]/Jahr`*5
siemens = data.frame(siemens$`Windgeschwindigkeit [m/s]`, verbund)
colnames(siemens) <- c("Windgeschwindigkeit [m/s]", "Leistung [kWh]/Jahr")

#Plotten des Leistungsverlaufes
ggplot(siemens) +
  geom_line(data=siemens, aes(x=siemens$`Windgeschwindigkeit [m/s]`, y=siemens$`Leistung [kWh]/Jahr`))+
  labs(title="Ertraege der Siemens SWT 3.15 142", x= "Windgeschwindigkeit [m/s]", y= "Leistung [kw/h]/Jahr")+
  scale_y_continuous(labels = comma, breaks = seq(0, 60000000, by = 5000000))+
  scale_x_continuous(breaks=1:18)+
  geom_vline(xintercept=6.85, colour="#FF9999")


#Berechnung der Ertraege analog (s.o.)

#Ertraege GE Wind 4.8 158

ewming <- ge$`Leistung [kWh]/Jahr`*0.038
ewmaxg <- ge$`Leistung [kWh]/Jahr`*0.063
ewdg <- ge$`Leistung [kWh]/Jahr`*0.0567

ewg = data.frame(ge$`Windgeschwindigkeit [m/s]`, ewming, ewmaxg, ewdg)


ggplot(ewg, aes(ge$`Windgeschwindigkeit [m/s]`, colour = variable)) +
  geom_line(aes(y = ewg$ewming, colour ="Verguetung 3.80 [ct/kWh]"))+
  geom_line(aes(y = ewg$ewmaxg, colour = "Verguetung 6.30 [ct/kWh]"))+
  geom_line(aes(y = ewg$ewdg, colour = "Verguetung 5.67 [ct/kWh]"))+
  labs(title="Ertraege der GE Wind 4.8 158", x= "Windgeschwindigkeit [m/s]", y= "Ertrag [EUR]/Jahr")+
  scale_y_continuous(labels=dollar_format(prefix="€"), breaks = seq(0, 2000000, by = 50000))+
  scale_x_continuous(breaks=1:18)+
  geom_vline(xintercept=6.85)+
  theme(legend.position="bottom")


#Ertraege Siemens SWT 3.15 142

ewmins <- siemens$`Leistung [kWh]/Jahr`*0.038
ewmaxs <- siemens$`Leistung [kWh]/Jahr`*0.063
ewds <- siemens$`Leistung [kWh]/Jahr`*0.0567

ews = data.frame(siemens$`Windgeschwindigkeit [m/s]`, ewmins, ewmaxs, ewds)


ggplot(ews, aes(siemens$`Windgeschwindigkeit [m/s]`, colour = variable)) +
  geom_line(aes(y = ews$ewmins, colour ="Verguetung 3.80 [ct/kWh]"))+
  geom_line(aes(y = ews$ewmaxs, colour = "Verguetung 6.30 [ct/kWh]"))+
  geom_line(aes(y = ews$ewds, colour = "Verguetung 5.67 [ct/kWh]"))+
  labs(title="Ertraege der Siemens SWT 3.15 142", x= "Windgeschwindigkeit [m/s]", y= "Ertrag [EUR]/Jahr")+
  scale_y_continuous(labels=dollar_format(prefix="€"), breaks = seq(0, 40000000, by = 100000))+
  scale_x_continuous(breaks=1:18)+
  geom_vline(xintercept=6.85)+
  theme(legend.position="bottom")

rm(ewdg, ewds, ewmaxg, ewmaxs, ewming, ewmins, verbund)


#Ertraege ueber Lebensdauer 20 Jahre analog (s.o.)

ewg$ewming <- ewg$ewming*20
ewg$ewmaxg <- ewg$ewmaxg*20
ewg$ewdg <- ewg$ewdg*20

ews$ewmins <- ews$ewmins*20
ews$ewmaxs <- ews$ewmaxs*20
ews$ewds <- ews$ewds*20


#Berechnung der Kosten analog (s.o.)

#Kosten GE Wind 4.8 158 
#k + bkges #GE (s.o.)

gkmin <- 5544000 + 2217600
gkmax <- 7560000 + 3024000


#Kosten Siemens SWT 3.15 142
#k + bkges #Siemens (s.o.)

skmin <- 18191250 + 7276500
skmax <- 24806250 + 9922500


#Gewinnrechnung analog (s.o.)

#Gewinn GE Wind 4.8 158

cf_minAg <- ewg$ewming - gkmin
cf_minBg <- ewg$ewming - gkmax

cf_maxAg <- ewg$ewmaxg - gkmin
cf_maxBg <- ewg$ewmaxg - gkmax

cf_dAg <- ewg$ewdg - gkmin
cf_dBg <- ewg$ewdg - gkmax

cfg <- cbind.data.frame(cf_minAg, cf_minBg, cf_maxAg, cf_maxBg, cf_dAg, cf_dBg)
colnames(cfg) <- c("S1 Gewinne [EUR]", "S2 Gewinne [EUR]", "S3 Gewinne [EUR]", "S4 Gewinne [EUR]", "S5 Gewinne [EUR]", "S6 Gewinne [EUR]")



ggplot(cfg, aes(ge$`Windgeschwindigkeit [m/s]`, colour = variable)) +
  geom_line(aes(y = cfg$`S1 Gewinne [EUR]`, colour ="Gewinne [EUR] S1"))+
  geom_line(aes(y = cfg$`S2 Gewinne [EUR]`, colour = "Gewinne [EUR] S2"))+
  geom_line(aes(y = cfg$`S3 Gewinne [EUR]`, colour ="Gewinne [EUR] S3"))+
  geom_line(aes(y = cfg$`S4 Gewinne [EUR]`, colour = "Gewinne [EUR] S4"))+
  geom_line(aes(y = cfg$`S5 Gewinne [EUR]`, colour ="Gewinne [EUR] S5"))+
  geom_line(aes(y = cfg$`S6 Gewinne [EUR]`, colour = "Gewinne [EUR] S6"))+
  labs(title="Gewinne pro Szenario der GE Wind 4.8 158", x= "Windgeschwindigkeit [m/s]", y= "Gewinn [EUR]")+
  scale_y_continuous(labels=dollar_format(prefix="€"), breaks = seq(-10000000, 30000000, by = 5000000))+
  scale_x_continuous(breaks=1:18)+
  geom_vline(xintercept=6.85)+
  theme(legend.position="bottom")


#Gewinn Siemens SWT 

cf_minAVs <- ews$ewmins - skmin
cf_minBVs <- ews$ewmins - skmax

cf_maxAVs <- ews$ewmaxs - skmin
cf_maxBVs <- ews$ewmaxs - skmax

cf_dAVs <- ews$ewds - skmin
cf_dBVs <- ews$ewds - skmax


cfs <- cbind.data.frame(cf_minAVs, cf_minBVs, cf_maxAVs, cf_maxBVs, cf_dAVs, cf_dBVs)
colnames(cfs) <- c("S1 Gewinne [EUR]", "S2 Gewinne [EUR]", "S3 Gewinne [EUR]", "S4 Gewinne [EUR]", "S5 Gewinne [EUR]", "S6 Gewinne [EUR]")


ggplot(cfs, aes(siemens$`Windgeschwindigkeit [m/s]`, colour = variable)) +
  geom_line(aes(y = cfs$`S1 Gewinne [EUR]`, colour ="Gewinne [EUR] S1"))+
  geom_line(aes(y = cfs$`S2 Gewinne [EUR]`, colour = "Gewinne [EUR] S2"))+
  geom_line(aes(y = cfs$`S3 Gewinne [EUR]`, colour ="Gewinne [EUR] S3"))+
  geom_line(aes(y = cfs$`S4 Gewinne [EUR]`, colour = "Gewinne [EUR] S4"))+
  geom_line(aes(y = cfs$`S5 Gewinne [EUR]`, colour ="Gewinne [EUR] S5"))+
  geom_line(aes(y = cfs$`S6 Gewinne [EUR]`, colour = "Gewinne [EUR] S6"))+
  labs(title="Gewinne pro Szenario der Siemens SWT 3.15 142", x= "Windgeschwindigkeit [m/s]", y= "Gewinn [EUR]")+
  scale_y_continuous(labels=dollar_format(prefix="€"), breaks = seq(-50000000, 50000000, by = 5000000))+
  scale_x_continuous(breaks=1:18)+
  geom_vline(xintercept=6.85)+
  theme(legend.position="bottom")
