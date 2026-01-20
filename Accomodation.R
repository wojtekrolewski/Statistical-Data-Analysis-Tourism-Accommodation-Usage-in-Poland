###  Statystyczna analiza danych  ###
###           noclegi             ###
###       Wojciech Rolewski       ###


#Biblioteki
setwd("E:/Projekt_stat")
install.packages("xlsx")
install.packages("ggplot2")
install.packages("moments")
library(moments)
library(xlsx)
library(ggplot2)

#Wczytywanie pliku
#https://stat.gov.pl/download/gfx/portalinformacyjny/pl/defaultaktualnosci/5498/4/109/1/tabl62_wykorzystanie_turystycznych_obiektow_noclegowych_w_polsce.xlsx
#Plik zawiera dane przestawiaj?ce wykorzystanie turystycze obiekt?w noclegowych w latach 2010-2020

noclegi<-read.xlsx(file = "E:/IAD/stat.an/Projekt_stat/noclegi.xlsx", sheetIndex = 1) #wczytujemy dane z pliku
noclegi<-na.omit(noclegi) #usuwamy NA
rownames(noclegi)<-noclegi[,1] #ustawiamy kolumne pierwsz? jako kolumn? nazw wierszy
noclegi<-noclegi[,-1] #usuwamy zb?dn? kolumn? numeruj?c? wiersze
names(noclegi) <- c("Wykorzystanie obiekt?w noclegowych [O]", "Wykorzystanie obiekt?w noclegowych [Z]",
                    "Udzielone noclegi [O]","Udzielone noclegi [Z]", "Wynaj?te pokoje [O]", "Wynaj?te pokoje [Z]") 
#zmieniamy nazwy kolumn na kr?tsze ( O oznacza liczb? og??em a Z liczb? os?b z innych kraj?w ni? Polska)
noclegi_Ikw<-noclegi[seq(1, 44, 4), ]
noclegi_IIkw<-noclegi[seq(2, 44, 4), ]
noclegi_IIIkw<-noclegi[seq(3, 44, 4), ]
noclegi_IVkw<-noclegi[seq(4, 44, 4), ] #tworzymy zestawienie kwartalne z ka?dego roku

noclegi2010<-noclegi[seq(1, 4, 1), ]
noclegi2011<-noclegi[seq(5, 8, 1), ]
noclegi2012<-noclegi[seq(9, 12, 1), ]
noclegi2013<-noclegi[seq(13, 16, 1), ]
noclegi2014<-noclegi[seq(17, 20, 1), ]
noclegi2015<-noclegi[seq(21, 24, 1), ]
noclegi2016<-noclegi[seq(25, 28, 1), ]
noclegi2017<-noclegi[seq(29, 32, 1), ]
noclegi2018<-noclegi[seq(33, 36, 1), ]
noclegi2019<-noclegi[seq(37, 40, 1), ]
noclegi2020<-noclegi[seq(41, 44, 1), ]#podzia? na lata

max(noclegi$"Wykorzystanie obiekt?w noclegowych [O]") #najwi?ksza liczba osob korzystaj?cych z nocleg?w
length(noclegi$"Wykorzystanie obiekt?w noclegowych [O]") #ilo?? badanych kwarta??w
range(noclegi$"Wykorzystanie obiekt?w noclegowych [O]") #przedzia? wyst?puj?cych warto?ci
srednia<-mean(noclegi$"Wykorzystanie obiekt?w noclegowych [O]") #srednia arytmetyczna ze wszystkich kwarta??w
median(noclegi$"Wykorzystanie obiekt?w noclegowych [O]") #warto?? ?rednia
quantile(noclegi$"Wykorzystanie obiekt?w noclegowych [O]",0.5)
quantile(noclegi$"Wykorzystanie obiekt?w noclegowych [O]",c(0.25,0.5,0.75))
summary(noclegi$"Wykorzystanie obiekt?w noclegowych [O]")#kwantyle
sd(noclegi$"Wykorzystanie obiekt?w noclegowych [O]")#odchylenie standardowe
kurtosis(noclegi$"Wykorzystanie obiekt?w noclegowych [O]")#miara koncentracji
sd(noclegi$"Wykorzystanie obiekt?w noclegowych [O]")/sqrt(length(noclegi$"Wykorzystanie obiekt?w noclegowych [O]"))
#b??d standardowy
pu<-qnorm(0.975)*sd(noclegi$"Wykorzystanie obiekt?w noclegowych [O]")/sqrt(length(noclegi$"Wykorzystanie obiekt?w noclegowych [O]"))
(lewo<-srednia-pu)
(prawo<-srednia+pu)#95% przedzial ufnosci
skewness(noclegi$"Wykorzystanie obiekt?w noclegowych [O]")#skosnosc

par(mfrow = c(2,2)) 
par(mar=c(7, 5, 2, 1))#przesuni?cie wykresu
barplot(noclegi_Ikw$`Wykorzystanie obiekt?w noclegowych [O]`,ylim=c(0,12000), names=row.names(noclegi_Ikw),
        las=3,col="deepskyblue4",main="Use of accommodation [Q1]")
barplot(noclegi_IIkw$`Wykorzystanie obiekt?w noclegowych [O]`,ylim=c(0,12000),names=row.names(noclegi_IIkw),
        las=3,col="forestgreen",main="Use of accommodation [Q2]")
barplot(noclegi_IIIkw$`Wykorzystanie obiekt?w noclegowych [O]`,ylim=c(0,12000),names=row.names(noclegi_IIIkw),
las=3,col="firebrick1",main="Use of accommodation [Q3]")
barplot(noclegi_IVkw$`Wykorzystanie obiekt?w noclegowych [O]`,ylim=c(0,12000),names=row.names(noclegi_IVkw),
        las=3,col="paleturquoise1",main="Use of accommodation [Q4]")
#zestawienie wykres?w s?upkowych przedstawiaj?cych wykorzystanie obiekt?w noclegowych 
plot(density(noclegi_Ikw$"Wykorzystanie obiekt?w noclegowych [O]"),main="Qarter I",col="aquamarine",
     xlim=c(0,14000),ylim=c(0,0.00035))
plot(density(noclegi_IIkw$"Wykorzystanie obiekt?w noclegowych [O]"),main="Qarter II",col="chartreuse2",
     xlim=c(0,14000),ylim=c(0,0.00035))
plot(density(noclegi_IIIkw$"Wykorzystanie obiekt?w noclegowych [O]"),main="Qarter III",col="orangered1",
     xlim=c(0,14000),ylim=c(0,0.00035))
plot(density(noclegi_IVkw$"Wykorzystanie obiekt?w noclegowych [O]"),main="Qarter IV",col="darkblue",
     xlim=c(0,14000),ylim=c(0,0.00035))

#zestawienie wykres?w rozk?adu g?sto?ci dla wykorzystania obiekt?w noclegowych
par(mfrow = c(2,1))
hist(noclegi_Ikw$"Wykorzystanie obiekt?w noclegowych [O]",main="Histogram wykorzystania obiekt?w noclegowych",
     xlab="Wykorzystanie obiekt?w noclegowych [setki tysi?cy]", col="aquamarine", ylim=(c(0,4)))
hist(noclegi_IIkw$"Wykorzystanie obiekt?w noclegowych [O]",main="Histogram wykorzystania obiekt?w noclegowych",
     xlab="Wykorzystanie obiekt?w noclegowych [setki tysi?cy]", col="chartreuse2", ylim=(c(0,4)))
hist(noclegi_IIIkw$"Wykorzystanie obiekt?w noclegowych [O]",main="Histogram wykorzystania obiekt?w noclegowych",
     xlab="Wykorzystanie obiekt?w noclegowych [setki tysi?cy]", col="darkorange", ylim=(c(0,4)))
hist(noclegi_IVkw$"Wykorzystanie obiekt?w noclegowych [O]",main="Histogram wykorzystania obiekt?w noclegowych",
     xlab="Wykorzystanie obiekt?w noclegowych [setki tysi?cy]", col="lightblue", ylim=(c(0,4)))


plot(ecdf(noclegi$`Wykorzystanie obiekt?w noclegowych [O]`),main="Dystrybuanta wykorzystania obiekt?w noclegowych[O]",
     col="darkgreen",xlab="osoby [tys]")
#wykres dystrybuanty wykorzystania obiektow noclegowych ogolem
plot(ecdf(noclegi$`Wykorzystanie obiekt?w noclegowych [Z]`),main="Dystrybuanta wykorzystania obiekt?w noclegowych [Z]",
     col="darkblue",xlab="osoby [tys]")
#wykres dystrybuanty wykorzystania obiektow noclegowych przez turystow zagranicznych
plot(ecdf(noclegi$`Udzielone noclegi [O]`),main="Dystrybuanta udzielonych nocleg?w [O]",
     col="darkred",xlab="osoby [tys]")
#wykres dystrybuanty udzielonych noclegow og??em
plot(ecdf(noclegi$`Udzielone noclegi [Z]`),main="Dystrybuanta udzielonych nocleg?w [Z]",
     col="darkorange",xlab="osoby [tys]")
#wykres dystrybuanty udzielonych noclegow turystom zagranicznym

par(mfrow = c(1,1))
par(mar=c(7, 5, 5, 1))#przesuni?cie wykresu
boxplot(noclegi2010$"Wykorzystanie obiekt?w noclegowych [O]",noclegi2011$"Wykorzystanie obiekt?w noclegowych [O]",
        noclegi2012$"Wykorzystanie obiekt?w noclegowych [O]",noclegi2013$"Wykorzystanie obiekt?w noclegowych [O]",
        noclegi2014$"Wykorzystanie obiekt?w noclegowych [O]",noclegi2015$"Wykorzystanie obiekt?w noclegowych [O]",
        noclegi2016$"Wykorzystanie obiekt?w noclegowych [O]",noclegi2017$"Wykorzystanie obiekt?w noclegowych [O]",
        noclegi2018$"Wykorzystanie obiekt?w noclegowych [O]",noclegi2019$"Wykorzystanie obiekt?w noclegowych [O]",
        noclegi2020$"Wykorzystanie obiekt?w noclegowych [O]",main="Distribution of accommodation in particular years", col='orange', names = 2010:2020, ylab="People [thousands]")
#Wykres pude?kowy przedstawiaj?cy rozk?ad wykorzystania obiekt?w noclegowych w poszczeg?lnych latach


t.test(noclegi$`Wykorzystanie obiekt?w noclegowych [O]`,mu=3000)
t.test(noclegi$`Wykorzystanie obiekt?w noclegowych [O]`,mu=6500)
#hipoteza 1

t.test(noclegi_IIIkw$`Udzielone noclegi [Z]`,mu=2000)
t.test(noclegi_IIIkw$`Udzielone noclegi [Z]`,mu=4600)
#hipoteza 2