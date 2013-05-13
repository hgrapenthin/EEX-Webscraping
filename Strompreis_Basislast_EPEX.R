library(gdata)
library(zoo)
# Average Price for Baseload Power at EPEX Spot per Quarter
urlex <-"http://cdn.eex.com/document/52446/Phelix_Quarterly.xls"
base <- read.xls(urlex, sheet=1, skip=5)  # , perl="C:/location/bin/perl.exe"
# base <- read.csv2("/home/hans/Phelix_Quarterly.csv")
#base$Zeit <- as.Date(strptime(base$Zeit,format="%d.%m.%y"))
# Konvertierung mit as.Date nowendig für xyplot() und lm()
#class(base$Zeit)
#as.Date(base$Zeit)
#as.data.frame(base)
View(base)
as.yearqtr("200706", "%Y%m") 
plot(base,type="l",ylab="Preis Euro/MWh",main="Basis-Strompreis EPEX")
#xyplot(Preis~Zeit,data=base, type="l",ylab="Preis Euro/MWh")
Preis.lm <- lm(Preis~Zeit, data=base)
abline(Preis.lm, col=2, lwd=2)