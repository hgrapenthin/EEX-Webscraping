
library(XML)
library(lattice)
library(latticeExtra)
library(arules)

################################# Plotten der fertigen Tabelle ###########

elix <- read.table("elix.tbl")
tail(elix)
str(elix)
elix$Datum <- as.Date(strptime(elix$Datum,format="%Y-%m-%d" ))
#elix$Datum <- strptime(elix$Datum, format="%Y-%m-%d")
class(elix$Datum)
elix$Peak <- as.numeric(elix$Peak)
elix$Base <- as.numeric(elix$Base)
elix$PeakVolume <- as.numeric(elix$PeakVolume)
elix$BaseVolume <- as.numeric(elix$BaseVolume)
#elix <- unique(elix)   # aussondern von Dubletten
xyplot(Peak+Base ~ Datum, data=elix, type="l",auto.key=T, ylab="Euro/MWh",ylim=c(0,200))
# Filtern der Daten mit Periode k

k <- 30
elix$Base <- filter(elix$Base,rep(1/k,k))     #method="convolution"
elix$Peak <- filter(elix$Peak, rep(1/k,k))
xyplot(Base + Peak~ Datum, data=elix, type="l",auto.key=T, ylab="Euro/MWh")

xyplot(PeakVolume+BaseVolume ~ Datum, data=elix, type="l",auto.key=T, ylab="GWh")
elix$BaseVolume <- filter(elix$BaseVolume,rep(1/k,k))     #method="convolution"
elix$PeakVolume <- filter(elix$PeakVolume, rep(1/k,k))
xyplot(PeakVolume+BaseVolume ~ Datum, data=elix, type="l",auto.key=T, ylab="GWh")
################################################################################
######Anhängen neuer Daten an vorhandene Tabelle elix.tbl
####################################################################
# Sys.time()
# starttermin <- as.Date("2010-10-25")  # Starttag # as.Date("2005-02-14")
# starttermin <- as.?Date("2012-10-21")
starttermin <- elix$Datum[length(elix$Datum)]
# weekdays(starttermin)
endtermin <- as.Date(Sys.time())-1
# weekdays(endtermin)
# Starttermin ist letzter Tag der Abrufwoche
abruf <- 0
abruf <- starttermin+7
# as.numeric(format(abruf,"%Y"))  # Prüfen auf Wochentag mit %w, So = 0
while (abruf < endtermin) 
{             #  Schaltet am Endewochenweise weiter 
jahr <- as.numeric(format(abruf,"%Y"))    # Herausles e der Jahreszahl vierstellig
class(jahr) # doy <- 4
doy <- as.numeric(format(abruf,"%j"))    # Tag Nummer des Jahres, für Jahreswechsel
if (doy < 7) jahr <- c(rep((jahr-1),(7-doy)), rep(jahr,(doy)))
url.phelix <- paste("http://www.eex.com/en/Market Data/Trading Data/Power/Hour Contracts | Spot Hourly Auction/spot-hours-table/",abruf,"/","PHELIX",sep="")     
# zur Übersicht:
#eexparse <- htmlParse(url.phelix)
# eexparse <- readHTMLTable(url.phelix,which=3, stringsAsFactors=F,trim=T)
phelindex <- readHTMLTable(url.phelix, header=F, which=3, stringsAsFactors=F,trim=T) # Phelix ab 2005-02-04
dateline <- paste(as.character(jahr),"/",substring(phelindex[1,((3:9)*2+1)],6,10),sep="")
# dateline <- strptime(dateline,format="%Y/%m/%d")  nicht gestestet
dateline <- as.character(strptime(dateline,format="%Y/%m/%d"))
class(dateline)
eelix <- as.data.frame(cbind(dateline,as.numeric(phelindex[2,((3:9)*2+1)]),as.numeric(phelindex[4,((3:9)*2+1)]),as.numeric(gsub(",", "", phelindex[3,((3:9)*2)]))/1000, as.numeric(gsub(",", "",phelindex[5,((3:9)*2)]))/1000))
# eelix <- as.data.frame(cbind(as.Date(dateline),as.numeric(phelindex[2,((3:9)*2+1)]),as.numeric(phelindex[4,((3:9)*2+1)]),as.numeric(gsub(",", "", phelindex[3,((3:9)*2)]))/1000, as.numeric(gsub(",", "",phelindex[5,((3:9)*2)]))/1000))

#View(eelix)
# str(eelix)
names(eelix)<- c("Datum","Base","Peak","BaseVolume", "PeakVolume")
# eelix$Datum <-strptime(eelix$Datum,format="%Y/%m/%d" )
#if(abruf == starttermin)  write.table(eelix,file="elixInsert.tbl") else 
{
  elix <- read.table("elix.tbl");               # sonst Werte an existierende anhängen
  elix <- rbind(elix,eelix);
  write.table(elix, file="elix.tbl")            # und neue Tabelle ablegen
}
#rm(eelix,phelindex,dateline,elix,doy,jahr)                                       # säubern
abruf <- abruf +7     # Eine Woche weiterschalten für nächste Runde
}
tail(elix)
#elix <- unique(elix)
## Ablage der ersten Daten in elix.txt
#write.table(elix, file="elix.txt")
#rm(eelix)#
######################################################################
#### Ende Anhängen neuer Daten
#####################################################################

elix <- read.table("elix.tbl")
str(elix)
elix$Datum <- as.Date(strptime(elix$Datum,format="%Y/%m/%d" ))
#elix$Datum <- strptime(elix$Datum, format="%Y-%m-%d")
class(elix$Datum)
elix$Peak <- as.numeric(elix$Peak)
elix$Base <- as.numeric(elix$Base)
elix$PeakVolume <- as.numeric(elix$PeakVolume)
elix$BaseVolume <- as.numeric(elix$BaseVolume)
#elix <- unique(elix)
xyplot(Peak+Base ~ Datum, data=elix, type="l",auto.key=T, ylab="Euro/MWh",ylim=c(0,100))
xyplot(PeakVolume+BaseVolume ~ Datum, data=elix, type="l",auto.key=T, ylab="GWh")

##########
elix <- read.table("elixStart.tbl")

elixInsert <- read.table("elixInsert.tbl")
elix <- rbind(elix,elixInsert)
elix$Datum <- strptime(elix$Datum,format="%Y/%m/%d")
elix.ref <- read.table("elix.tbl.ref")
elix <- rbind(elix,elix.ref)
elix <- unique(elix)              
write.table(elix, file="elix.tbl")




###########  Versuch Lesen der Heruntergeladenen Webseite

#url.phelix <- "/home/hans/2012-09-01"
phelix <- readHTMLTable(url.phelix, header=F, which=5, stringsAsFactors=F, trim=T)

## Rausschneiden der leeren Spalten
#phelix <- phelix[,-((1:9)*2)]
#phelix <- phelix[,-2]
#View(phelix)
#class(phelix)
## Formatieren der Zeile 1 auf Y-m-d
dateline <- substring(phelix[1,((2:9)*2+1)],6,10)

## Bau der Zeitspalte Schritt 1: Vervielfachung der Tage für die Zeitspalte
i <- 1
dit <- ""
for (i in (1:8)){
  if(i==1) 
    dit <- rep(dateline[i],24)
  else 
    dit <- c(dit,rep(dateline[i],24) )
}
#View(dit)
## Bau der Stunden für Zeitspalte 
# zeit <-  c(paste("0",(1:9),":","00",":","00",sep=""),paste(10:24,":","00",":","00",sep=""))
zeit <-  c(paste("0",(1:9),sep=""),paste(10:24,sep=""))
#View(zeit)
#class(zeit)
## Zusammenbau von Datum und Zeit für die Zeitspalte
dit <- strptime(paste(dit,zeit),format="%m/%d %H")
unclass(dit)
structure(0,class=c("POSIXt","POSIXct"))
tim <- difftime(dit,"2005-02-14 01:00:00 CET",unit="hours")
#dit <- paste(dit,zeit)
#class(dit)
#View(dit)
## Zusammenfügen aller Preise , Volumen

pp <- 0
vol <- 0
i <- 0
for(i in (2:9)) {
#helix <- as.data.frame(cbind(dit,as.numeric(phelix[(1:24)*2,(i*2+1)]), as.numeric(gsub(",", "", phelix[((1:24)*2+1),(i*2)]))/1000))
pp <- if (i==2) as.numeric(phelix[(1:24)*2,(i*2+1)])
else c(pp,as.numeric(phelix[(1:24)*2,(i*2+1)]) )
vol <- if (i==2) as.numeric(gsub(",", "", phelix[((1:24)*2+1),(i*2)]))/1000 
else 
  c(vol,as.numeric(gsub(",", "", phelix[((1:24)*2+1),(i*2)]))/1000)
}
helix <- as.data.frame(cbind(dit,pp,vol))   #as.data.frame
names(helix) <- c("Datum","PreisPeak", "Volumen")
daterange=c(min(dit),max(dit))   #figure out the lowest and highest months
#View(helix)
helix$PreisPeak <- as.numeric(helix$PreisPeak)
#plot(helix$PreisPeak~tim,data=helix,xaxt="n", type="l",col="blue") #~Datum
#axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="days"), format="%d.%m") #label the x axis by months
#elix$Zeit <- strptime(elix$Zeit,format="%Y-%m-%d %T")
par(mfrow=c(2,1))
plot(as.POSIXlt(format(dit),tzt="GMT"),helix$PreisPeak,type="l", col="blue",xlab="Datum", ylab="Peak Preis €/MWh")
plot(as.POSIXlt(format(dit),tzt="GMT"),helix$Volumen,type="l", col="blue",xlab="Datum", ylab="Volumen GWh")

