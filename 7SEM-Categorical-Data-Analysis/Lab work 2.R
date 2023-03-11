load("korupcija2016.Rdata")
# 1
d<-data.frame(paimtu=duom$B17, 
               lytis=duom$S01,
               kaltas=duom$B36,
               sektorius=duom$D06)
# Pasalinu neapsisprendusius is Q17 klausimo ir paverciu Q17 i 2r
d <- subset(d, unclass(paimtu)<3)
d <- droplevels(d)
d$paimtu<-ifelse(unclass(d$paimtu)==1,1,0)
d$paimtu<-factor(d$paimtu)
# Palieku tik zmones kurie dirba valtybiniame sektoriuje
d <- subset(d, sektorius=="Valstybiniame sektoriuje")
d <- droplevels(d)
d <- subset(d, select = -c(sektorius))

# Pagal lyti
# paimtu - 2r, lytis - 2r
# rr,or,rho_P,tau, gamma
# polichorines ir poliserijines koreliacijos neskaiciuosiu nes lytis neatitinka logiskumo tam
library(epitools)
# rr
riskratio(d$lytis,d$paimtu)
# or
epitools::oddsratio(d$lytis,d$paimtu)
# rho_P
cor(unclass(d$lytis),unclass(d$paimtu))
# Moterys maziau linkusios paimti kysi taciau koreliacija silpna
cor.test(unclass(d$lytis),unclass(d$paimtu))
cor.test(unclass(d$lytis),unclass(d$paimtu), method="kendall")
# Priimu nepriklausomumo hipoteze, lytis nepriklauso nuo to ar paimtu kysi
# gama koeficientas
library(vcd)
library(vcdExtra)
GKgamma(table(d$lytis,d$paimtu))

# Pagal kaltes supratima
# Laikysiu kad kaltes supratimo kintamasis nominalus
# Nes nera lengvai suprasti kas yra kaltesnis kysio davime
# paimtu - 2r, kaltas - nominalus
# Skaiciuosiu U
library(ryouready)
nom.uncertainty(table(d$kaltas,d$paimtu))
# Deja nom.uncertainty siuo atveju duoda NaN
# As mastau kad kaltes supratimas yra spektras tarp visiskos kaltes davejui ir visiskos kaltes priemejui
# Tad galime paskaiciuoti poliserijine koreliacija
d <- subset(d, unclass(kaltas) <4)
d <- droplevels(d)
# Pasidarau kad:
# 1 - visa kalte davejui
# 2 - visa kalte priemejui
# 1.5 - kalte vienoda abiems
x <- unclass(d$kaltas)
x[x==3]<-1.5
table(x)
library(polycor)
polyserial(x, unclass(d$paimtu))
# Kuo daugiau zmogus galvoja kad kalte yra priemejui, tuo maziau jis paimtu kysi
# Taciau koreliacija silpna

# 2
set.seed(2)
n <- c(20, 40, 80, 160, 320, 640)
sqneu <- c()
neu <- c()
for (i in n) {
    Us <- c()
    for (j in c(1:1000)){
    x <- sample( c(1, 0), i, replace=TRUE, prob=c(0.5, 0.5) )
    y <- sample( c(1, 0), i, replace=TRUE, prob=c(0.5, 0.5) )
    U <- nom.uncertainty(table(x, y))
    Us <- c(Us, U$uc.symmetric)
    }
    print(mean(Us))
    sqneu <- c(sqneu, sqrt(i) * mean(Us) )
    neu <- c(neu, i * mean(Us) )
}
# Abu viename grafike
plot(n, sqneu, lwd=4.0,type = "l", col = "blue", xlab = "n", ylab = "sq(n)EU", ylim = c(0,0.8))
lines(n, neu, lwd=4.0,type = "l", col = "red", xlab = "n", ylab = "nEU")

# Sq(n)EU grafikas
plot(n, sqneu, lwd=4.0,type = "l", col = "blue", xlab = "n", ylab = "sq(n)EU", ylim = c(0,0.1))
# Matome kad arteja link 0 
sqneu
# Tas matosi ir is duomenu

# nEU grafikas
plot(n, neu, lwd=4.0,type = "l", col = "red", xlab = "n", ylab = "nEU")
# Is grafiko matosi nukrypimai bet gale sueina i viena taska
neu
# Tas taskas pagal duomenis galetu buti kazkas aplink 0.75

