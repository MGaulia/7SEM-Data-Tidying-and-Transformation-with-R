library(car)

### Duomenys
load("korupcija2016.Rdata")

df<-data.frame(
    lytis=duom$S01,
    amzius=duom$S02,
    tautybe=duom$S03
    )

institucijos <- data.frame(
    inst1 = duom$B3701,
    inst2 = duom$B3702,
    inst3 = duom$B3703,
    inst4 = duom$B3704,
    inst5 = duom$B3705,
    inst6 = duom$B3706,
    inst7 = duom$B3707,
    inst8 = duom$B3708,
    inst9 = duom$B3709,
    inst10 = duom$B3710,
    inst11 = duom$B3711,
    inst12 = duom$B3712,
    inst13 = duom$B3713,
    inst14 = duom$B3714,
    inst15 = duom$B3715
)

institucijos$paminiseima <- ifelse(
        institucijos$inst1=="LR Seimo"  |
        institucijos$inst2=="LR Seimo" |
        institucijos$inst3=="LR Seimo" |
        institucijos$inst4=="LR Seimo" |
        institucijos$inst5=="LR Seimo" |
        institucijos$inst6=="LR Seimo" |
        institucijos$inst7=="LR Seimo" |
        institucijos$inst8=="LR Seimo" |
        institucijos$inst9=="LR Seimo" |
        institucijos$inst10=="LR Seimo" |
        institucijos$inst11=="LR Seimo" |
        institucijos$inst12=="LR Seimo" |
        institucijos$inst13=="LR Seimo" |
        institucijos$inst14=="LR Seimo" |
        institucijos$inst15=="LR Seimo",
        1,
        0
    )
institucijos$paminiseima [is.na(institucijos$paminiseima )] <- 0
institucijos$paminivyr <- ifelse(
        institucijos$inst1=="LR Vyriausyb\xebs" |
        institucijos$inst2=="LR Vyriausyb\xebs" |
        institucijos$inst3=="LR Vyriausyb\xebs" |
        institucijos$inst4=="LR Vyriausyb\xebs" |
        institucijos$inst5=="LR Vyriausyb\xebs" |
        institucijos$inst6=="LR Vyriausyb\xebs" |
        institucijos$inst7=="LR Vyriausyb\xebs" |
        institucijos$inst8=="LR Vyriausyb\xebs" |
        institucijos$inst9=="LR Vyriausyb\xebs" |
        institucijos$inst10=="LR Vyriausyb\xebs" |
        institucijos$inst11=="LR Vyriausyb\xebs" |
        institucijos$inst12=="LR Vyriausyb\xebs" |
        institucijos$inst13=="LR Vyriausyb\xebs" |
        institucijos$inst14=="LR Vyriausyb\xebs" |
        institucijos$inst15=="LR Vyriausyb\xebs",
        1,
        0)
institucijos$paminivyr[is.na(institucijos$paminivyr )] <- 0
institucijos$nepaminivyr <- ifelse(institucijos$paminivyr==0, 1, 0)

df$keistas <- ifelse(
    institucijos$paminiseima==1 &
    institucijos$nepaminivyr==1 ,
    1,
    0)

### 1 dalis
levels(df$tautybe) <- c('Lietuvis', 'Rusas', 'Kita', 'Kita')

### 2 dalis
fit<-glm(
    keistas~lytis+amzius+tautybe, 
    data=df,
    family=binomial(link = "probit")
    )

summary(fit)

# lytis statistiskai nereiksminga
# amzius statistiskai nereiksmingas ( norsbent naudojant 0.1 reiksmingumo lygi )
lht(fit, c("tautybeRusas=0", "tautybeKita=0"))
# tatutybe statistiskai nereiksminga

exp(fit$coefficients)
# Sansas kad moteris keista 1.07 didesni nei kad vyras keistas
# Sansas kad rusas keistas 1.16 didesnis nei kad Lietuvis keistas
# Su kiekvienais metais keistumo tikimybe padideja 1.005 karto

### 3 dalis
fit2<-glm(
    keistas~lytis+amzius+tautybe+tautybe*amzius, 
    data=df,
    family=binomial(link = "probit")
    )

summary(fit2)
# Taip, dabar amzius statistiskai reiksmingas

# Modelis: (b zymesiu beta koeficientus)
#  z = b0 + b1*1[lytis=M] + b2*amzius + b3*1[tautybe=Rusas] + b4*1[tautybe=Kita] + b5*amzius*1[tautybe=Rusas] + b6*amzius*1[tautybe=Kita]
# Ta galime pasitvirtinti ir is summary(fit2), turime laisvaji nari ir dar 6 koeficientus

# Tikrinome hipoteze:
# Kadangi is formules matome kad amziaus koeficientas yra b2, tai:
# H0: b2 = 0 ir gaudami p reiksme 0.0461 < 0.05, atmeteme nuline hipoteze
# Tad b2 statistiskai reiksmingai skiriasi nuo 0 , amzius statistiskai reiksmingas

### 3 dalis
names(fit$coefficients)
A <- c(1, 0, 60, 0, 0)
eta<-A%*%fit$coefficient
se<-sqrt(A%*%vcov(fit)%*%A)
delta<-qnorm(0.975)*se
ci<-c(eta-delta,eta+delta)
prob<-exp(eta)/(exp(eta)+1)
prob.ci<-exp(ci)/(exp(ci)+1)

prob
prob.ci

# Tikimybe kad salygoje zmogus yra keistuolis yra 0.26
# 95% Pasikliovimo interavalas [0.23, 0.29]