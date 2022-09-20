#Documento R che comprende l'analisi del dataset fornito 

#Librerie utilizzate

install.packages("broom")
library(car)
library(MASS)
library(leaps)
library(dplyr)
library(broom)

#Scelgo la working directory 
setwd("C:/Users/Antonio/Desktop/Progetto_Statistica")

#Recupero i dati e li assegno a un data frame 
data=read.csv('Dataset_AH_gruppo11.csv') 

#Controllo se ci sono entry nulle 
table(is.na(data)) 
#Nel nostro caso la tabella è popolata  

# Analisi delle prime 5 righe del dataset
head(data)
# Analisi delle ultime 5 righe del dataset
tail(data) 

#Controlliamo che non ci siano colonne uguali e che i dati siano numerici 
str(data) 

#Visualizziamo il dataset nella sua forma tabellare 
View(data) 

#Resoconto del dataset 
summary(data) 

#Per creare una tabella delle frequenze è necessario calcolare innanzitutto il numero di classi necessarie
#k = 1+3,3*Log(N), che nel nostro caso vale 8
#per ottenere l'ampiezza di ogni classe dividiamo l'ampiezza tra il max e il min per 8
#cpu: max = 1,71 min = -1,6951 diff = 3,4051 A=0,4256
#HD: A=0,4346
#Proc: A=0,4318
#Aging: A=0,4391
#Audio: A=0,4076
#RAM: A=0,4705

#CPU TABLE
countCPUClass1<-0

countCPUClass2<-0

countCPUClass3<-0

countCPUClass4<-0

countCPUClass5<-0

countCPUClass6<-0

countCPUClass7<-0

countCPUClass8<-0

for(i in data$x1_CPU) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  if(i >= -1.6951 & i< -1.2695)
    countCPUClass1=countCPUClass1+1
  else if(i >= -1.2695 & i< -0.8439){
    countCPUClass2=countCPUClass2+1
  }
  else if(i >= -0.8439 & i< -0.4183){
    countCPUClass3=countCPUClass3+1
  }
  else if(i >= -0.4183 & i< 0.0073){
    countCPUClass4=countCPUClass4+1
  }
  else if(i >= 0.0073 & i< 0.4329){
    countCPUClass5=countCPUClass5+1
  }   
  else if(i >= 0.4329 & i< 0.8585){
    countCPUClass6=countCPUClass6+1
  }
  else if(i >= 0.8585 & i< 1.2841){
    countCPUClass7=countCPUClass7+1
  }
  else if(i >= 1.2841 & i<= 1.72){ #A causa di problemi di approssimazione dell'ambiente
    countCPUClass8=countCPUClass8+1
  }
}
countCPUClass1

countCPUClass2

countCPUClass3

countCPUClass4

countCPUClass5

countCPUClass6

countCPUClass7

countCPUClass8


#HD TABLE

countHDClass1<-0

countHDClass2<-0

countHDClass3<-0

countHDClass4<-0

countHDClass5<-0

countHDClass6<-0

countHDClass7<-0

countHDClass8<-0

for(i in data$x2_HD) {
  if(i >= -1.78468 & i< -1.35008)
    countHDClass1=countHDClass1+1
  else if(i >= -1.35008 & i< -0.91548){
    countHDClass2=countHDClass2+1
  }
  else if(i >= -0.91548 & i< -0.48088){
    countHDClass3=countHDClass3+1
  }
  else if(i >= -0.48088 & i< -0.04628){
    countHDClass4=countHDClass4+1
  }
  else if(i >= -0.04628 & i< 0.38832){
    countHDClass5=countHDClass5+1
  }   
  else if(i >= 0.38832 & i< 0.82292){
    countHDClass6=countHDClass6+1
  }
  else if(i >= 0.82292 & i< 1.25752){
    countHDClass7=countHDClass7+1
  }
  else if(i >= 1.25752 & i<= 1.6927){
    countHDClass8=countHDClass8+1
  }
}

countHDClass1

countHDClass2

countHDClass3

countHDClass4

countHDClass5

countHDClass6

countHDClass7

countHDClass8


#Proc TABLE

countProcClass1<-0

countProcClass2<-0

countProcClass3<-0

countProcClass4<-0

countProcClass5<-0

countProcClass6<-0

countProcClass7<-0

countProcClass8<-0

for(i in data$x3_proc) {
  if(i >= -1.9384 & i< -1.5065)
    countProcClass1=countProcClass1+1
  else if(i >= -1.5065 & i< -1.0747){
    countProcClass2=countProcClass2+1
  }
  else if(i >= -1.0747 & i< -0.6429){
    countProcClass3=countProcClass3+1
  }
  else if(i >= -0.6429 & i< -0.2111){
    countProcClass4=countProcClass4+1
  }
  else if(i >= -0.2111 & i< 0.2207){
    countProcClass5=countProcClass5+1
  }   
  else if(i >= 0.2207 & i< 0.6525){
    countProcClass6=countProcClass6+1
  }
  else if(i >= 0.6525 & i< 1.0843){
    countProcClass7=countProcClass7+1
  }
  else if(i >= 1.0843 & i<= 1.517){
    countProcClass8=countProcClass8+1
  }
}

countProcClass1

countProcClass2

countProcClass3

countProcClass4

countProcClass5

countProcClass6

countProcClass7

countProcClass8

#Aging TABLE

countAgingClass1<-0

countAgingClass2<-0

countAgingClass3<-0

countAgingClass4<-0

countAgingClass5<-0

countAgingClass6<-0

countAgingClass7<-0

countAgingClass8<-0

for(i in data$x4_aging) {
  if(i >= -1.9198 & i< -1.4807)
    countAgingClass1=countAgingClass1+1
  else if(i >= -1.4807 & i< -1.0416){
    countAgingClass2=countAgingClass2+1
  }
  else if(i >= -1.0416 & i< -0.6025){
    countAgingClass3=countAgingClass3+1
  }
  else if(i >= -0.6025 & i< -0.1634){
    countAgingClass4=countAgingClass4+1
  }
  else if(i >= -0.1634 & i< 0.2757){
    countAgingClass5=countAgingClass5+1
  }   
  else if(i >= 0.2757 & i< 0.7148){
    countAgingClass6=countAgingClass6+1
  }
  else if(i >= 0.7148 & i< 1.1539){
    countAgingClass7=countAgingClass7+1
  }
  else if(i >= 1.1539 & i<= 1.594){
    countAgingClass8=countAgingClass8+1
  }
}

countAgingClass1

countAgingClass2

countAgingClass3

countAgingClass4

countAgingClass5

countAgingClass6

countAgingClass7

countAgingClass8

#Audio TABLE

countAudioClass1<-0

countAudioClass2<-0

countAudioClass3<-0

countAudioClass4<-0

countAudioClass5<-0

countAudioClass6<-0

countAudioClass7<-0

countAudioClass8<-0

for(i in data$x5_audio) {
  if(i >= -1.6999 & i< -1.2922)
    countAudioClass1=countAudioClass1+1
  else if(i >= -1.2922 & i< -0.8846){
    countAudioClass2=countAudioClass2+1
  }
  else if(i >= -0.8846 & i< -0.477){
    countAudioClass3=countAudioClass3+1
  }
  else if(i >= -0.477 & i< -0.0694){
    countAudioClass4=countAudioClass4+1
  }
  else if(i >= -0.0694 & i< 0.3382){
    countAudioClass5=countAudioClass5+1
  }   
  else if(i >= 0.3382 & i< 0.7458){
    countAudioClass6=countAudioClass6+1
  }
  else if(i >= 0.7458 & i< 1.1534){
    countAudioClass7=countAudioClass7+1
  }
  else if(i >= 1.1534 & i<= 1.562){
    countAudioClass8=countAudioClass8+1
  }
}

countAudioClass1

countAudioClass2

countAudioClass3

countAudioClass4

countAudioClass5

countAudioClass6

countAudioClass7

countAudioClass8

#RAM TABLE

countRAMClass1<-0

countRAMClass2<-0

countRAMClass3<-0

countRAMClass4<-0

countRAMClass5<-0

countRAMClass6<-0

countRAMClass7<-0

countRAMClass8<-0

for(i in data$x6_RAM) {
  if(i >= -2.01071 & i< -1.54021)
    countRAMClass1=countRAMClass1+1
  else if(i >= -1.54021 & i< -1.06971){
    countRAMClass2=countRAMClass2+1
  }
  else if(i >= -1.06971 & i< -0.59921){
    countRAMClass3=countRAMClass3+1
  }
  else if(i >= -0.59921 & i< -0.12871){
    countRAMClass4=countRAMClass4+1
  }
  else if(i >= -0.12871 & i< 0.34179){
    countRAMClass5=countRAMClass5+1
  }   
  else if(i >= 0.34179 & i< 0.81229){
    countRAMClass6=countRAMClass6+1
  }
  else if(i >= 0.81229 & i< 1.28279){
    countRAMClass7=countRAMClass7+1
  }
  else if(i >= 1.28279 & i<= 1.75363){
    countRAMClass8=countRAMClass8+1
  }
}

countRAMClass1

countRAMClass2

countRAMClass3

countRAMClass4

countRAMClass5

countRAMClass6

countRAMClass7

countRAMClass8


#box plot relativo al dataset 
boxplot(data[,1]) #y
boxplot(data[,2:7]) #x

#calcolo di upper e lower bound per gli outliers 
#CPU: 
Q <- quantile(data$x1_CPU, probs=c(.25, .75), na.rm = FALSE) 
iqr <- IQR(data$x1_CPU) 
up <-  Q[2]+1.5*iqr # Upper Range   
low<- Q[1]-1.5*iqr # Lower Range﻿ 
up 
low 
#HD: 
Q <- quantile(data$x2_HD, probs=c(.25, .75), na.rm = FALSE) 
iqr <- IQR(data$x2_HD) 
up <-  Q[2]+1.5*iqr # Upper Range   
low<- Q[1]-1.5*iqr # Lower Range﻿ 
up 
low 
#Processo: 
Q <- quantile(data$x3_proc, probs=c(.25, .75), na.rm = FALSE) 
iqr <- IQR(data$x3_proc) 
up <-  Q[2]+1.5*iqr # Upper Range   
low<- Q[1]-1.5*iqr # Lower Range﻿ 
up 
low 
#Aging: 
Q <- quantile(data$x4_aging, probs=c(.25, .75), na.rm = FALSE) 
iqr <- IQR(data$x4_aging) 
up <-  Q[2]+1.5*iqr # Upper Range   
low<- Q[1]-1.5*iqr # Lower Range﻿ 
up 
low 
#Audio: 
Q <- quantile(data$x5_audio, probs=c(.25, .75), na.rm = FALSE) 
iqr <- IQR(data$x5_audio) 
up <-  Q[2]+1.5*iqr # Upper Range   
low<- Q[1]-1.5*iqr # Lower Range﻿ 
up 
low 
#RAM: 
Q <- quantile(data$x6_RAM, probs=c(.25, .75), na.rm = FALSE) 
iqr <- IQR(data$x6_RAM) 
up <-  Q[2]+1.5*iqr # Upper Range   
low<- Q[1]-1.5*iqr # Lower Range﻿ 
up 
low 

#Cerco gli outliers per ogni colonna 
outliersCPU <- boxplot(data$x1_CPU, plot=FALSE)$out 
outliersCPU 
outliersHD <- boxplot(data$x2_HD, plot=FALSE)$out 
outliersHD 
outliersPROC <- boxplot(data$x3_proc, plot=FALSE)$out 
outliersPROC 
outliersAGING <- boxplot(data$x4_aging, plot=FALSE)$out 
outliersAGING 
outliersAUDIO <- boxplot(data$x5_audio, plot=FALSE)$out 
outliersAUDIO 
outliersRAM <- boxplot(data$x6_RAM, plot=FALSE)$out 
outliersRAM 

#Scatter plot: 
pairs(data , upper.panel = NULL) 

#Analisi di correlazione 
install.packages('corrplot') 
library(corrplot) 
data_new=data[,1:7] 
cor1 = cor(data_new) 
corrplot(cor1, method = "number", type = "lower") 


#Normalità della distribuzione 
qqnorm(data$x1_CPU) 
qqline(data$x1_CPU) 
qqnorm(data$x2_HD) 
qqline(data$x2_HD) 
qqnorm(data$x3_proc) 
qqline(data$x3_proc) 
qqnorm(data$x4_aging) 
qqline(data$x4_aging) 
qqnorm(data$x5_audio) 
qqline(data$x5_audio) 
qqnorm(data$x6_RAM) 
qqline(data$x6_RAM) 

# Assunzioni di normalità delle distribuzioni delle v.i.

shapiro.test(data$x1_CPU) # L'assunzione di normalità non è favorita, essendo il p-value = 0.000902 < 0.05
hist(data$x1_CPU, main="CPU histogram", xlab="CPU")

shapiro.test(data$x2_HD) # L'assunzione di normalità non  è favorita, essendo il p-value =  0.004082 < 0.05
hist(data$x2_HD, main="HDD histogram", xlab="HD") 

shapiro.test(data$x3_proc) # L'assunzione di normalità non è favorita, essendo il p-value = 0.0006523 < 0.05
hist(data$x3_proc, main="Process histogram", xlab="Proc") 

shapiro.test(data$x4_aging) # L'assunzione di normalità non è favorita, essendo il p-value = 0.0006833 < 0.05
hist(data$x4_aging, main="Aging histogram", xlab="Aging") 

shapiro.test(data$x5_audio) # L'assunzione di normalità non è favorita, essendo il p-value = 0.0001532 < 0.05
hist(data$x5_audio, main="Audio histogram", xlab="Audio") 

shapiro.test(data$x6_RAM) # L'assunzione di normalità non è favorita, essendo il p-value = 0.01508 < 0.05
hist(data$x6_RAM, main="RAM histogram", xlab="RAM") 

# SUDDIVISIONE TRAIN-TEST SET 

splitpoint=75 #Customize the split point between train/test 
train.data=data[1:splitpoint,]
test.data=data[(splitpoint+1):100,]

# ANALISI MODELLI LINEARI

sm1 = lm(y_prestazSWcalc~x1_CPU,data=train.data)
summary(sm1) # 31.24% R^2 -> 1°
sm2 = lm(y_prestazSWcalc~x2_HD,data=train.data)
summary(sm2) # 0.24% R^2,  ma il p-value di x2 (e anche quello generale) è 0.6721 > 0.05, quindi andrebbe scartata
sm3 = lm(y_prestazSWcalc~x3_proc,data=train.data)
summary(sm3) # 12.05% R^2 -> 3°
sm4 = lm(y_prestazSWcalc~x4_aging,data=train.data)
summary(sm4) # 12.93% R^2 -> 2°
sm5 = lm(y_prestazSWcalc~x5_audio,data=train.data)
summary(sm5) # 0.46% R^2, ma il p-value di x2 (e anche quello generale) è 0.561 > 0.05, quindi andrebbe scartata
sm6 = lm(y_prestazSWcalc~x6_RAM,data=train.data)
summary(sm6) # 0.57% R^2, ma p-value di x6 (e anche quello generale) è 0.5167 > 0.05, quindi andrebbe scartata

# Plotting modelli lineari con R^2 maggiore

par(mfrow=c(1,3))
plot( train.data$x1_CPU, train.data$y_prestazSWcalc, ylab = "Prestazioni software", xlab = "Indice prestazioni CPU")
abline(lm(train.data$y_prestazSWcalc ~ train.data$x1_CPU) , col = "blue")
plot( train.data$x4_aging, train.data$y_prestazSWcalc, ylab = "Prestazioni software", xlab = "Indice invecchiamento")
abline(lm(train.data$y_prestazSWcalc ~ train.data$x4_aging) , col = "blue")
plot( train.data$x3_proc, train.data$y_prestazSWcalc, ylab = "Prestazioni software", xlab = "Indice numero di processi attivi")
abline(lm(train.data$y_prestazSWcalc ~ train.data$x3_proc) , col = "blue")

# ANALISI SUL MODELLO LINEARE MIGLIORE DATO L'R^2

# Assunzione di  linearità dei residui 

par(mfrow=c(1,2))
plot(sm1,1)

# Assunzione di Normalità  dei residui

shapiro.test(sm1$residuals) # L'assunzione di normalità è favorita, essendo il p-value = 0.4442 > 0.05
hist(sm1$residuals, main="Normalità residui", xlab="residui")

# Analisi variabili da utilizzare idealmente in base al numero di regressori scelti per regressione multipla 

lin_mod_all=lm(y_prestazSWcalc ~ ., data=data)
step_lin=step(lin_mod_all) # analisi tramite AIC e RSS

subs=regsubsets(y_prestazSWcalc ~ ., data, method="seqrep", nvmax = 6)
summary_sub=summary(subs)
print(summary_sub)
par(mfrow=c(1,3))
plot(summary_sub$rss, xlab = 'Modello', ylab = 'RSS', main = 'Forward Stepwise Selection')
plot(summary_sub$rsq, xlab = 'Modello', ylab = 'R^2', main = 'Forward Stepwise Selection')
plot(summary_sub$adjr2, xlab = 'Modello', ylab = 'AdjR^2', main = 'Forward Stepwise Selection')


# REGRESSIONE LINEARE MULTIPLA CON STEPWISE FORWARD SELECTION 

mm1 = lm(y_prestazSWcalc~x1_CPU + x4_aging,data=train.data)
summary(mm1) # 40.41% R^2

anova(sm1 , mm1) # ANOVA (per vedere se il miglioramento nell'R^2 corretto è statisticamente significativo) -> p-value = 0.001372 < 0.05 -> OTTIMO

mm2 = lm(y_prestazSWcalc~x1_CPU + x4_aging + x3_proc ,data=train.data)
summary(mm2) # 57.91% R^2

anova(mm1 , mm2) # ANOVA -> p-value = 7.38e-07 < 0.05 -> OTTIMO 

# ANALISI SUL MODELLO MIGLIORE DATO L'R^2 OTTENUTO TRAMITE REGRESSIONE LINEARE MULTIPLA 

# Assunzione di  linearità dei residui 

par(mfrow=c(1,2))
plot(mm2,1)

# Assunzione di Normalità  dei residui

shapiro.test(mm2$residuals) # L'assunzione di normalità è favorita, essendo il p-value = 0.4802 > 0.05
hist(mm2$residuals, main="Normalità residui", xlab="residui")

# REGRESSIONE POLINOMIALE MULTIPLA ORTOGONALE 

pm1 = lm(y_prestazSWcalc ~ poly(x1_CPU,2) + poly(x4_aging,2) + poly(x3_proc,2) + x1_CPU:x4_aging + x1_CPU:x3_proc + x3_proc:x4_aging + x1_CPU:x3_proc:x4_aging, data = train.data)
summary(pm1) # 78.22% R^2 ma p-value di x1^2, x4^2, x1*x4, x1*x3, x4*x3, x1*x3*x4 > 0.05 -> da togliere

pm2 = lm(y_prestazSWcalc ~ x1_CPU + x4_aging + poly(x3_proc,2),  data = train.data)
summary(pm2) # 77.33%  R^2 -> vediamo l'ANOVA

anova(mm2,pm2) # ANOVA -> p-value = 5.441e-11 < 0.05 -> MODELLO MIGLIORE

pm3 = lm(y_prestazSWcalc ~ x1_CPU + x4_aging + poly(x3_proc,3),  data = train.data)
summary(pm3) # 77.44% R^2 ma x3^3 ha p-value= 0.556 > 0.05, quindi NO

# ANALISI SUL MODELLO FINALE 

# Assunzione di  linearità dei residui 

par(mfrow=c(1,2))
plot(pm2,1) # buona, possibili outliers

# Assunzione di Normalità 

shapiro.test(pm2$residuals) # Gli errori non sono normalmente distribuiti considerato il p-value = 0.04996 < 0.05
hist(pm2$residuals, main="Normalità residui", xlab="residui")

# ANALISI SPECIFICA POSSIBILI OUTLIERS 

dm = augment(pm2)
head(dm)

# Analizziamo i valori minimi e massimi dei residui sotto distribuzione Studentizzata

min(dm$.std.resid)
max(dm$.std.resid)

# non ci sono valori < -3 che possono essere eliminati per "rule of thumb", quindi non ci sono outliers

# PREDIZIONI SUL MODELLO FINALE E  ANALISI DELLE PERFORMANCE TRAMITE CALCOLO DI SQE(SSE) e MSQE(MSE)

pred=predict(pm2, test.data)

mse_func=function(actual,predicted) 
{
  mean( (actual-predicted)^2 ) 
}

mymse=mse_func(test.data$y_prestazSWcalc,pred)
mymse #27.51494
myreg=lm(y_prestazSWcalc ~ x1_CPU + x4_aging + poly(x3_proc,2), data=data)
y.fitted=myreg$fitted.values
y=data$y_prestazSWcalc

n=length(y)
sqe=sum((y - y.fitted)^2)
sqe #3139.093
msqe=sqe/(n-5)
msqe #33.04308
S=sqrt(msqe)
S #5.748311

# INTERVALLI DI CONFIDENZA PARAMETRI MODELLO FINALE (2.5 % -> L, 97.5 % -> U)
install.packages("plotrix")
library(plotrix)

c0<-coef(pm2)
cc<-confint(pm2)

par(mfrow=c(1,4))
plotCI( c0[2],1, err="x",ui=cc[2,2], li=cc[2,1], axes = FALSE, xlab = "", ylab="")
axis(side=1)         ## add default y-axis (ticks+labels)
axis(side=2,at=1,label=c("x1_CPU"))
box(bty="l")         ## add box

plotCI( c0[3],1, err="x",ui=cc[3,2], li=cc[3,1], axes = FALSE, xlab = "", ylab="")
axis(side=1)         ## add default y-axis (ticks+labels)
axis(side=2,at=1,label=c("x4_aging"))
box(bty="l")         ## add box

plotCI( c0[4],1, err="x",ui=cc[4,2], li=cc[4,1], axes = FALSE, xlab = "", ylab="")
axis(side=1)         ## add default y-axis (ticks+labels)
axis(side=2,at=1,label=c("x3_proc"))
box(bty="l")         ## add box

plotCI( c0[5],1, err="x",ui=cc[5,2], li=cc[5,1], axes = FALSE, xlab = "", ylab="")
axis(side=1)         ## add default y-axis (ticks+labels)
axis(side=2,at=1,label=c("x3_proc^2"))
box(bty="l")         ## add box

title("Intervalli di confidenza coefficienti parametri", line = -2, outer = TRUE)

# GRAFICO MODELLO FINALE

plot(pm2,2)

