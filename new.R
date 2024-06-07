#dane <- read.csv2("wydajnosc.csv")
#
par(mfrow=c(2,1))
#rozklad <- table(dane$Wydajnosc/length(dane$Wydajnosc))
#dystrybuanta <- cumsum(rozklad)
#plot(as.numeric(names(dystrybuanta)), dystrybuanta, type="s",
#main ="Dsytrybuanta empiryczna - szereg punktowy", xlab="Wydajność", ylab="P-stwo")

#Nie działa
#krok <- 2
#przedzialy <- seq(min(dane$Wydajnosc)-krok, max(dane$Wydajnosc))
#rozkladPrzedzialowy <- table(cut(dane$Wydajnosc, przedzialy,
#length(dane$Wydajnosc)))
#dystrybuantaPrzedzialowa <- cumsum(rozkladPrzedzialowy)
#plot(dystrybyantaPrzedzialowa, type="s", main="Dyst.emp",
#xlab = "coś tam", ylab = "fadfa", xaxt="n")
#axis(1, labels=names(dystrybyantaPrzedzialowa),
#at=1:length(names(dystrybyantaPrzedzialowa)))

#if (!require("DescTools")) install.packages("DescTools")
#library(DescTools)
#
#LorenzWydajnosc <-Lc(dane$Wydajnosc)
#plot(LorenzWydajnosc, general=FALSE, iwd=2, type="l",xlab="p", ylab="L(p)",main="krzywa Lorenza", las=1, pch=NA)

#Gini G: sum_(i=1)^n (2i-n-1)y_i/(n^2 śr(y)) ; y-sort!

#if (!require("corrplot")) install.packages("corrplot")
#install.packages("corrplot")
#library(corrplot)





#dane <- read.table("korelacja",header=T,sep="\t",dec=".")
#danexy <- cor(as.matrix(cbind(dane$x,dane$y)))
#corrplot(danexy)

E <- function(data){
	return(sum(data)/length(data))
}

Wariancja <- function(data){
	len <- length(data)
	probs <- rep(1/len, len)
	Exp_val = E(data)
	return(sum((data-Exp_val)^2*probs))
}


Kowariancja <- function(datax, datay){
	return(E(datax*datay)-(E(datax)*E(datay)))
}

Wsp_korelacji<- function(datax, datay){
	return(Kowariancja(datax, datay)/(sqrt(Wariancja(datax))*sqrt(Wariancja(datay))))
}

Kowariancja(dane$x, dane$y)
Wsp_korelacji(dane$x, dane$y)


#N <-length(dane$x)
#xSr <-mean(dane$x)
#ySr <-mean(dane$y)
#
#xSumKwadrat <- sum((dane$x-xSr)^2)
#ySumKwadrat <- sum((dane$y-ySr)^2)
#xySumOdchylenOdSr <- sum((dane$x-xSr)*(dane$y-ySr))
#
#sX <-sqrt(xSumKwadrat/N)
#sY <-sqrt(ySumKwadrat/N)
#covXY <- xySumOdchylenOdSr/N
#
#wspDet = wspKorPeasrsona^2

#WspKorPearson <-cor(dane$x,dane$y)
#WspKorSpearman<-cor(dane$x,dane$y,method = c("spearman"))
#WspKorKendall<-cor(dane$x,dane$y,method = c("kendall"))
#
#WspKorPearson 
#WspKorSpearman
#WspKorKendall
#
#daneS <- read.table("spearman.txt", header=T, sep="\t",dec=".")
#WspKorSpearman<-cor(daneS[,1],daneS[,2],method = c("spearman"))
#WspKorSpearman
#
#daneK <- read.table("kendall", header=T, sep="\t",dec=".")
#WspKorKendall<-cor(daneK[,2],daneK[,3],method = c("kendall"))
#WspKorKendall

dane <- read.table("PP", header=T, sep="\t",dec=".")

X<-dane$XProdukcja
Y<-dane$YPrzychody

plot(X,Y)
mX <- matrix(X)
mY <- matrix(Y)

oldX <- X
X <- cbind(rep(1,length(dane[,2])), dane[,2])

paramtryMNK <- solve(t(X)%*%X)%*%t(X)%*%dane[,3]
paramtryMNK 
#lines(X,X*paramtryMNK[1,]+paramtryMNK[2,],type="l")

X<-dane$XProdukcja
Y<-dane$YPrzychody
N <- length(X)
#Y <- y
iloczynyXsrX_YsrY <- (X-mean(X))*(Y-mean(Y))
sumaKwadratXsrX <- sum((X-mean(X))^2)
sumaKwadratYsrY <- sum((Y-mean(Y))^2)
covXY <- sum(iloczynyXsrX_YsrY)/N

b = covXY / (sumaKwadratXsrX/N)
a = mean(Y) - b* mean(X)
Yteor = a + b*X

#done bymyself
par(mfrow=c(2,1))
myx <- seq(from = 1, to = 16, length = 2)
myy <- a + b*myx  
plot(X,Y)
lines (myx, myy, col="red")
plot(X,Y)
lines (myx, myy, col="red")

#III sposób - nie działa mi
lm
modelProdPrzych <- lm(YPrzychody~XProdukcja, data=dane)

# dalej
Yteor <- modelProdPrzych$fitted
wspolczynniki <- modelProdPrzych$coefficients
resztyModelu<-(modelProdPrzych)$r.squared

par(mfrow=c(2,1))
plot(X,Y,col="blue",pch=1,xlim=c(2,16),ylim=c(5,18),main="przychody-wart. empir. i teor.")
lines(X,Yteor,col="green")
legend("bottomright",c("Y empiryczne", "Y teoretyczne"),col = c("blue","green"),
pch=c(1,NA),lty=c(NA,1))
lines(X, Y, pch = 15, cex = 1.5)

#dalej dane 1

dane1 <-read.csv2("dane11V24lm1.csv",header=T,sep=";",dec=".")
model1 <-lm(y~x1+x2,data=dane1)
summary(model1)
yteor <- model1$fitted.values
confint(model1,level=0.95)

#dane 2
dane2 <-read.csv2("dane11V24lm2.csv",header=T,sep=";",dec=".")
#K1. postac modelu
#K2. szacowanie par
#K3. CI dla par
#K4. Istotność zm. / sr . wzgl. blad szacunku ("rozdęcie")
#K5. Eliminacja zm. nieistotnych
#K6. Jakość modelu

if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
corrplot(cor(as.matrix(dane2)))

model2 <- lm(T~N+I+P,data=dane2)
summary(model2)

# dane 3
dane3 <-read.csv2("dane11V24lm3.csv",header=T,sep=";",dec=".")
corrplot(cor(as.matrix(dane3)))
model3 <- lm(Y~X1+X2+X3,data=dane3)
summary(model3)

model3 <- lm(Y~X1+X3,data=dane3)
summary(model3)

#dane 4
dane4 <-read.csv2("dane11V24lm4.csv",header=T,sep=";",dec=".")
corrplot(cor(as.matrix(dane4)))

#dalej
trendy <-read.csv2("Trendy.csv",header=T,sep=";",dec=".")
t<-1:20
set.seed(5)
y0<-c(20:1)+rnorm(20)
plot(y0)
model0<-lm(y0~t)
summary(model0)
yteor<-model0$coef[1]+model0$coef[2]*t
plot(y0)
lines(yteor,col="blue")

#dalej trendy y1 - liniowa
y1 <- trendy$y1
plot(y1)
model0<-lm(y1~t)
yteor<-model0$coef[1]+model0$coef[2]*t
lines(yteor,col="blue")

# kwadratowa

t2<-t^2
model<-lm(y1~t+t2)
summary(model)
a = model$coef[1]
b = model$coef[2]
c = model$coef[3]
y<-a+b*t+c*t^2
lines(y,col="red")

#logarytmiczna
model<-lm(log(y1)~t)
summary(model)
a = model$coef[1]
b = model$coef[2]
y<-exp(a+b*t)
lines(y,col="green")

#hiperbola
odw <- 1/t
model<-lm(y1~odw)
summary(model)
a = model$coef[1]
b = model$coef[2] + 2
y<-a+b*odw
lines(y,col="black")
