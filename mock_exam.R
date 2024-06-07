#dane <- read.csv2("wydajnosc.csv")

Z <- rnorm(5000)

funkcja_A <- function(Z){
	A2 <- c( mean(Z), median(Z), quantile(Z, probs = 0.95))

	min <- quantile(Z, probs = 0.25)
	max <- quantile(Z, probs = 0.75)
	result <-0
	for (i in Z){
		if ( i > min & i < max){
			result <- result + 1
		}
	}

	A3 <- result/length(Z)*100

	A4 <- hist(Z, breaks=3)

	ret_val <- c(A2, A3, A4)
	return(ret_val)
}

trendy <-read.csv2("Trendy.csv",header=T,sep=";",dec=".")
t<-1:20
set.seed(5)

y1 <- trendy$y1
plot(y1)
model0<-lm(y1~t)
yteor<-model0$coef[1]+model0$coef[2]*t
lines(yteor,col="blue")

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
model<-lm(1/y1~t)
summary(model)
a = model$coef[1]
b = model$coef[2]
y<-a+b/t
lines(y,col="black")
