#if (!require("somelib")) install.packages("somelib")
#library(somelib)

#in order to run source("filename", echo=TRUE)

prob <- rep(1/6, 6)

E <- function(data, probabilities){
	return(sum(data*probabilities))
}

Wariancja <- function(data, probs){
	Exp_val = E(data,probs)
	return(sum((data-Exp_val)^2*probs))
}

P_wystapienia <- function(n,k,p){
	return(sum(choose(n,k)*p^k*(1-p)^(n-k)))
}

Kowariancja <- function(datax, datay){
	probs <- rep(1/length(datax), length(datax))
	return(E(datax*datay,probs)-(E(datax,probs)*E(datay,probs)))
}

Wsp_korelacji<- function(datax, datay){
	probs <- rep(1/length(datax), length(datax))
	return(Kowariancja(datax, datay)/(sqrt(Wariancja(datax,probs))*sqrt(Wariancja(datay,probs))))
}

Pole_pod_wykresem <- function(X, Y, a, b){
	for each in X


#par(new = FALSE)
data1 <- c( 1, 2, 3, 4, 5)
data2 <- c( 2, 4, 3, 2, 2)

plot(data1, type="l")
par(new=TRUE)
plot(data2)
