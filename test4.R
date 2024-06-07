
x <- (1:6)

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

x
prob
E(x, prob)
Wariancja(x,prob)
#P_wystapienia(6,1,1/6)

rzuty_kostka <- function(n){
	x <-0:n
	y <- c()
	for (i in x){
		y <- c(y,P_wystapienia(max(x),i,1/3))
	}
	plot(x,y,type="h", col="blue", lwd=3)
	print(E(x,y))
	print(Wariancja(x,y))
}
rzuty_kostka(100)

