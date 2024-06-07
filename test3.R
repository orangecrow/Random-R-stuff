#if (!require("ggplot2")) install.packages("ggplot2")
#library(ggplot2)

#in order to run source("test3.R", echo=TRUE)
#
#f <- function(x) x^2
#
#x <- seq(from = 0, to =5, length = 100)
#
#curve(f, from = min(x), to = max(x) , col="green", lwd=2, main= "something" )
#
#xlabel("x-axis")
#ylabel("y-axis")

#method 2

f <- function(x) x^2-1

x <- seq(from = -1, to = 3, length = 1000)

y <- f(x) 

pole_pod_wykresem <-function(X,Y,a,b){
	j <- 1
	sum <- 0
	redX <- c()
	redY <- c()
	for (i in tail(X, -1)){
		if (a<=X[j]&X[j+1]<=b){
			sum <- sum+(X[j+1]-X[j])*(Y[j]+Y[j+1])/2
			redX <- c(redX, X[j])
			redY <- c(redY, Y[j])
		}
		if (X[j]<=b&!X[j+1]<=b){
			redX <- c(redX, X[j])
			redY <- c(redY, Y[j])
		}
		j <- j+1
	}
	if (X[j]<=b){
		redX <- c(redX, X[j])
		redY <- c(redY, Y[j])
	}
	redX <- c(redX, max(redX))
	redX <- c(redX, min(redX))
	redY <- c(redY, 0)
	redY <- c(redY, 0)
	polygon(redX, redY, col="green")
	return(sum)
}

plot (x, y, type = "l", col="red")
pole_pod_wykresem(x, y, -1, 3)
