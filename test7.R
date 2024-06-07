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
x <- rnorm(5000)
sorted_x <- sort(x)
y <- 1:5000
plot(sorted_x,y)
i <- 1
z <- c()
for (j in sorted_x){
	if(j < sorted_x[4990]){
		z <- c(z, 1/(sorted_x[i+4]-sorted_x[i+3])+1/(sorted_x[i+3]-sorted_x[i+2])+1/(sorted_x[i+2]-sorted_x[i+1])+1/(sorted_x[i+1]-sorted_x[i]))
	}
	i <- i+1
}
plot(z)
	

