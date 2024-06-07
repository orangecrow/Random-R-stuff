#if (!require("ggplot2")) install.packages("ggplot2")
#library(ggplot2)

#in order to run source("filename", echo=TRUE)
x = c() 
y = c()

for (j in 1:100){
#	y = c()
	for (i in 1:100){
		x <- sample(1:6, 6, replace=TRUE)
		y <- c(y,sum(x==6))
	}
	#hist(y, prob = TRUE, br=length(unique(y)))
	breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
	test <- hist(y, prob = TRUE, br=breaks)
	#test <- hist(y, prob = TRUE)

	#plot(table(y))
	#lines(density(y), col="blue", lwd=2)
}
test
mean(y)
median(y)
sum(y==0)/10000
sum(y==1)/10000
sum(y==2)/10000
sum(y==3)/10000
sum(y==4)/10000
#density_values <- dnorm(x, mean = mean(y), sd = sd(y))
#lines(x, density_values)
#for (i in 1:1000){
#	x <- sample(1:6, 1000, replace=TRUE)
#	y <- c(y,sum(x==6))
#}

#par(mfrow(c(1,2)))

#hist(y, prob = TRUE, breaks=length(unique(y)))

#plot(table(y))
#lines(density(y), col="blue", lwd=2)

