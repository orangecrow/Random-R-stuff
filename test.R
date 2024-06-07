x <- rnorm(5000)
histX <- hist(x, breaks=30, plot=FALSE)
myvec <- c(0)
myvecvec <- c(myvec)
for (j in 1:10){
	myvec <- c(0)
	for (i in (j*1):(j*10)){
		myvec <- append(myvec,i)
	}
	myvecvec <- append(myvecvec, myvec)
}
myvecvec

	
