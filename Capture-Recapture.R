

m <- 20
k <- 20
N0 <- c(40,50,60,70)
x <- 0:min(m,k)
alpha <- 0.1

# Illustrate the critical values c(N0):

par(mfcol=c(2,length(N0)))
for (i in 1:length(N0))
{
	dx <- dhyper(x,m,N0[i]-m,k)
	Fx <- phyper(x,m,N0[i]-m,k)
	c <- min(x[Fx > alpha]) - 1
	barplot(dx,names.arg=x,col=c("blue","red")[1+(x <= c)],
		main=paste("N0 = ",
			as.character(N0[i]),
			", c(N0) = ",
			as.character(c),
			sep=""),
		xlab="x",ylab="P(X = x)")
	sf <- stepfun(x,c(0,Fx))
	plot.stepfun(sf,lw=2,pch="",
		main=paste("N0 = ",
			as.character(N0[i]),
			", c(N0) = ",
			as.character(c),
			sep=""),
		xlab="x",ylab="P(X <= x)")
	abline(h=alpha,col="red",lwd=2)
}

# Show for all values N0 = 20, 21, 22, ...
# which values x are "normal" (blue) and which
# are "too small" (red):

N0min <- 20
N0max <- 100
par(mfcol=c(1,1),mai=c(1,1,0,0))
plot(NULL,
	xlim=c(min(x),max(x)),ylim=c(N0min,N0max),
	xlab="x",ylab="N0")
for (N0 in N0min:N0max)
{
	Fx <- phyper(x,m,N0-m,k)
	points(x,rep(N0,length(x)),
		col=c("blue","red")[1 + (Fx <= alpha)],
		pch=16)
}


m <- 20
k <- 20
N0 <- c(40,50,60,70)
x <- 0:min(m,k)
alpha <- 0.1

# Illustrate the critical values d(N0):

par(mfcol=c(2,length(N0)))
for (i in 1:length(N0))
{
	dx <- dhyper(x,m,N0[i]-m,k)
	Fx <- phyper(x,m,N0[i]-m,k)
	d <- min(x[Fx >= 1-alpha])
	# d = qhyper(1-alpha,m,N0[i]-m,k)
	barplot(dx,names.arg=x,col=c("blue","red")[1+(x > d)],
		main=paste("N0 = ",
			as.character(N0[i]),
			", d(N0) = ",
			as.character(d),
			sep=""),
		xlab="x",ylab="P(X = x)")
	sf <- stepfun(x,c(0,Fx))
	plot.stepfun(sf,lw=2,pch="",
		main=paste("N0 = ",
			as.character(N0[i]),
			", d(N0) = ",
			as.character(d),
			sep=""),
		xlab="x",ylab="P(X <= x)")
	abline(h=1-alpha,col="red",lwd=2)
}

# Show for all values N0 = 20, 21, ..., 100
# which values x are "normal" (blue) and which
# are "too large" (red):

N0min <- 20
N0max <- 100
par(mfcol=c(1,1),mai=c(1,1,0,0))
plot(NULL,
	xlim=c(min(x),max(x)),ylim=c(N0min,N0max),
	xlab="x",ylab="N0")
for (N0 in N0min:N0max)
{
	Fx <- phyper(x,m,N0-m,k)
	d <- min(x[Fx >= 1-alpha])
	points(x,rep(N0,length(x)),
		col=c("blue","red")[1 + (x > d)],
		pch=16)
}



source("Exercise2_solved.R")

CaptureRecapture(20,20,3)
CaptureRecapture(m=20,k=20,X=3,alpha=0.05)


CaptureRecapture(20,20,3,alpha=0.05,illustrate=TRUE)

CaptureRecapture(20,20,1,alpha=0.01)
CaptureRecapture(20,20,1,alpha=0.05)
CaptureRecapture(20,20,1,alpha=0.10)