ploter<-function(u,iter)
{
	v<-vector(length=iter-1)
	w<-vector(length=iter-1)
	for (i in 1:iter-1)
	{
		v[i]=u[i+1]
		w[i]=u[i]
	}
	plot(v,w)
}
lcg <- function(iter,a,x0,m)
{
	u<-vector(length=iter)
	u[1]<-x0/m
	for (i in 1:iter)
	{
		x1<-(a*x0)%%m
		x0<-x1
		u[i+1]<-x1/m		
	}
	hist(u,col="light cyan",breaks=50)
	ploter(u,iter)
}
iter=100000
lcg(iter,16807,1,2^31-1)
cat("\n")
lcg(iter,40692,1,21474833399)
cat("\n")
lcg(iter,40014,1,2147483563)