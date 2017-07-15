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
	x<-vector(length=iter)
	x[1]=x0
	for (i in 1:iter-1)
	{
		x[i+1]=(a*x0)%%m
		x0=x[i+1]
		u[i+1]=x[i+1]/m		
	}
	x
}
efg<-function(iterations,m)
{
	x<-vector(length=iterations)
	u<-vector(length=iterations)
	x1<-lcg(17,16807,1,(2^31)-1)
	for(i in 1:iterations)
	{
		if (i<=17)
		{
			x[i]=x1[i]
			u[i]=x[i]/(2^31-1)
		}
		else
		{
			x[i]=(x[i-5]+x[i-17])%%m
			u[i]=x[i]/m
		}	
	}
	cat("Mean:::",mean(u),"Variance:::",var(u))
	for (i in 1:5)
	{
		cat("Autocorrelation:: for lag=",i,"is :::",autocorrelation(iterations,u,i,mean(u)),"\n")
	}
	ploter(u,iterations)
}
autocorrelation<-function(iter,u,l,mu)
{
	num=0
	denom=0
	limit=iter-l
	for (t in l+1:limit) 
	{
		num = num + ((u[t] - mu)*(u[t - l] - mu))
	}
	for (t in 1:iter) 
	{
		denom =denom + (u[t]- mu)^2
	}
	num/denom
	}
iter=100000
efg(iter,2^31)