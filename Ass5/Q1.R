
f<-function(x)
{
	pdf<-c(0.05,0.25,0.45,0.15,0.1)
	pdf[x]/(2.5*0.20)
}
accep<-function(iterations)
{
	cdf<-c(0.20,0.40,0.60,0.80,1)
	i<-1
	c<-2.5
	dist<-vector(length=iterations)
	while (i<=iterations)
	{
		u<-runif(2)
		for (j in 1:5)
		{
			if (u[1]<=cdf[j])
			{
				x=j
				break
			}
		}
		if (u[2]<=f(x))
		{
			dist[i]=x
			i=i+1
		}
	}
	cat("Mean=",mean(dist),"Variance=",var(dist))
	hist(dist,col="red",breaks=50)
}

inver<-function(iterations)
{
	pdf<-c(0.05,0.25,0.45,0.15,0.1)
	cdf<-c(0.05,0.3,0.75,0.9,1)
	u<-runif(iterations)
	dist<-vector(length=iterations)
	for (i in 1:iterations)
	{
		for (j in 1:5)
		{
			if (u[i]<=cdf[j])
			{
				dist[i]=j
				break
			}
		}
	}
	cat("Mean=",mean(dist),"Variance=",var(dist))
	hist(dist,col="red",breaks=50)
}
inver(10)
accep(10)