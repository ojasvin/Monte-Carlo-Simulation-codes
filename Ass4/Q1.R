f<-function(x)
{
	8*x*(1-x)^3
}
rejection<-function()
{
	i=1
	dist<-vector(length=10000)
	while(i<=10000)
		{
			u<-runif(2)
			if (u[2]<=f(u[1]))
			{
				dist[i]=u[1]
				i=i+1
			}

		}
	png("Rejection.png")	
	hist(dist,col="blue",breaks=50)	
}
rejection()