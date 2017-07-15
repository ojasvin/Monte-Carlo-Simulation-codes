marsgalia<-function(mean,variance)
{
	i=1
	count=0
	z<-vector(length=10000)
	while(i<=10000)
	{
		count=count+1
		u<-runif(2)
		u<-2*u-1
		x<-u[1]^2+u[2]^2
		if (x>1)
		{
			next
		}		
		y<-sqrt((-2*log(x))/x)
		z1<-u[1]*y
		z2<-u[2]*y
		z[i]=z1
		z[i+1]=z2
		i=i+2
	}
	z<-z*variance+mean
	hist(z,main="marsgalia",breaks=50)
	cat(mean(z),"   ",var(z),"\n")
	cat("Proportion of values rejected::",1-(i/(2*count)),"\n")
}


box<-function(mean,variance)
{
	u1<-runif(5000)
	r<--2*log(u1)
	u2<-runif(5000)
	th<-2*pi*u2
	z1<-sqrt(r)*cos(th)
	z2<-sqrt(r)*sin(th)
	z<-vector(length=10000)
	i=1
	j=1
	while (i<=10000)
	{
		z[i]=z1[j]
		i=i+1
		z[i]=z2[j]
		j=j+1
		i=i+1
	}
	z<-z*variance+mean
	'hist(z1,breaks=50)
	hist(z2,breaks=50)'
	hist(z,main="box-mueller",breaks=50)
	cat(mean(z),"   ",var(z),"\n")
}
system.time(box(5,5))
system.time(marsgalia(5,5))