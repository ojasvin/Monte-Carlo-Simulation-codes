ThirdBrown<-function()
{
	paths<-10
	count<-5000
	interval<-5/count
	sample<-matrix(0,nrow=(count+1),ncol=paths)
	for(i in 1:paths)
	{
		sample[1,i]<-5
		for(j in 2:(count+1))
		{
			sample[j,i]<-sample[j-1,i]+interval*(0.0325-(0.05*(j-1)*interval))+((interval)^.5)*rnorm(1)*(0.012+0.0138*((j-1)*interval)+0.00125*((j-1)*interval)^2)
		}
	}	
	cat("E[W(2)] = ",mean(sample[2001,]),"\n")
	cat("E[W(5)] = ",mean(sample[5001,]),"\n")
	matplot(sample,main="ThirdBrownian",xlab="Time",ylab="Path",type="l")


}


Brownian<-function()
{
	paths<-10
	count<-5000
	interval<-5/count
	sample<-matrix(0,nrow=(count+1),ncol=paths)
	for(i in 1:paths)
	{
		sample[1,i]<-5
		for(j in 2:(count+1))
		{
			sample[j,i]<-sample[j-1,i]+interval*0.06+((interval)^.5)*rnorm(1)*0.3
		}
	}	
	cat("E[W(2)] = ",mean(sample[2001,]),"\n")
	cat("E[W(5)] = ",mean(sample[5001,]),"\n")
	matplot(sample,main="Brownian",xlab="Time",ylab="Path",type="l")
}

sbrownian<-function() 
{
	paths<-10
	count<-5000
	interval<-5/count
	sample<-matrix(0,nrow=(count+1),ncol=paths)
	for(i in 1:paths)
	{
		sample[1,i]<-0
		for(j in 2:(count+1))
		{
			sample[j,i]<-sample[j-1,i]+((interval)^.5)*rnorm(1)
		}
	}	
	cat("E[W(2)] = ",mean(sample[2001,]),"\n")
	cat("E[W(5)] = ",mean(sample[5001,]),"\n")
	matplot(sample,main="Standard Brownian",xlab="Time",ylab="Path",type="l")
}
sbrownian()
Brownian()
ThirdBrown()