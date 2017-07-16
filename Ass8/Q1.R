controlvariate<-function(alpha)
{
	cat("control Variate Estimator\n\n")
	iterations<-10
	for(j in 1:4)
	{
		iterations<-iterations*10
		cat("For iterations = ",iterations,"\n")
		u<-runif(iterations)
		x<-exp(sqrt(u))
		y<-sqrt(u)
		c<--cov(x,y)/var(y)
		f<-(exp(sqrt(u))+c*(y-mean(y)))
		theta<-mean(f)
		cat("Confidence interval is::")
		v<-sqrt(var(f))
		n<-qnorm(1-(alpha/2),0,1)
		cat("( ",theta-((n*v)/sqrt(iterations))," , ",theta+((n*v)/sqrt(iterations))," ) ","\n")	
		cat("Variance reduction is::",((var(x)-var(f))/var(x))*100,"\n\n")
	}




}
antithetic<-function(alpha)
{
	cat("Antithetic Estimator\n\n")
	iterations<-10
	for(j in 1:4)
	{
		iterations<-iterations*10
		cat("For iterations = ",iterations,"\n")
		u<-runif(iterations)
		f1<-exp(sqrt(u))
		f<-((exp(sqrt(u))+exp(sqrt(1-u)))/2)
		theta<-mean(f)
		cat("Confidence interval is::")
		v<-sqrt(var(f))
		n<-qnorm(1-(alpha/2),0,1)
		cat("( ",theta-((n*v)/sqrt(iterations))," , ",theta+((n*v)/sqrt(iterations))," ) ","\n")	
		cat("Variance reduction is::",((var(f1)-var(f))/var(f1))*100,"\n\n")
	}
}
monteest<-function(alpha) 
{
	cat("Monte Estimator\n\n")
	iterations<-10
	for(j in 1:4)
	{
		iterations<-iterations*10
		cat("For iterations = ",iterations,"\n")
		u<-runif(iterations)
		f<-exp(sqrt(u))
		theta<-mean(f)
		cat("Confidence interval is::")
		v<-sqrt(var(f))
		n<-qnorm(1-(alpha/2),0,1)
		cat("( ",theta-((n*v)/sqrt(iterations))," , ",theta+((n*v)/sqrt(iterations))," ) ","\n\n")
	}
}
monteest(0.05)
antithetic(0.05)
controlvariate(0.05)