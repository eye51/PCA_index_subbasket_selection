calcTrackingError <-function (start,timeseries)
{


	y<-as.vector(timeseries)

	NoElements<-length(y)/3


	x1<-y[1:NoElements]
	x2<-y[(NoElements+1):(2*NoElements)]
	x3<-y[(2*NoElements+1):(3*NoElements)]

	a<-start[1]
	b<-start[2]


	c<- -a-b

	noemer<-numeric(NoElements)
	teller<-numeric(NoElements)

	noemer<-0
	teller<-0

	if (a < 0) noemer=noemer-a*x1 else teller=teller+a*x1
	if (b < 0) noemer=noemer-b*x2 else teller=teller+b*x2
	if (c < 0) noemer=noemer-c*x3 else teller=teller+c*x3


	e<-noemer/teller

	hh<-BastiaanADF(e,"c",1)

	stat<-hh@test$statistic

	return (stat)
}



suppressWarnings(min1<-nlm(BastiaanMin3Series,start,timeseries=combi))
