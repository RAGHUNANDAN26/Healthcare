
#Reading the data to R 
#name of the dataset is STATES

STATES<-read.csv("states.csv",as.is = TRUE)
STATES

STATES$Uninsured.Rate..2010. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2010.))
STATES$Uninsured.Rate..2015. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2015.))
STATES$Uninsured.Rate.Change..2010.2015.<-STATES$Uninsured.Rate..2015.-STATES$Uninsured.Rate..2010.

STATES$UninsuredRateof2020assumption<-STATES$Uninsured.Rate.Change..2010.2015.+STATES$Uninsured.Rate..2015.
STATES$population<-STATES$Health.Insurance.Coverage.Change..2010.2015./STATES$Uninsured.Rate.Change..2010.2015.*-1

STATES$Average.Monthly.Tax.Credit..2016.<-as.numeric(sub("\\$","",STATES$Average.Monthly.Tax.Credit..2016.))

#predictive using Mutiple Linear Regression

ml<-lm(STATES$UninsuredRateof2020assumption~STATES$Uninsured.Rate..2015.+STATES$Uninsured.Rate..2010.)

ml

plot(ml)

newdata<-data.frame(STATES$Uninsured.Rate..2015.)

STATES$Uninsured.Rate..2020.<-predict(ml,newdata)

STATES$Uninsured.Rate..2020.<-STATES$Uninsured.Rate..2020./100



#function to neglect negative value 

for(i in 1:52){
  if(STATES$Uninsured.Rate..2020.[i]<0){
    STATES$Uninsured.Rate..2020.[i]<--1*STATES$Uninsured.Rate..2020.[i]
  }else{
    STATES$Uninsured.Rate..2020.[i]<-STATES$Uninsured.Rate..2020.[i]*100
    
  }
}




