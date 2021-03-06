
#read the data from its directory

#..\\data\usa_states_healthcare.csv

STATES<-read.csv("usa_states_healthcare.csv",as.is = TRUE)
STATES

STATES$Uninsured.Rate..2010. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2010.))
STATES$Uninsured.Rate..2015. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2015.))
STATES$Uninsured.Rate.Change..2010.2015.<-STATES$Uninsured.Rate..2015.-STATES$Uninsured.Rate..2010.

STATES$UninsuredRateof2020assumption<-STATES$Uninsured.Rate.Change..2010.2015.+STATES$Uninsured.Rate..2015.
STATES$population<-STATES$Health.Insurance.Coverage.Change..2010.2015./STATES$Uninsured.Rate.Change..2010.2015.*-1

STATES$Average.Monthly.Tax.Credit..2016.<-as.numeric(sub("\\$","",STATES$Average.Monthly.Tax.Credit..2016.))

STATES$Uninsured.Rate..2020.<-STATES$Uninsured.Rate..2020./100

STATES <- STATES[!STATES$State == "United States", ]

summary(STATES)

View(STATES)

summary(STATES$State.Medicaid.Expansion..2016.)

#Changing True &false to 0 and 1

STATES$State.Medicaid.Expansion..2016.[STATES$State.Medicaid.Expansion..2016.=="True"]=1

STATES$State.Medicaid.Expansion..2016.[STATES$State.Medicaid.Expansion..2016.=="False"]=0

STATES$State.Medicaid.Expansion..2016.<-as.integer(STATES$State.Medicaid.Expansion..2016.)

View(STATES)


plot(STATES$State,STATES$State.Medicaid.Expansion..2016.)

logistic<-glm(State.Medicaid.Expansion..2016.~State,data=STATES,family = binomial)

summary(logistic)


# Confidence Intervel for coef regression ---------------------------------

confint(logistic)

exp(logistic$coefficients)

exp(confint(logistic))

logistic<-glm(State.Medicaid.Expansion..2016.~Uninsured.Rate.Change..2010.2015.,data=STATES,family = binomial)

#glm is a generalized lineam model 
#glm(formula, family=familytype(link=linkfunction), data=)
summary(logistic)


plot(logistic)





