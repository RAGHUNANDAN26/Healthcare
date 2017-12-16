
STATES<-read.csv("states.csv",as.is = TRUE)
STATES

STATES$Uninsured.Rate..2010. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2010.))/100 
STATES$Uninsured.Rate..2015. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2015.))/100

STATES$Uninsured.Rate.Change..2010.2015.<-STATES$Uninsured.Rate..2015.-STATES$Uninsured.Rate..2010.
STATES$a.2020<-STATES$Uninsured.Rate.Change..2010.2015.+STATES$Uninsured.Rate..2015.
STATES$population<-STATES$Health.Insurance.Coverage.Change..2010.2015./STATES$Uninsured.Rate.Change..2010.2015.*-1

STATES$Average.Monthly.Tax.Credit..2016.<-as.numeric(sub("\\$","",STATES$Average.Monthly.Tax.Credit..2016.))

#lm-linear Modelling #In R, the lm()function can be used to create a simple regression model

ml<-lm(STATES$a.2020~STATES$Uninsured.Rate..2015.)
ml

summary(ml)

newdata<-data.frame(STATES$Uninsured.Rate..2015.)

STATES$newd<-predict(ml,newdata)

#function to neglect negative value 
for(i in 1:52){
  if(STATES$newd[i]<0){
    STATES$newd[i]<--1*STATES$newd[i]
  }else{
    STATES$newd[i]<-STATES$newd[i]*100
  }
}

STATES$Uninsured.Rate..2010.<-100*STATES$Uninsured.Rate..2010.
STATES$Uninsured.Rate..2015.<-100*STATES$Uninsured.Rate..2015.

library(reshape2)
library(ggplot2)

df<-data.frame(STATES$State,STATES$Uninsured.Rate..2010.,STATES$Uninsured.Rate..2015.,STATES$newd)
df
df.long<-melt(df)
ggplot(df.long,aes(STATES.State,value,fill=variable))+
 geom_bar(stat="identity",position="dodge")+
theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


library(shiny)
library(reshape2)
library(ggplot2)

df<-data.frame(STATES$State,STATES$Uninsured.Rate..2010.,STATES$Uninsured.Rate..2015.,STATES$newd)
df
df.long<-melt(df)   #molten or taking the 3 col which we used compare
 
#ui

ui<-fluidPage(
  h1("health insurence dataset"),  #header
  #img(height=100,width=100,source="C:\\Users\\Lenovo\\Desktop\\ELEMENTS\\www"),
  sidebarLayout(
    sidebarPanel(
      selectInput("statename","Choose a state",choices =STATES$State),width=3
    ),
    mainPanel(
      tableOutput("y"), 
      plotOutput("q")
    )
  ))


server<-shinyServer(function(input,output){
  
  output$y<-renderTable(  #to display plot out
    sf<-subset(STATES,STATES$State==input$statename)
  )
  output$q<-renderPlot(
    
    ggplot(df.long,aes(STATES.State==input$statename,value,fill=variable))+  #when we compare two argument we should aes fn
    geom_bar(stat="identity",position="dodge")+  
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
     #text should be in 45degree 
    
  )
  
})

shinyApp(ui=ui,server=server)  #to run or deploy shinyapp




