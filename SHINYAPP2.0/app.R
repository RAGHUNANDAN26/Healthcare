
#Reading the data to R 
#name of the dataset is STATES

STATES<-read.csv("states.csv",as.is = TRUE)
STATES

STATES$Uninsured.Rate..2010. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2010.))/100 
STATES$Uninsured.Rate..2015. <- as.numeric(sub("%","",STATES$Uninsured.Rate..2015.))/100

STATES$Uninsured.Rate.Change..2010.2015.<-STATES$Uninsured.Rate..2015.-STATES$Uninsured.Rate..2010.
STATES$UninsuredRateof2020assumption<-STATES$Uninsured.Rate.Change..2010.2015.+STATES$Uninsured.Rate..2015.
STATES$population<-STATES$Health.Insurance.Coverage.Change..2010.2015./STATES$Uninsured.Rate.Change..2010.2015.*-1
STATES$population2020<-STATES$Health.Insurance.Coverage.Change..2010.2015./STATES$Uninsured.Rate..2020.

STATES$Average.Monthly.Tax.Credit..2016.<-as.numeric(sub("\\$","",STATES$Average.Monthly.Tax.Credit..2016.))

#lm-linear Modelling #In R, the lm()function can be used to create a simple regression model

ml<-lm(STATES$UninsuredRateof2020assumption~STATES$Uninsured.Rate..2015.)
ml

summary(ml)

newdata<-data.frame(STATES$Uninsured.Rate..2015.)

STATES$Uninsured.Rate..2020.<-predict(ml,newdata)

#function to neglect negative value 
for(i in 1:52){
  if(STATES$Uninsured.Rate..2020.[i]<0){
    STATES$Uninsured.Rate..2020.[i]<--1*STATES$Uninsured.Rate..2020.[i]
  }else{
    STATES$Uninsured.Rate..2020.[i]<-STATES$Uninsured.Rate..2020.[i]*100
    
  }
}

STATES$Uninsured.Rate..2010.<-100*STATES$Uninsured.Rate..2010.
STATES$Uninsured.Rate..2015.<-100*STATES$Uninsured.Rate..2015.

# GGPLOT OF PREDICTED ANALYSIS --------------------------------------------

library(reshape2)
library(ggplot2)

df<-data.frame(STATES$State,STATES$Uninsured.Rate..2010.,STATES$Uninsured.Rate..2015.,STATES$Uninsured.Rate..2020.)
df
df.long<-melt(df)
ggplot(df.long,aes(STATES.State,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


library(shiny)
library(reshape2)
library(ggplot2)

df<-data.frame(STATES$State,STATES$Uninsured.Rate..2010.,STATES$Uninsured.Rate..2015.,STATES$Uninsured.Rate..2020.)
df
df.long<-melt(df)   #molten or taking the 3 col which we used compare




# SHINY APP  --------------------------------------------------------------

#install.packages("shinythemes")

library(shinythemes)


# UI ----------------------------------------------------------------------

ui<-fluidPage(
  theme = shinytheme("cerulean"),
  h1("HEALTH INSURENCE COVERAGE OF USA"),  #header
  
  sidebarLayout(
    sidebarPanel(
      selectInput("statename","Choose a state",choices =STATES$State),width=3
    ),
    mainPanel(

# Creating Tabset for all Outputs -----------------------------------------

            tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("q")),
                  tabPanel("Table", tableOutput("p")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("UninsuredrateVSstate",plotOutput("UNDRTvsST")),
                  tabPanel("HeathinsurenceVSstate",plotOutput("HICvsST")),
                  tabPanel("MarketplaceVSstate",plotOutput("MKPLvsST")),
                  tabPanel("MonthlyAvgtaxCredit",plotOutput("MonTxCrtvsST")),
                  tabPanel("Mediciateenrollment",plotOutput("MdctEnrlvsST"))
      )
    )
  )
)
  

# SERVER ------------------------------------------------------------------

server<-shinyServer(function(input,output){
  

# Plotting UninsuredRate of 2010,2015,2020 --------------------------------

    output$p<-renderTable(  
    sf<-subset(STATES[,c(1,2,3,17)],STATES$State==input$statename)
  )

# Displaying the Output as Table ------------------------------------------

    output$q<-renderPlot(
    
    ggplot(data=df.long,aes(x=STATES.State==input$statename,y=value,fill=variable))+
      geom_bar(stat="identity",position="dodge")+  
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
  )

# to Display the Summary Output of Uninsured rate of 2020 -----------------

      output$summary <- renderPrint({
    summary(STATES$Uninsured.Rate..2020.)

})
      output$UNDRTvsST<-renderPlot(
        ggplot(HealthCare_state, aes(State2, Health_Ins_55tCov_Ch_2010_2015)) + 
          geom_bar(stat="identity", fill = "firebrick") +
          coord_flip() +
          ggtitle("Health Insurance Change 2013-2015") +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("State") +
          ylab("Health Insurance Change 2013-2015") + 
          geom_hline(yintercept = mean(HealthCare_state$Health_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average
        
        
      )
      output$HICvsST<-renderPlot(
        ggplot(HealthCare_state, aes(State2, Health_Ins_Cov_Ch_2010_2015)) + 
          geom_bar(stat="identity", fill = "firebrick") +
          coord_flip() +
          ggtitle("Health Insurance Change 2013-2015") +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("State") +
          ylab("Health Insurance Change 2013-2015") + 
          geom_hline(yintercept = mean(HealthCare_state$Health_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average
        
      )
      output$MKPLvsST<-renderPlot(
        ggplot(HealthCare_state, aes(State2, Mktpl_Ins_Cov_Ch_2010_2015)) + 
          geom_bar(stat="identity", fill = "firebrick") +
          coord_flip() +
          ggtitle("Marketplace Health Insurance Change 2013-2015") +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("State") +
          ylab("Marketplace Health Insurance Change 2013-2015") + 
          geom_hline(yintercept = mean(HealthCare_state$Mktpl_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average
        
      )
      output$MonTaxCrtvsST<-renderPlot(
        ggplot(HealthCare_state, aes(State2, Avg_Mo_Tx_Credit)) + 
          geom_bar(stat="identity", fill = "red") +
          coord_flip() +
          ggtitle("Avg Monthly Tax Credit") +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("State") +
          ylab("Avg Monthly Tax Credit") + 
          geom_hline(yintercept = mean(HealthCare_state$Avg_Mo_Tx_Credit), color = "blue", linetype = "dotdash") # add national average
        
      )
     output$MdctEnrlvsST<-renderPlot(
       ggplot(aca_state2, aes(State2, Medicaid_Enroll_Ch_2013_2016, fill=State_Medicaid_Exp)) + 
         geom_bar(stat="identity") +
         coord_flip() +
         ggtitle("Medicaid Enrollment Change 2013-2016") +
         theme(plot.title = element_text(hjust = 0.5)) +
         xlab("State") +
         ylab("Medicaid Enrollment Change 2013-2016") +
         scale_fill_discrete(name="Medicaid\nExpansion") +
         geom_hline(yintercept = mean(aca_state2$Medicaid_Enroll_Ch_2013_2016), color = "blue", linetype = "dotdash") # add national average
       
     )
})

shinyApp(ui=ui,server=server)  #to run shinyapp


 

