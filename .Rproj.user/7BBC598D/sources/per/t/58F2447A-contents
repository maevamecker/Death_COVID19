# -------- APPLICATION CORONAVIRUS MARINA SERRANO DIEGO AND MAEVA MECKER ---------
#Librairies

library(data.table)
library(readr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(dplyr)

#-------------------------------------------------------------------------------
#STEP 1 : DATA SET POPULATION
#We import, check and clean data set population

World_Pop <-read_csv("worldPop.csv") #Import dataset and header
class(World_Pop) #Check the class
setDT(World_Pop) #Transform into a data.table
class(World_Pop) # Check the class again

#colnames(World_Pop) #Check the names of the columns

#Clearler indentificatoin of the columns
names(World_Pop)[names(World_Pop) == "Country.Name."] <- "Country Name" 
names(World_Pop)[names(World_Pop) == "2018"] <- "Population 2018"

#str(World_Pop) check the type of the variables

#We add a row for EU27 to be able later to calculate the death per capita for EU27

EU27 = c("Austria", "Belgium", "Bulgaria", "Croatia", 
         "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Germany", "Greece", 
         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
         "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
         "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")


EU27DT = World_Pop[`Country Name` %in% EU27]
head(EU27DT[,1:2])
colSums(EU27DT[,!(1)])
t(colSums(EU27DT[,!(1)]))
sumEU = cbind(data.table(A='EU27'), t(colSums(EU27DT[,!(1)])))
World_Pop2<-rbind(World_Pop,sumEU, use.names=FALSE)

#------------------------------------------------------------------------------
#STEP 2 : DATA SET COVID
#Import and check Data Set Covid

#Import Data set

CoronaD <-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

setDT(CoronaD) #Transform the data frame into data.table

#Change the column names so we dont have probles with / sign

#colnames(CoronaD) 

names(CoronaD)[names(CoronaD) =="Country/Region"] <- "Country_Region"
names(CoronaD)[names(CoronaD) =="Province/State"] <- "Province_State"


#Add a row for EU27

EU27DT = CoronaD[`Country_Region` %in% EU27][is.na(`Province_State`)]
head(EU27DT[,1:7])
EU27line<-cbind(Province_Region=NA ,Country_Name='EU27',Lat=NA, Long=NA, t(colSums(EU27DT[,!1:4])))
CoronaD2<-rbind(CoronaD,EU27line, use.names=FALSE)

#View(CoronaD2) #Check our database

#Keep the columns with the date's information
Dates<-colnames(CoronaD2[,5:255])

#Create a new Data Base with the vertical storage of the dates and the deaths
Corona_V = melt(CoronaD2, id.vars = 1:4, 
                measure.vars = Dates)

#Change the names to have a simpler information of what the columns contain
names(Corona_V)[names(Corona_V) == "variable"] <- "Date"
names(Corona_V)[names(Corona_V) == "value"] <- "Dead"

#Transform this new data set into a data.table to easier manupulate
setDT(Corona_V)

#Redefined the dates type
Corona_V$Date<-as.Date(Corona_V$Date, format = "%m/%d/%y")

#View(Corona_V) #Check our data table for verification

#-------------------------------------------------------------------------------------------
#STEP 3: In order to plot the lockdown dates for each country we add new data bases
#Import Data Base 1 that contains some of the countries lockdown dates

Lockdown1<-read_csv('CoronaLockdown.csv')

#Cleaning of this data base
head(Lockdown1)
Lockdown1[is.na(Lockdown1)] = ""
Lockdown1<-Lockdown1[Lockdown1$Place == "" , c(2,4,5)]

#Set to a data.table
setDT(Lockdown1)

#str(Lockdown1) check structure of our data table

#Redefined the dates type
Lockdown1$Start.date<-as.Date(Lockdown1$Start.date, "%d/%m/%y")
Lockdown1$End.date<-as.Date(Lockdown1$End.date, "%d/%m/%y")

#Import DataBase 2 that contains other countries lockdown dates
Lockdown2<-read.csv('CoronaLockdown2.csv')
#Set to a data.table
setDT(Lockdown2)

Lockdown2<-Lockdown2[, c(1,2,3)]
names(Lockdown2)[names(Lockdown2) == "Start"] <- "Start.date"
names(Lockdown2)[names(Lockdown2) == "End"] <- "End.date"

#str(Lockdown2) #Check the structure of our data

#Redefined the dates type
Lockdown2$Start.date<-as.Date(Lockdown2$Start.date, "%d/%m/%y")
Lockdown2$End.date<-as.Date(Lockdown2$End.date, "%d/%m/%y")

#str(Lockdown2) #Check the type has changed

#Combination of two Lockdown data sets

LOCKDOWN<-rbind(Lockdown1, Lockdown2)
#View(LOCKDOWN)

#We add a row for EU to be able to do an inner merge just after

EU.df = data.frame(Country = "EU27", Start.date = "", End.date = "" )

LOCKDOWN<-rbind(LOCKDOWN,EU.df, use.names=FALSE)


#-----------------------------------------------------------------------------------------
#STEP4 : Create an inner join with all the common countries in the three data tables : Lockdown, Corona_V, World_Pop

#New data set with the inner join data : Corona_V and World_Pop2
#We will keep only the rows of the countries that are in both Data tables

Inner<-merge(Corona_V, World_Pop2, by.x="Country_Region", by.y="Country Name", all= FALSE)

#View(Inner)

#-------

#CORONAVIRUS DATA: Inner join of : Corona_V, LOCKDOWN and World_Pop

CORONAVIRUS<-merge(Inner, LOCKDOWN, by.x="Country_Region", by.y="Country", all=FALSE)
class(CORONAVIRUS)#Check the class
CORONAVIRUS$Dead <- as.numeric(CORONAVIRUS$Dead)
#str(CORONAVIRUS) #Check it CORONAVIRUS$Dead have been converted to numeric

#-------

#CORONAVIRUS_PERCAPITA

#We add a new column to this data with a calculated variable = Dead_per_capita in per million
CORONAVIRUS$Dead_per_capita_Permillion<-((CORONAVIRUS$Dead)/(CORONAVIRUS$`Population 2018`))*1000000

#------
#Check
#View(CORONAVIRUS)

#-------------------------------------------------------------------------------------------
#Informaiton for the app

myPlot = function(SelectedCountries, scale, TypeDeath,TimeFrame,Ymax) {
  LockdownDate<- CORONAVIRUS[Country_Region %in% SelectedCountries & Province_State=="", Start.date]
  if (TypeDeath == "Deaths") {
    Ymax = Ymax / 100 * max(CORONAVIRUS[,Dead], na.rm = TRUE)
    p = ggplot(CORONAVIRUS[is.na(`Province_State`)],
               aes(x= Date, y = Dead, group = `Country_Region`)) + 
      geom_line(colour ="grey") + 
      geom_line(data = CORONAVIRUS[`Country_Region` %in% SelectedCountries ][is.na(`Province_State`)], aes(colour = `Country_Region`)) +
      scale_x_date(limits=c(as.Date(TimeFrame[1]),as.Date(TimeFrame[2]))) +
      scale_y_continuous(limits = c(NA,Ymax)) +
      geom_vline(xintercept= LockdownDate, colour ="black") +
      annotate("text", x = as.Date("2020/03/20"), y = 10000, label = c("Confinement",LockdownDate)) +
      ggtitle("Deaths from Covid 19", subtitle = "Data: John Hopkins") +
      theme(text = element_text(family = "Arial"))
  } else {
    Ymax = Ymax / 100 * max(CORONAVIRUS[,Dead_per_capita_Permillion], na.rm = TRUE )
    p = ggplot(CORONAVIRUS[is.na(`Province_State`)],
               aes(x= Date, y = Dead_per_capita_Permillion, group = `Country_Region`)) + 
      geom_line(colour ="grey") + 
      geom_line(data = CORONAVIRUS[`Country_Region` %in% SelectedCountries ][is.na(`Province_State`)], aes(colour = `Country_Region`)) +
      scale_x_date(limits=c(as.Date(TimeFrame[1]),as.Date(TimeFrame[2]))) +
      scale_y_continuous(limits = c(NA,Ymax)) +
      geom_vline(xintercept= LockdownDate, colour ="black") +
      annotate("text", x = as.Date("2020/03/20"), y = 10000, label = c("Confinement",LockdownDate)) +
      ggtitle("Deaths from Covid 19", subtitle = "Data: John Hopkins") +
      theme(text = element_text(family = "Arial"))
  }
  if (scale == "Linear scale") {
    p = p
  }
  else {
    if (TypeDeath == "Deaths") {scaleName = "Deaths (Logarithmic scale)"}
    else {scaleName = "Deaths per capita per million (Logarithmic scale)"}
    p = p +
      scale_y_log10(name = scaleName,
                    limits = c(1,Ymax))
  }
  return(p)
}


SelectedCountries = unique(CORONAVIRUS$Country_Region)


#--------------------------------------------------------------------------------
#This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Deaths from Covid-19"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Country",
                  "Choose country:",
                  choices=SelectedCountries),
      
      sliderInput(inputId = "Ymax", "y-max % of worst affected country", min = 0, max = 100 , value = 35 , step = 10 , format = "Percentage" ),
      
      radioButtons(inputId="YScale", "Scale y-axis", choices= c("Logarithmic scale", "Linear scale")),
      
      radioButtons(inputId="TypeDeath", "Deaths", choices= c("Deaths", "Deaths per capita %")),
      
      dateRangeInput(inputId="TimeFrame", "Chose time frame", start=min(as.Date(CORONAVIRUS$Date)), end=max(as.Date(CORONAVIRUS$Date)))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"))
  )
)



# Define server logic required to draw a histogram


server <- function(input, output) { 
  
  output$Plot <- renderPlot({ 
    myPlot(input$Country, input$YScale, input$TypeDeath, input$TimeFrame,input$Ymax) #, input$Ymax,  ,
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



