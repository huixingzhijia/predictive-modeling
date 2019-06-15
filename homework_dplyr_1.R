library(dplyr)
library(data.table)
library(gapminder)
library("readr")
library("ggplot2")
library(tidyr)


setwd("/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/high performance/week2/homework/healthcare2")


data <- gapminder
head(data)
#Dplyr

#1) 
data[1:30,]
data[data$year==1952,]
data[data$continent=="Africa",]
data[data$year==2007,'lifeExp']

#3) 

#select the top five countries that have the largest population. 
data %>% filter(year==1952) %>%  top_n(5,wt=pop)

#select the top five countries in each contient that have the highest population
data %>% filter(year==1952) %>% group_by(continent) %>% top_n(5,wt=pop)

data %>% filter(year==1987) %>% group_by(continent) %>% top_n(5,wt=pop)

data %>% filter(year==2007) %>% group_by(continent) %>% top_n(5,wt=pop)

data %>% filter(year==1952) %>% top_n(-5,wt=pop)

data %>% filter(year==1987) %>% top_n(-5,wt=pop)

data %>% filter(year==2007) %>% top_n(-5,wt=pop)

data %>% filter(year==1952) %>% top_n(5,lifeExp)

data %>% filter(year==1952) %>% top_n(-5,lifeExp)

#Calculate the average life expectancy by country across all years for only countries in Asia. 
#Which Country in Asia has the highest and lowest average life expectancy?

data %>% filter(continent=='Asia') %>% group_by(country) %>% 
  summarise(avg_life=mean(lifeExp,na.rm = T)) %>% 
  filter(avg_life==min(avg_life)|avg_life==max(avg_life))


#f. Create a new column that is pop*gdpPercap. Which countries have the highest value
#for this column?

data %>% mutate(new_column=pop*gdpPercap) %>% top_n(1,wt=new_column) %>% select(country)

#4)

cor(data$gdpPercap,data$lifeExp)

#5)

coef <- function(var){

dt <- data[data$country==var,]
lm.fit <- lm(year ~ lifeExp,data=dt)
return(lm.fit$coefficients[2])
}

coef("Mexico")

list <- unique(data$country)
df <- data.frame(do.call(rbind,lapply(list,coef)),country=list) 
df %>% top_n(1,wt=lifeExp)
df %>% top_n(-1,wt=lifeExp)


data <- gapminder
write.csv(data,"/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/high performance/week1/hw/gapminder.csv")



# homework 2

setwd("/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/high performance/week2/homework/healthcare2")
## R Markdown

library(data.table)
library(lubridate)
library(dplyr)
library(tidyverse)

clinic <- fread("Clinic.csv")
diseasemap <- fread("DiseaseMap.csv")
icdcodes<- fread("ICDCodes.csv")
insuranceprovider <- fread("InsuranceProvider.csv")
mortality <- fread("Mortality.csv")
OutpatientVisit <- fread("OutpatientVisit.csv")
Patient <- fread("Patient.csv")
PatientAnalytic <- fread("PatientAnalyticFile.csv")
patientInsurance <- fread("PatientInsurance.csv")
staff <- fread("Staff.csv")


head(Patient)
head(mortality)

df <- merge(Patient,mortality,by=c("PatientID"),all.x=T)
df$death <- ifelse(is.na(df$DateOfDeath),0,1)
df$Gender <- factor(df$Gender,levels = c("male","female"))
df <- df[!is.na(df$Gender),]

df %>% group_by(Gender) %>% summarise(mortality=sum(death,na.rm = F)/n())
#Yes men are more likely to die than women.

#2)
Patient$Gender <- factor(Patient$Gender,level=c("male","female"))
Patient <- Patient[!is.na(Patient$Gender)]
df2 <- merge(Patient,OutpatientVisit,by=c("PatientID"))
df2$VisitDate <- ymd(df2$VisitDate)
df2 <- df2[!is.na(df2$VisitDate)]


df3 <- gather(df2[,c("PatientID",'Gender',"ICD10_1","ICD10_2","ICD10_3")],key = "ICD10level",value = "ICD10",-c("PatientID","Gender"))

df4 <- merge(df3,diseasemap,by=c("ICD10"),all.x=T)
df4$Condition <- factor(df4$Condition)


a <- df4 %>% group_by(Condition,Gender) %>% summarise(num_peop_group=n_distinct(PatientID)) 

total <- length(unique((df4$PatientID)))
e <- df4 %>% group_by(Condition) %>% summarise(all_pct = n_distinct(PatientID)/total*100)


c <- df4 %>% group_by(Gender) %>% summarise(num_patient_gender=n_distinct(PatientID))
d <- merge(a,c,by=c("Gender"),all.x = T)
d$Percentage <- d$num_peop_group/d$num_patient_gender*100
f <- d %>% select(Gender,Condition,Percentage) %>% spread(key="Gender",value = "Percentage")

final <- merge(f,e,by=c("Condition"))


##Q3
#Calculate the mortality rate for every year between 2005 and 2018. 
#Is it generally increasing, or decreasing? Assume patients are only at risk of 
#death as of their first visit (in the outpatient Visit file). 
#Once they have died, they are no longer at risk in subsequent years...
#a. This is a harder question to answer than at first glance. 
#What should the denominator of patients be for every year? How will you calculate it?

mortality$DateOfDeath <- ymd(mortality$DateOfDeath)
mortality$death_year <- year(mortality$DateOfDeath)


death_summary <-  mortality %>% group_by(death_year) %>% summarise(num_death = n_distinct(PatientID)) 

OutpatientVisit$VisitDate <- ymd(OutpatientVisit$VisitDate)

visit <- OutpatientVisit[!is.na(OutpatientVisit$VisitDate)]

visit$year <- year(visit$VisitDate)

patient_at_risk <- visit %>% group_by(PatientID) %>% summarise(at_risk_year= min(year)) %>% distinct(PatientID,at_risk_year)

patient_death_year <- mortality[,c("death_year","PatientID")]

p_risk_death <- merge(patient_at_risk,patient_death_year,by.x=c("PatientID"),by.y=c("PatientID"),all.x=T)
p_risk_death$death_year <- ifelse(is.na(p_risk_death$death_year),2050,p_risk_death$death_year)

num_risk <- function(){
  
}


risk_function <- function(year){
  a <- nrow(p_risk_death[(p_risk_death$at_risk_year<=year) & (p_risk_death$death_year>=year),])
  return(a)
}

year_list <- 2005:2018

risk_num <- data.frame(year=year_list,risk_patient=do.call(rbind,lapply(year_list,risk_function)))
df <- merge(risk_num,death_summary,by.x=c("year"),by.y=c("death_year"))
df$mortality <- df$num_death/df$risk_patient

ggplot(df,aes(x=year,y=mortality))+geom_point()+theme_bw()

ggplot(df,aes(x=year,y=mortality))+geom_line()+theme_bw()


#
merge.function <- function(key){
  df <- at.risk.data.long[PatientID==key]
  hh <- data.frame(Year=seq(min(df$Year), max(df$Year), by=1))
  df.1 <- merge(df,hh,by="Year",all=T)
  df.1 <- na.locf(na.locf(df.1), fromFirst = TRUE)
  return(df.1)
}

a <- lapply(unique(at.risk.data.long$PatientID), merge.function)




