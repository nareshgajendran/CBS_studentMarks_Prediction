library(ggplot2)
library(MASS)
library(psych)
library(dplyr)
library(stringr)


setwd("/Users/nareshgajendran/Desktop/BACP/Task 1")
getwd()

mydata <- read.csv("student-performance.csv")

#Descriptive stats#
names(mydata)
View(mydata)
summary(mydata)
str(mydata)
head(mydata)

#Data cleaning#
mydata$mm <- as.factor(gsub(',','',mydata$mm))
mydata$Semester <- as.factor(replace(mydata$Semester,mydata$Semester== "F","First"))
mydata$Parent.responsible.for.student <- as.factor(str_replace_all(mydata$Parent.responsible.for.student,"mom","Mother"))

levels(mydata$Topic)

library(corrplot)
library(RColorBrewer)

#Define Dummies#
mydata$Female <- ifelse(mydata$mm == "Female",1,0)
mydata$male <-   ifelse(mydata$mm == "Male",1,0)

mydata$SecA <- ifelse(mydata$Section.ID == "A",1,0)
mydata$SecB <- ifelse(mydata$Section.ID == "B",1,0)
mydata$SecC <- ifelse(mydata$Section.ID == "C",1,0)

mydata$Sem1 <- ifelse(mydata$Semester == "First",1,0)
mydata$Sem2 <- ifelse(mydata$Semester == "Second",1,0)

mydata$StudentWithFather <- ifelse(mydata$Parent.responsible.for.student == "Father",1,0)
mydata$StudentWithMother <- ifelse(mydata$Parent.responsible.for.student == "Mother",1,0)

mydata$ParentAnsSurvey <- ifelse(mydata$Parent.Answering.Survey == "Yes",1,0)
mydata$ParentNotAnsSurvey <- ifelse(mydata$Parent.Answering.Survey == "No",1,0)

mydata$ParSchoolSatisfactionGood <- ifelse(mydata$ParentschoolSatisfaction == "Good",1,0)
mydata$ParSchoolSatisfactionBad <- ifelse(mydata$ParentschoolSatisfaction == "Bad",1,0)

mydata$StudAbsDayUnder7 <- ifelse(mydata$StudentAbsenceDays == "Under-7",1,0)
mydata$StudAbsDayAbove7 <- ifelse(mydata$StudentAbsenceDays == "Above-7",1,0)

  
mydata$level <- with(mydata, ifelse(Total.Marks< 69, "Low-Level",
                             ifelse(Total.Marks <89, "Middle-Level"
                                    ,"High-Level")))


View(mydata)

#normalisation of other values in the data#

normalize <-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
mydata$norm.RaisedHand<-normalize(mydata$Raised.hand)

normalize <-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
mydata$norm.VisitedResouces<-normalize(mydata$Visited.resources)

normalize <-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
mydata$norm.ViewingAnnouncements<-normalize(mydata$Viewing.announcements)

normalize <-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
mydata$norm.DiscussionGroups<-normalize(mydata$Discussion.groups)

normalize <-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
mydata$norm.totalMarks<-normalize(mydata$Total.Marks)

#Data Visualisation#

par(mfrow = c(2,2))

boxplot(mydata$Total.Marks, horizontal = TRUE, col = "lightblue", main = "Total Marks")
hist(mydata$Total.Marks, main = "No of students by marks", ylab = "no of Students", xlab = "Marks", col = "green")
boxplot(mydata$Total.Marks ~ mydata$mm, main = "Marks - Male vs Female", col = "orange")
boxplot(mydata$Total.Marks ~ mydata$Section.ID, col = "grey", main = "Marks - Sectionwise split")


#this is to find the important variables in data#
attach(mydata)
str(mydata)
#lpm <- (mydatanorm.totalMarks ~ mm + Section.ID + Topic + Semester + Parent.responsible.for.student
       # + Raised.hand + Visited.resources + Viewing.announcements + Discussion.groups + Parent.Answering.Survey
      #  ParentschoolSatisfaction + StudentAbsenceDays + )



lpm0 <- (mydata$norm.totalMarks ~ Female + male + SecA + SecB + SecC + Sem1 + Sem2 + StudentWithFather
         + StudentWithMother + ParentAnsSurvey + ParentNotAnsSurvey + ParSchoolSatisfactionGood + ParSchoolSatisfactionBad
         + StudAbsDayUnder7 + StudAbsDayAbove7 + norm.RaisedHand + norm.VisitedResouces + norm.DiscussionGroups)
LPM.1<-lm(lpm0,mydata)
summary(LPM.1)
lpm1 <- (mydata$norm.totalMarks ~ Female + StudentWithFather + ParentAnsSurvey
         + StudAbsDayUnder7 + norm.RaisedHand + norm.VisitedResouces)
LPM.2<- lm(lpm1,mydata)
summary(LPM.2)
mean(LPM.2$residuals)
plot(LPM.2)

LPM.2.1 <- rlm(lpm1,mydata)
summary(LPM.2.1)

#The variables in the models are less than 5 hence they all are significant so this is derived from value influencing factors in the data#
#Plots on fitting lines#

vif(LPM.2)

#Coorelation between Residuals and explanatory variables#
cor.test(mydata$Total.Marks, LPM.2$residuals)

#Homocedasticity#

ncvTest(LPM.2)
spreadLevelPlot(LPM.2)

#Parsimony
library(leaps)
Null <- lm(mydata$Total.Marks ~1)
Full <- lm(mydata$norm.totalMarks ~ Female + StudentWithFather + ParentAnsSurvey + StudAbsDayUnder7 + norm.RaisedHand + norm.VisitedResouces)
step(Null,scope = list(lower=Null, upper=Full),direction = "forward")

#Split the data#

set.seed(1234)
pd<-sample(2,nrow(mydata),replace=TRUE, prob=c(0.7,0.3))

train<-mydata[pd==1,]
val<-mydata[pd==2,]
attach(train)

allvl <- (train$norm.totalMarks ~ Female + male + SecA + SecB + SecC + Sem1 + Sem2 + StudentWithFather
+ StudentWithMother + ParentAnsSurvey + ParentNotAnsSurvey + ParSchoolSatisfactionGood + ParSchoolSatisfactionBad
+ StudAbsDayUnder7 + StudAbsDayAbove7 + norm.RaisedHand + norm.VisitedResouces + norm.DiscussionGroups)

LPM.3<-lm(allvl,train)
summary(LPM.3)

impvl <- (train$norm.totalMarks ~  ParentAnsSurvey
           + StudAbsDayUnder7+ norm.RaisedHand + norm.VisitedResouces)
LPM.4<-rlm(impvl,train)
summary(LPM.4)

plot(LPM.3)


# Important Variables
vif(LPM.4)

#Predction

val$prediction <- predict(LPM.4, newdata = val)

#Model score#

MAPE(val$prediction, val$norm.totalMarks)


write.csv(val, file = "Validation.csv")

#END
 