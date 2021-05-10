save.image("Titanic.RData")
library(dplyr)
library(ggplot2)
# Exploratory Data Analysis of Titanic Dataset

titanic<-read.csv("C:\\Users\\Pankaj Rana\\Documents\\GitHub\\Titatic_Survival\\titanic.csv",na.strings = c("","NA"))
str(titanic)

# Removing irrelavant coloumns from the Datasets

colnames(titanic)
titanic<-titanic[,c("Survived","Pclass","Sex","Age","SibSp","Parch","Embarked")]
head(titanic)

# converting to relevant datatypes

titanic$Sex<-as.factor(titanic$Sex)
titanic$Embarked<-as.factor(titanic$Embarked)
titanic$Survived<-as.factor(titanic$Survived)
titanic$Pclass<-as.factor(titanic$Pclass)
str(titanic)

#grouping the siblings in three groups: "0", "3 or less","more than 3" as 0,1&2 

unique(titanic$SibSp)
titanic$SibSp[titanic$SibSp<=3&titanic$SibSp!=0]<-1
nrow(filter(titanic,titanic$SibSp>=4))
titanic$SibSp[titanic$SibSp>=4]<-2
titanic$SibSp<-as.factor(titanic$SibSp)


str(titanic)

#grouping the Parch variable in three groups: "0", "3 or less","more than 3" as 0,1&2 

unique(titanic$Parch)
titanic$Parch[titanic$Parch<=3&titanic$Parch!=0]<-1
nrow(filter(titanic,titanic$Parch>=4))
titanic$Parch[titanic$Parch>=4]<-2
titanic$Parch<-as.factor(titanic$Parch)

str(titanic)

# Exploring individual data Variables

# Exploring Survived variable
# 0 = Not Survived, 1 =  Survived

summary(titanic$Survived)
# checking for missing values
sum(is.na(titanic$Survived))

# Exploring Pclass Variable
# 1= first class, 2= second Class, 3= third class

summary(titanic$Pclass)
sum(is.na(titanic$Pclass))

#Exploring Sex variable
summary(titanic$Sex)
sum(is.na(titanic$Sex))


#Exploring Age Variable
summary(titanic$Age)

# Imputing missing values
(177/891)*100# 19% are missing values

# values can be imputed with the group mean values of SibSp groups

mean(titanic$Age[titanic$SibSp==0], na.rm = TRUE)# mean age of 0 siblings
mean(titanic$Age[titanic$SibSp==1], na.rm=TRUE)# mean age of 3 or less sibings
mean(titanic$Age[titanic$SibSp==2], na.rm=TRUE)# mean age of more than 3 siblings

titanic$Age<-ifelse(is.na(titanic$Age)&titanic$SibSp==0, 31,titanic$Age)
titanic$Age<-ifelse(is.na(titanic$Age)&titanic$SibSp==1, 28,titanic$Age)
titanic$Age<-ifelse(is.na(titanic$Age)&titanic$SibSp==2, 8,titanic$Age)

summary(titanic$Age)

#Exploring SibSp variable
summary(titanic$SibSp)
sum(is.na(titanic$SibSp))

#exploring Parch Variable
summary(titanic$Parch)
sum(is.na(titanic$Parch))


#Exploring Embarked variable
summary(titanic$Embarked)

filter(titanic, is.na(Embarked))
#removing Embarked NA records from the dataset view small no.

index<-which(is.na(titanic$Embarked))

titanic<-titanic[-index,]

summary(titanic)
