library(ggplot2)

#Import train And test DataSet
titanic.train<- read.csv(file="train.csv",stringsAsFactors = FALSE,header=TRUE)
titanic.test<- read.csv(file="test.csv",stringsAsFactors = FALSE,header=TRUE)


#Add a missing Survived field in test dataset
titanic.test$Survived<-NA

#Add new fields Istrainset in train and test dataset for binding
titanic.train$Istrainset<-T
titanic.test$Istrainset<-F
#Combing the datasets using rbind
titanic.full<-rbind(titanic.train,titanic.test)


##########feature Engineering
#Create field Fsize(familysize) from SibSp
titanic.full$Fsize[titanic.full$SibSp == 1 | titanic.full$SibSp == 0] <- 'single'
titanic.full$Fsize[titanic.full$SibSp <= 4 & titanic.full$SibSp > 1] <- 'small'
titanic.full$Fsize[titanic.full$SibSp > 4] <- 'large'


#Create field Salute(title) from Name
for(i in 1:nrow(titanic.full)){
  if(any( grep("Miss",titanic.full$Name[i]))==T )  {  titanic.full$Salute[i]<-"Miss" }
  else  if( any( grep("Master",titanic.full$Name[i]))==T  ){ titanic.full$Salute[i]<-"Master" }
  else  if( any( grep("Mrs",titanic.full$Name[i]))==T ){  titanic.full$Salute[i]<-"Mrs" }
  else  if( any( grep("Dr",titanic.full$Name[i]))==T){  titanic.full$Salute[i]<-"Dr" } 
  else  if  ( any( grep("Mr",titanic.full$Name[i]))==T)  {  titanic.full$Salute[i]<-"Mr" }
  
}

#Data Cleaning

table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

table(is.na(titanic.full$Fare))
median.Fare<-median(titanic.full$Fare,na.rm = T)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-median.Fare


titanic.full[is.na(titanic.full$Age) & titanic.full$Salute =="Mr" ,"Age"]<- round(mean(titanic.full$Age[titanic.full$Salute == "Mr"], na.rm = TRUE))
titanic.full[is.na(titanic.full$Age) & titanic.full$Salute =="Mrs" ,"Age"]<- round(mean(titanic.full$Age[titanic.full$Salute == "Mrs"], na.rm = TRUE))
titanic.full[is.na(titanic.full$Age) & titanic.full$Salute =="Dr" ,"Age"]<- round(mean(titanic.full$Age[titanic.full$Salute == "Dr"], na.rm = TRUE))
titanic.full[is.na(titanic.full$Age) & titanic.full$Salute =="Miss" ,"Age"]<- round(mean(titanic.full$Age[titanic.full$Salute == "Miss"], na.rm = TRUE))
titanic.full[is.na(titanic.full$Age) & titanic.full$Salute =="Master" ,"Age"]<- round(mean(titanic.full$Age[titanic.full$Salute == "Master"], na.rm = TRUE))

#Convert the field as factor
titanic.full$Fsize<-as.factor(titanic.full$Fsize)
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Salute<-as.factor(titanic.full$Salute)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)
titanic.full$Survived<-as.factor(titanic.full$Survived)

#Separate the train and test data using 'Istrainset'
titanic.train1<-titanic.full[titanic.full$Istrainset==T,c("PassengerId","Survived","Pclass","Fsize","Salute","Sex","Embarked","Fare","Age")]
titanic.test1<-titanic.full[titanic.full$Istrainset==F,c("PassengerId","Survived","Pclass","Fsize","Salute","Sex","Embarked","Fare","Age")]

#Using logical Regression for "Binary Perdiction"
glm.model<- glm(Survived ~ Embarked + Fare + Age +Fsize+ Sex + Pclass + Salute ,data=titanic.train1,family =binomial(link = 'logit'))
summary(glm.model)
###step.mdl<- step(glm.model)

Survived.perdict<-predict(glm.model,titanic.test1 ,type = 'response')
results<-ifelse(Survived.perdict>0.6,1,0)
solution <- data.frame(PassengerID = titanic.test1$PassengerId, Survived = results)

#Save the solution in the csv format
write.csv(solution,"Kaggle_titanic_007.csv",row.names = F)


ggplot(titanic.full, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  xlab("Pclass") +
  facet_grid(.~Sex)+
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Pclass vs Sex vs Survived")