#Machine learning avec R (Regression linéaire multiple)

#1.visualisation du jeu de donnée et inspection

names(Data_Famille)
Data_Famille$CIT<-as.factor(Data_Famille$CIT)
Data_Famille$LFP<-as.factor(Data_Famille$LFP)
str(Data_Famille)
sum(is.na(Data_Famille))
#il n'y a pas de données manquantes
summary(Data_Famille)
pairs(Data_Famille[-c(1,18)])
cor(Data_Famille[-c(1,18)])

#2.Partition du jeu de données en donnée d'apprentissage et donnée test

set.seed(123)
ind<-sample(2,nrow(Data_Famille),replace = TRUE,prob = c(0.8,0.2))
training<-Data_Famille[ind==1,]
testing <-Data_Famille[ind==2,]
head(training)
head(testing)
cbind(summary(training$WHRS),summary(testing$WHRS))

#Modelisation

model<-lm(FAMINC~.,data=training)
model
summary(model)
reg<-step(model)
summary(reg)
model1<-lm(FAMINC~LFP+WHRS+K618+WA+WW+HHRS+HE+HW+MTR+AX ,data = training)
f_model<-step(model1)
summary(f_model)
anova(reg,f_model)
str(training)
pairs(training[-c(1,18)])

##detection des multicolinéarités

install.packages("faraway")
library(faraway)

vif(f_model)#pour visualiser les problemes de multicolinearité dans le modele(model1) 
vif(reg) #pour visualiser les problemes de multicolinearité dans le modele(reg)

#Diagnostiques de la regression

par(mfrow=c(2,2))
plot(reg)
Data_Famille[c(101,319,331),] #outliers
plot(f_model)

##la linéarité et l'homoscédacité ne semble pas être verifié
##Nous utilisons donc un modèle polynomiale dans un autre script

#Prediction avec f_model

pred<-predict(f_model,testing)
head(pred)
head(testing)

#prediction pour un individu en particulier
testing
id_50<-predict(f_model,data.frame(LFP="1",WHRS=240,K618=2,WA=30,WW=2.6375,HHRS=1864,
                          HW=3.4077,MTR=0.7815))
print(id_50)
