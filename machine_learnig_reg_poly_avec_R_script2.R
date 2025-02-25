#Machine learning avec R (Regression polynomiale)

#1.visualisation du jeu de donn�e et inspection

names(Data_Famille)
Data_Famille$CIT<-as.factor(Data_Famille$CIT)
Data_Famille$LFP<-as.factor(Data_Famille$LFP)
str(Data_Famille)
sum(is.na(Data_Famille))
#il n'y a pas de donn�es manquantes
summary(Data_Famille)
pairs(Data_Famille[-c(1,18)])
cor(Data_Famille[-c(1,18)])

#2.Partition du jeu de donn�es en donn�e d'apprentissage et donn�e test

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

##detection des multicolin�arit�s

install.packages("faraway")
library(faraway)

vif(f_model)#pour visualiser les problemes de multicolinearit� dans le modele(model1) 
vif(reg) #pour visualiser les problemes de multicolinearit� dans le modele(reg)

#Diagnostiques de la regression

par(mfrow=c(2,2))
plot(reg)
Data_Famille[c(101,319,331),] #outliers
plot(f_model)

##la lin�arit� et l'homosc�dacit� ne semble pas �tre verifi�,cette courbe nous montre
##une tendance quadratique.

#Am�lioration du modele par un modele de regression polynomial

pol1<-step(lm(FAMINC~ LFP + WHRS + I(WHRS^2) + K618 + I(K618^2) + WA + I(WA^2) + HHRS+ I(HHRS^2) + HW + I(HW^2)+ MTR + I(MTR^2) + WW + I(WW^2),data=training))
summary(pol1)
par(mfrow=c(2,2))
plot(pol1)
vif(pol1)#il y a des problemes de multicolinearit� malgr� que R-squared adjusted soit elev� par rapport au
# modele de regression lin�aire multiple
#Prediction avec le modele polynomial

pred<-predict(pol1,testing)
head(pred)
head(testing)
#On remarque egalement que dans le modele lineaire les  valeurs predites sont plus
#proche des vrais valeurs de la variable d'int�r�t par rapport au modele polynomiale
anova(f_model,pol1)
#il y a des problemes de multicolin�arit� dans le modele polynomial

