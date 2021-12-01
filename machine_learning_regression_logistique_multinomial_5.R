#Regression logistique multinomial


url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data<-read.csv(url, header= FALSE)
head(data)

#donnons un nom a chaque colonne

colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang",
                    "oldpeak","slope","ca","thal","hd")

#inspection des variables

head(data)
str(data)

#gestion des données manquantes et transformation des variables en leur type propre

data[data == "?"] <- NA
data[data$sex==0,]$sex<- "F"
data[data$sex==1,]$sex<- "M"


##transtypage

data$sex<-as.factor(data$sex)
data$cp<-as.factor(data$cp)
data$fbs<-as.factor(data$fbs)
data$restecg<-as.factor(data$restecg)
data$exang<-as.factor(data$exang)
data$slope<-as.factor(data$slope)

data$ca<-as.integer(data$ca)
data$ca<-as.factor(data$ca)

data$thal<-as.integer(data$thal)
data$thal<-as.factor(data$thal)
data$hd<-as.factor(data$hd) #Notre variable d'interêt mise sous forme de facteur
str(data)

#imputation 

rowSums(is.na(data))
sum(is.na(data))/(nrow(data))*100
##le pourcentage de données manquantes est faible dans ce jeu de données  je choisi de supprimer

###visualisation des données manquantes

install.packages("VIM")
library(VIM)
mis_plot<-aggr(data,col=c("blue","red"),labels=names(data),cex.axis=0.6,gap=2,ylab=c("Missing data","Pattern"),
               numbers=TRUE,sortVars=TRUE)

##supprimer les données manquantes

data<-data[!(is.na(data$ca)|is.na(data$thal)),]
nrow(data)

#Partionnement 

##Assurons nous que les malades du coeur sont repartis entre les hommes etles femmes
xtabs(~hd+sex,data)

set.seed(123)#pour etre sur le meme alea
ind<-sample(2,nrow(data),replace=TRUE,prob = c(0.8,0.2))
train<-data[ind==1,]
test<-data[ind==2,]
head(train)
head(test)
nrow(train)
nrow(test)

#prepa data

data$hd<-relevel(data$hd,ref="0")

#Developpement du model de regression  logistique multinomial

install.packages(nnet)#depuis le CRAN
library(nnet)
mymodel<-multinom(hd~.,train)
summary(mymodel)

#prediction

p1<-predict(mymodel,train)#predit les classes
print(p1)
p2<-predict(mymodel,train,type='prob')# predit par probabilité
print(p2)

#taux d'erreur de classement

cm <- table(predict(mymodel),train$hd)
print(cm)

taux_erreur<-1-(sum(diag(cm))/sum(cm))
taux_erreur
sum(diag(cm))/sum(cm)
#### taux de précision 70% 

# 2-tailed z test

z<-summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p<-(1-pnorm(abs(z),0,1))*2
p

#taux d'erreur pour la donnée test

pred<-predict(mymodel,test)
cm2 <- table(pred,test$hd)
print(cm2)

taux_erreur_2<-1-(sum(diag(cm2))/sum(cm2))
taux_erreur_2
sum(diag(cm2))/sum(cm2)
