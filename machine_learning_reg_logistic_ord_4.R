#regression logistic avec d'intérêt ordinal

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


data$hd<-as.ordered(data$hd) #Notre variable d'interêt mise sous forme ordinale

#sain=0 ,faible=1,moyen=2,grave=3 ,severe=4
str(data)
#imputation 
rowSums(is.na(data))
sum(is.na(data))/(nrow(data))*100
##le pourcentage de données manquantes est faible dans ce jeu de données  je choisi de 

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

#Modele ordinal regression logistic

library(MASS)
model<-polr(hd~.,train,Hess = TRUE)
summary(model)

#calcul de la p-value

(ctable <- coef(summary(model)))
p<-pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
(ctable<-cbind(ctable,"p value" = p))
model<-polr(hd ~ sex+cp+trestbps+slope+ca+thal,train,Hess = TRUE)
summary(model)

#prediction

pred<-predict(model,train,type='prob')#type 'prob'a enlever lors du calcul de la matrice de confusion
print(pred,digits = 3)

#confusion matrix and error for training data

(cm <- table(pred, train$hd))
taux_erreur=1-sum(diag(cm))/sum(cm)
taux_erreur
#un taux d'erreur de 37.76% soit un taux de bonne prediction de 62.24%

#confusion matrix and error for testing data

pred1<-predict(model,test)
print(pred1,digits = 3)
(cm1 <- table(pred1, test$hd))
taux_erreur1 = 1-sum(diag(cm1))/sum(cm1)
taux_erreur1
#le taux d'erreur est beaucoup elevé pour choisir un tel modele pour predire heart disease
