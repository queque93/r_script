#Arbre de décision(decision tree)

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
set.seed(123)
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

#Decision tree with party

library(party)
tree<-ctree(hd ~ ., data=train, controls = ctree_control(mincriterion = 0.9,minsplit = 68))
tree
plot(tree)

#prediction

predict(tree,test, type="prob")
predict(tree,test)#prediction en fonction des classes


#Decision tree with rpart

library(rpart)#install.packages("rpart.plot") depuis le cran
library(rpart.plot)#installer depuis le cran
tree1 <- rpart(hd ~ ., train)

rpart.plot(tree1)
rpart.plot(tree1,extra = 1)
rpart.plot(tree1,extra = 2)
rpart.plot(tree1,extra = 3)
rpart.plot(tree1,extra = 4)

#prediction
predict(tree1,test)

#Erreur de classification pour la donnée d'entrainement

cm<-table(pred=predict(tree,train),actual=train$hd)
print(cm)
(sum(diag(cm))/sum(cm))
tau<-1-(sum(diag(cm))/sum(cm))#taux d'erreur
tau
#Erreur de classification pour la donnée test

testpredict<-predict(tree,test)

cm1<-table(pred=testpredict,actual=test$hd)
print(cm1)
(sum(diag(cm1))/sum(cm1))
tau1<-1-(sum(diag(cm1))/sum(cm1))#taux d'erreur
tau1
