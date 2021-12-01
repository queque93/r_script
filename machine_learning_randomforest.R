#Randomforest( Forêt aléatoire)

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

#randomforest

install.packages("randomForest")#depuis le cran
library(randomForest)
set.seed(222)
rf<-randomForest(hd~.,data = train)
rf
attributes(rf)
rf$confusion

#Prediction and confusion matrix- train data

install.packages("caret")#depuis le cran
library(caret)
p1<-predict(rf,train)
confusionMatrix(p1,train$hd)#fonction de la librairie caret

#Prediction with test data

p2<-predict(rf,test)
confusionMatrix(p2,test$hd)

#Error  rate of randomforest

plot(rf)
##les erreurs se stabilisent au niveau de 420 arbres
head(train)
#Tune mtry (les arbres qui ne sont pas utilisé dans le boostrap)

t<-tuneRF(train[,-14],train[,14],stepFactor = 0.6,plot = TRUE,ntreeTry = 420,trace = TRUE,improve = 0.05)

rf<-randomForest(hd~.,data = train, ntree=420, mtry=2, importance=TRUE, proximity=TRUE)
rf

##erreur apres amelioration- data train

p1<-predict(rf,train)
confusionMatrix(p1,train$hd)

##erreur apres amelioration- data test

p2<-predict(rf,test)
confusionMatrix(p2,test$hd)

#nombre de noeud pour les arbres

hist(treesize(rf),main="No of nodes for the  trees",col = "blue")

#importance des variables

varImpPlot(rf,sort = TRUE,n.var = 10,main = "TOP-10 variables importance")
importance(rf) 
varUsed(rf)

#Dependance partielle

partialPlot(rf,train,thal,"0")

#extraction d'un seul arbre

getTree(rf,1,labelVar = TRUE)

#multi-dimension scaling  plot of proximity matrix
MDSplot(rf, train$hd)

