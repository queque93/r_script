#Nous travaillons avec une base de données issus data_base machine learning 

#création d'un objet  qui va contenir  la base de donnée

url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data<-read.csv(url, header= FALSE)
head(data)

#donnons un nom a chaque colonne
colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang",
                    "oldpeak","slope","ca","thal","hd")
#ce sont les noms listés sur le site web UCI

#inspection des variables

head(data)
str(data)

#gestion des données manquantes et transformation des variables en leur type propre

attach(data)
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

data$hd<-ifelse(test=data$hd == 0, yes = "healty", no="unhealthy")
data$hd<-as.factor(data$hd)

nrow(data[is.na(data$ca)|is.na(data$thal),])
data[is.na(data$ca)|is.na(data$thal),]

#Imputation des variables qualitatives

install.packages("VIM")
library(VIM)

##Visualisation des données manquantes

mis_plot<-aggr(data,col=c("blue","red"),labels=names(data),cex.axis=0.6,gap=2,ylab=c("Missing data","Pattern"),
               numbers=TRUE,sortVars=TRUE)

##imputation la methode du knn
data_imp<-kNN(data,variable=c("ca","thal"),k=5)
sum(is.na(data_imp))
mis_plot1<-aggr(data_imp,col=c("blue","red"),labels=names(data_imp),cex.axis=0.6,gap=2,ylab=c("Missing data","Pattern"),
               numbers=TRUE,sortVars=TRUE)

data_imp[c(88,167,193,267,288,303),]
data[is.na(data$ca)|is.na(data$thal),]
dim(data_imp)
dim(data)
data_imp<-data_imp[,-c(15,16)]
print(data_imp)

#tableau croisé

mat1<-xtabs(~hd+sex,data=data_imp)
mat2<-xtabs(~hd+cp,data=data_imp)
mat1<-as.table(mat1)
mat2<-as.table(mat2)
mat1
mat2
prop.table(mat1,1)#profil-ligne
prop.table(mat1,2)#profil-colonne
#graphique de tableau croisé

barplot(mat1)
barplot(mat2)
barplot(mat1,legend=TRUE,beside=TRUE,col=rainbow(2),main="heart disease by sex")
barplot(mat2,legend=TRUE,beside=TRUE,col=rainbow(2),main="heart disease by sex")
barplot(prop.table(mat1,1),legend=TRUE,col=rainbow(2),main="sex by heart disease")
barplot(prop.table(mat1,2),legend=TRUE,col=rainbow(2),main="sex by heart disease")

#Partition des données en train et test

set.seed(123)#pour etre sur le meme alea
ind<-sample(2,nrow(data_imp),replace=TRUE,prob = c(0.8,0.2))
train<-data_imp[ind==1,]
test<-data_imp[ind==2,]

head(train)
head(test)
nrow(train)
nrow(test)

#Modele de regression logistic

logistic<-glm(hd ~., data=train, family = "binomial")
summary(logistic)
logistic<-step(logistic)
model<-glm(hd ~ sex + cp + trestbps + slope + ca + thal, data=train, family = "binomial")
summary(model)

#prediction

p1<-predict(model,train,type='response')
head(p1)
head(train)

#misses classification - train data error

pred1<-ifelse(p1>0.5,"unhealthy","healthy")
cm<-table(predited=pred1,actual=train$hd)
##taux de mauvais classement
t_err=(1-(sum(diag(cm))/sum(cm)))*100
t_err
##taux de bien classé
t_cl=(sum(diag(cm))/sum(cm))*100
t_cl

#misses classification - test data error

p2<-predict(model,test,type='response')
pred2<-ifelse(p2>0.5,"unhealthy","healthy")
cm1<-table(predited=pred2,actual=test$hd)
cm1
####taux de bien classé
t_cl1=(sum(diag(cm1))/sum(cm1))*100
t_cl1

#Goodness-of-fit test (test de validité)
with(model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))
#la p-value<0.05 ,le modele est significatif.