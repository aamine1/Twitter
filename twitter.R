#uploading the data

twitter <- read.csv("C:/Users/mitadm/Desktop/twitter.csv")
users <- read.csv("C:/Users/mitadm/Desktop/users.csv")
pairs <- read.csv("C:/Users/mitadm/Desktop/pairs.csv")

#splitting the data

set.seed(10)
library(caTools)
split=sample.split(twitter$Choice,SplitRatio=0.5)
twitterTrain=subset(twitter, split==TRUE)
twitterTest=subset(twitter, split==FALSE)

#logistic regression

logmodel=glm(Choice~.-A_ID - B_ID,data=twitterTrain,family=binomial)
summary(logmodel)
logmodel=glm(Choice~.-A_ID - B_ID - B_mentions_received - A_mentions_received  - A_posts - B_posts,data=twitter,family=binomial)
summary(logmodel)
Prediction=predict(logmodel,newdata=twitterTrain,type="response")
twitterTrain$Prediction=Prediction
table(twitterTrain$Choice, Prediction>0.5)
(830+1132)/nrow(twitterTest)
twitterTest$Prediction=Prediction
table(twitterTest$Choice, Prediction>0.5)
(862+1144)/nrow(twitterTest)

#CART Model

library(caret)
library(e1071)
folds=trainControl(method="cv",number=10)
cpValues=expand.grid(.cp=seq(0.001,0.5,0.001))
#cp=0.004
train(Choice~.,data=twitterTrain,method="rpart",trControl=folds,tuneGrid=cpValues)

library(rpart)
library(rpart.plot)
twitterTree=rpart(Choice~.,data=twitterTrain, method="class", cp=0.005)
prp(twitterTree)
PredictCart=predict(twitterTree, type="class")
table(twitterTrain$Choice,PredictCart)
(1019+1147)/nrow(twitterTrain)
twitterTreeTest=predict(twitterTree,newdata=twitterTest, type="class")
table(twitterTest$Choice,twitterTreeTest)
(1016+1086)/nrow(twitterTest)
library(ROCR)
PredictROC=predict(twitterTree)
PredictROC=PredictROC[,2]
pred=prediction(PredictROC, twitterTrain$Choice)
perf=performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred,"auc")@y.values)

#Random Forest

library(randomForest)
twitterForest=randomForest(Choice~.,data=twitterTrain,ntree=500,nodesize=25)
PredictForest=predict(twitterForest)
table(twitterTrain$Choice,PredictForest>0.5)
(1027+1090)/nrow(twitterTrain)
PredictForestTest=predict(twitterForest,newdata=twitterTest)
table(twitterTest$Choice,PredictForestTest>0.5)
(1062+1090)/nrow(twitterTest)
PredictROCforest=predict(twitterForest)
predforest=prediction(PredictROCforest, twitterTrain$Choice)
perfforest=performance(predforest,"tpr","fpr")
plot(perfforest)
as.numeric(performance(predforest,"auc")@y.values)

PredictForestTest=predict(twitterForest,newdata=pairs)

#Graph Vizualisation

unique_users=users[1:546,]
pairs$score=PredictForestTest 
for (i in 1:546){
  unique_users$score[i]=sum(pairs[((i-1)*545+1):(545*i),20])/545
}
library(igraph)
edges_tmp=subset(pairs,score>=0.5 & A_ID<10 & B_ID<10)
edges=data.frame(edges_tmp$A_ID,edges_tmp$B_ID)
g = graph.data.frame(edges, TRUE, c(seq(0,9,1)))
V(g)$size =20*unique_users$score

#Hierarchical Clustering

#scaling the data
new_users=unique_users
new_users$A_follower_count=(unique_users$A_follower_count-min(unique_users$A_follower_count))/(max(unique_users$A_follower_count)-min(unique_users$A_follower_count))
new_users$A_following_count=(unique_users$A_following_count-min(unique_users$A_following_count))/(max(unique_users$A_following_count)-min(unique_users$A_following_count))
new_users$A_listed_count=(unique_users$A_listed_count-min(unique_users$A_listed_count))/(max(unique_users$A_listed_count)-min(unique_users$A_listed_count))


UsersDistance=dist(new_users[,c(1,10)],method="euclidean")
Clusterusers=hclust(UsersDistance,method="ward.D")
plot(Clusterusers,labers=FALSE)
rect.hclust(Clusterusers,4)

ClusterGroups=cutree(Clusterusers,k=4)
table(ClusterGroups)

newCluster1=subset(new_users,ClusterGroups==1)
newCluster2=subset(new_users,ClusterGroups==2)
newCluster3=subset(new_users,ClusterGroups==3)
newCluster4=subset(new_users,ClusterGroups==4)

#kmeans

k=4

set.seed(100)
KMC=kmeans(new_users[,c(1,10)],centers=k, iter.max=1000)
str(KMC)
ClusterGroups2=KMC$cluster
str(ClusterGroups2)
table(ClusterGroups2)

newCluster1=subset(new_users,ClusterGroups2==1)
newCluster2=subset(new_users,ClusterGroups2==2)
newCluster3=subset(new_users,ClusterGroups2==3)
newCluster4=subset(new_users,ClusterGroups2==4)

new_users$cluster=ClusterGroups2
new_users$cluster2=ClusterGroups2

tapply(new_users$score, ClusterGroups2, mean)

for (i in 1:546){
  if (new_users$cluster2[i]==4) new_users$cluster[i]=1
  if (new_users$cluster2[i]==2) new_users$cluster[i]=2
  if (new_users$cluster2[i]==1) new_users$cluster[i]=3
  if (new_users$cluster2[i]==3) new_users$cluster[i]=4
}

#final data set (with the cluster information)

final_users=new_users[,c(1,2,3,4,5,6,7,8,11)]

#CART Model

set.seed(1000)
library(caTools)
split=sample.split(final_users$cluster,SplitRatio=0.7)
finalTrain=subset(final_users, split==TRUE)
finalTest=subset(final_users, split==FALSE)

train(cluster~.,data=finalTrain,method="rpart",trControl=folds,tuneGrid=cpValues)
finalTree=rpart(cluster~.,data=finalTrain, method="class", cp=0.001)
prp(finalTree)
PredictCart=predict(finalTree, type="class")
table(finalTrain$cluster,PredictCart)
(64+136+55+91)/nrow(finalTrain)
finalTreeTest=predict(finalTree,newdata=finalTest, type="class")
table(finalTest$cluster,finalTreeTest)
(29+58+26+34)/nrow(finalTest)

