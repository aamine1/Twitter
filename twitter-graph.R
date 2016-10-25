logit=glm(Choice~.,data=train,family=binomial)
summary(logit)
library('igraph')
g = graph.data.frame(edges, TRUE, users)
plot(g, vertex.label=NA,vertex.size=5,edge.arrow.size=0.2)
giant.component <- function(graph) { 
     cl <- clusters(graph) 
     induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))}
giant=giant.component(g)
vcount(giant)
ecount(giant)
incident(giant, "1", mode= "out")
incident(giant, "1", mode= "in")
V(giant)$size=1.3*sqrt(degree(giant,mode="out"))
plot(giant, vertex.label=NA,edge.arrow.size=0.2,layout=layout.kamada.kawai)
#
sub_train=subset(train,train$A_follower_count>100000)
sub_train=subset(sub_train,sub_train$B_follower_count>100000)
sub_users=subset(users,users$follower_count>100000)
sub_edges=subset(edges,edges$A_ID %in% sub_users$ID)
sub_edges=subset(sub_edges,sub_edges$B_ID %in% sub_users$ID)
sub_g=graph.data.frame(sub_edges, TRUE, sub_users)
sub_giant=giant.component(sub_g)
V(sub_giant)$size=degree(sub_giant,mode="out")
plot(sub_giant, vertex.label=NA,edge.arrow.size=0.2,layout=layout.kamada.kawai)
#
linear=lm(A_mentions_received~A_follower_count,data=sub_train)
summary(linear)
library(ggplot2)
ggplot(sub_train, aes(x = log(A_posts), y = A_follower_count)) + geom_point(color = "green", size = 3, shape = 17)+ ggtitle("Followers vs. Posts") + xlab("Number of posts") + ylab("Number of followers")+geom_smooth(method=lm)
ggplot(sub_train, aes(x = log(A_follower_count), y = log(A_mentions_received))) + geom_point(color = "green", size = 3, shape = 17)+ ggtitle("Mentions vs. Followers") + xlab("log(Number of Followers)") + ylab("log(Number of Mentions)")+geom_smooth(method=lm)


