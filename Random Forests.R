library(ggplot2)
library(cowplot)
library(randomForest)
url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data<- read.csv(url, header = FALSE)
head(data) ###columns don't have names, so let's fix that by labeling columns from the uci website
colnames(data)<- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)
str(data)
data[data=="?"]<- NA
data$sex[data$sex == 0] <- "F"
data$sex[data$sex == 1] <- "M"
data$sex<- as.factor(data$sex)

data$cp<- as.factor(data$cp)
data$fbs<- as.factor(data$fbs)
data$restecg<- as.factor(data$restecg)
data$exang<- as.factor(data$exang)
data$slope<- as.factor(data$slope)

data$ca<- as.integer(data$ca)
data$ca<- as.factor(data$ca)

data$thal<- as.integer(data$thal)
data$thal<- as.factor(data$thal)

data$hd<- ifelse(test = data$hd == 0, yes = "Healthy", no = "Unhealthy")
data$hd<- as.factor(data$hd)

str(data)

set.seed(42)
data.imputed<- rfImpute(hd~., data = data, iter=6)

model<- randomForest(hd~., data=data.imputed, proximity= TRUE)
model

oob.error.data<- data.frame(
  Trees= rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error= c(model$err.rate[,"OOB"],
           model$err.rate[,"Healthy"],
           model$err.rate[,"Unhealthy"])
)
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

oob.values<- vector(length = 10)
for(i in 1:10){
  temp.model<-randomForest(hd~.,data = data.imputed, mtry=i, ntree=1000)
  oob.values[i]<-temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
#in this case when mtry=3, which is the default value, 
#we get the lowest OOB error rate
min(oob.values)
model<- randomForest(hd~.,
                     data = data.imputed,
                     ntree=1000,
                     proximity=TRUE,
                     mtry=which(oob.values==min(oob.values)))
distance.matrix<- as.dist(1-model$proximity)

mds.stuff<- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE)

mds.var.per<- round(mds.stuff$eig/sum(mds.stuff$eig)*100,1)

mds.values<- mds.stuff$points
mds.data<- data.frame(Sample=rownames(mds.values),
                      X=mds.values[,1],
                      Y=mds.values[,2],
                      Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X,y=Y, label=Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1-", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2-", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1-Random Forest Proximities")