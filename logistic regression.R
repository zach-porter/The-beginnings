url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data1<- read.csv(url, header = FALSE)
head(data1)
#name the columns
colnames(data1)<- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","hd")
View(data1)
str(data1)

#loading in dplyr just in case for apply functions
library("dplyr")

#clean up data, apply NA to ?, turn numbers that represent categorical data into factors
data1[data1=="?"]<- NA
data1$sex[data1$sex == 0] <- "F"
data1$sex[data1$sex == 1] <- "M"
data1$sex<- as.factor(data1$sex)
data1$cp<- as.factor(data1$cp)
data1$fbs<- as.factor(data1$fbs)
data1$restecg<- as.factor(data1$restecg)
data1$exang<- as.factor(data1$exang)
data1$slope<- as.factor(data1$slope)

data1$ca<- as.integer(data1$ca)
data1$ca<- as.factor(data1$ca)

data1$thal<- as.integer(data1$thal)
data1$thal<- as.factor(data1$thal)

data1$hd<- ifelse(test = data1$hd == 0, yes = "Healthy", no = "Unhealthy")
data1$hd<- as.factor(data1$hd)

str(data1)

nrow(data1[is.na(data1$ca) | is.na(data1$thal),])

data1[is.na(data1$ca) | is.na(data1$thal),]
data1
nrow(data1)

xtabs(~hd+sex, data = data1)

xtabs(~ hd + cp, data = data1)

xtabs(~ hd + fbs, data = data1)

xtabs(~ hd + restecg, data = data1)#only 4 patients represented by the variable 1

xtabs(~ hd + exang, data = data1)

xtabs(~ hd + exang, data = data1)

xtabs(~ hd + slope, data = data1)

xtabs(~ hd + ca, data = data1)

xtabs(~ hd + thal, data = data1)

logistics<- glm(hd ~ ., data = data1, family = "binomial")
summary(logistics)

logistics<- glm(hd ~ sex + cp + trestbps + slope + ca + thal, data = data1, family = "binomial")
summary(logistics)

ll.null <- logistics$null.deviance/-2
ll.proposed <-logistics$deviance/-2
(ll.null-ll.proposed)/ll.null

1 - pchisq(2*(ll.proposed-ll.null), df=(length(logistics$coefficients)-1))

#ceate new data frame using logistics for probability of heart disease
predicted.data<- data.frame(probability.of.hd=logistics$fitted.values,hd=data1$hd)

#sort the data from low to high
predicted.data<- predicted.data[order(predicted.data$probability.of.hd, decreasing = FALSE),]

#create a new column that has the rank of each sample, from low to high probability
predicted.data$rank<- 1:nrow(predicted.data)

library(ggplot2)

library(cowplot)

ggplot(data = predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2)+
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")
ggsave("heart_disease_probablities.pdf")
