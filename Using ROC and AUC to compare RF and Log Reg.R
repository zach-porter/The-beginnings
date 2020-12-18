install.packages("pROC")
install.packages("randomForest")
library(pROC)
library(randomForest)
set.seed(420)
num.samples<- 100
weight<-sort(rnorm(num.samples, mean = 172, sd = 29))
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/num.samples)), 
                yes=1, no=0)
obese
plot(x=weight, y=obese)
logistic_regresion<- glm(obese~weight,family=binomial)
lines(weight, logistic_regresion$fitted.values)
roc(obese, logistic_regresion$fitted.values, plot = TRUE, legacy.axes= TRUE, percent = TRUE,
    xlab= "False Positive Percentage",
    ylab= "True Positive Percentage",
    col= "Blue",
    lwd= 4,
    print.auc= TRUE)
rf<- randomForest(factor(obese)~weight)
roc(obese, logistic_regresion$fitted.values, 
    plot = TRUE, 
    legacy.axes= TRUE, 
    percent = TRUE,
    xlab= "False Positive Percentage",
    ylab= "True Positive Percentage",
    col= "#377eb8",
    lwd= 4,
    print.auc= TRUE)
plot.roc(obese, 
         rf$votes[,1], 
         percent= TRUE,
         col="#4daf4a",
         lwd=4,
         print.auc=TRUE,
         add=TRUE,
         print.auc.y=40)
legend("bottomright", 
       legend = c("Logistic Regression", "Random Forest"),
       col=c("#377eb8","#4daf4a"),
       lwd=4)