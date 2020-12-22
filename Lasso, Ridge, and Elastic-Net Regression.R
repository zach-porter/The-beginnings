install.packages("glmnet")
library(glmnet)
library(dplyr)
set.seed(42)
n<-1000
p<-5000
real_p<-15
x<- matrix(rnorm(n*p),nrow = n, ncol = p)
y<-apply(x[,1:real_p],1,sum) +rnorm(n)
train_rows<- sample(1:n, .66*n)
x.train<-x[train_rows,]
x.test<-x[-train_rows,]
y.train<-y[train_rows]
y.test<-y[-train_rows]
alpha0.fit<- cv.glmnet(x.train, y.train, 
                       type.measure = "mse", 
                       alpha=0, 
                       family="gaussian")
alpha0.predict<- predict(alpha0.fit,
                         s=alpha0.fit$lambda.1se,
                         newx=x.test)
mean((y.test-alpha0.predict)^2)
alpha1.fit<- cv.glmnet(x.train, y.train, 
                                 type.measure = "mse", 
                                 alpha=1, 
                                 family="gaussian")
alpha1.predicted<- predict(alpha1.fit,
                           s= alpha1.fit$lambda.1se,
                           newx=x.test)
mean((y.test-alpha1.predicted)^2)
alpha0.5.fit<- cv.glmnet(x.train, y.train, 
                       type.measure = "mse", 
                       alpha=0.5, 
                       family="gaussian")
alpha0.5.predicted<- predict(alpha0.5.fit,
                           s= alpha0.5.fit$lambda.1se,
                           newx=x.test)
mean((y.test-alpha0.5.predicted)^2)
list.of.fits<-list()
for (i in 0:10) {
  fit.name<- paste0("alpha", i/10)
  
  list.of.fits[[fit.name]]<- cv.glmnet(x.train, y.train,
                                       type.measure = "mse",
                                       alpha= i/10,
                                       family="gaussian")
}
results<- data.frame()
for (i in 0:10) {
  fit.name<- paste0("alpha", i/10)
  
  predicted<- predict(list.of.fits[[fit.name]],
                      s=list.of.fits[[fit.name]]$lambda.1se,
                      newx=x.test)
  mse<- mean((y.test-predicted)^2)
  temp<-data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results<-rbind(results, temp)
}
results
