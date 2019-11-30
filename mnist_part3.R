install.packages("e1071")
library(e1071)
library(caTools)
library(glmnet)
library(OpenImageR)

set.seed(123)
#---------------------------Preparing the data---------------------------------------------

mnist.dat<-read.csv("C:/Users/Jagmeet's PC/Documents/mnist.csv")
mnist.dat.reduced<-as.data.frame(down_sample_image(mnist.dat,2,gaussian_blur = FALSE))

mnist.dat.reduced$label<-factor(mnist.dat.reduced$label)

#-------------------------Splitting data----------------------------------------------------

train.rows<-sample(nrow(mnist.dat.reduced),5000)
x.train<-mnist.dat.reduced[train.rows,]
x.test<-mnist.dat.reduced[-train.rows,]

#--------------------------regularized multinomial logit mode-------------------------------

mnist.dat.lasso.cv<-cv.glmnet(as.matrix(x.train[,-1]),x.train[,1],family="multinomial",type.measure ="class")
mnist.dat.lasso.cv$lambda.min #Best lambda value 0.0009904169

mnist.dat.lasso.cv.pred<-predict(mnist.dat.lasso.cv,as.matrix(x.test[,-1]),type="class")
plot(mnist.dat.lasso.cv)

mnist.dat.lasso.cv.confmat<- table(x.test[,1],mnist.dat.lasso.cv.pred)

sum(diag(mnist.dat.lasso.cv.confmat))/sum(mnist.dat.lasso.cv.confmat) #accuracy 0.890375


#---------------------------------------svm--------------------------------------------------

mnist.svm.tune <- tune.svm(x.train[,-1],x.train[,1],cost=1:10)






