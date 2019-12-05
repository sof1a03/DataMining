install.packages("e1071")
library(e1071)
library(caTools)
library(glmnet)
library(OpenImageR)

set.seed(123)
#---------------------------Preparing the data---------------------------------------------

mnist.dat<-read.csv("C:/Users/Jagmeet's PC/Documents/mnist.csv")
mnist.dat.reduced<- apply(mnist.dat[1, -1], 1, function(image){
  image.matrix <- matrix(image,nrow = 28, ncol = 28, byrow = TRUE)
  image.reduced <- down_sample_image(image.matrix, 2, gaussian_blur = FALSE)
  return (image.reduced)
})

mnist.dat.reduced$label<-factor(mnist.dat.reduced$label)
sum(sapply(x.train, function(x) sum(is.na(x))))

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

mnist.svm.tune <- tune.svm(x.train[,-1],x.train[,1],cost=c(0.1,0.5,1,5), gamma=c(0.01,0.0,.05,0.1,0.5))

mnist.svm.tune$performances

mnist.svm.tuned <- svm(x.train[, -1],x.train[,1],type="C-classification",kernel="radial",scale = FALSE)

mnist.svm.pred <- predict(mnist.svm.tuned,x.test[,-1])

sum(diag(table(x.test[,1],mnist.svm.pred)))/nrow(x.test)







