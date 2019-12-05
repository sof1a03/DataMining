#-----------------install packages----------------------#
install.packages("OpenImageR")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("nnet")
install.packages("raster")
install.packages("e1071")
install.packages("caTools")
install.packages("glmnet")
#-----------------libraries----------------------#
library( OpenImageR)
library(ggplot2)
library(ggpubr)
library(dplyr)
theme_set(theme_pubr())
library(nnet)
library(raster)
library(e1071)
library(caTools)
library(glmnet)

#-----------------set up code( please change the repository name)----------------------#
mnist.dat <- read.csv("C:/Users/ponti/Documents/GitHub/DataMining/pattern recognition/mnist.csv")
#reducing the dimensions of the image by half
mnist.dat.reduced<- apply(mnist.dat[ , -1], 1, function(image){
  image.matrix <- matrix(image,nrow = 28, ncol = 28, byrow = TRUE)
  image.reduced <- down_sample_image(image.matrix, 2, gaussian_blur = FALSE)
  return (image.reduced)
})
mnist.dat.reduced <- t(mnist.dat.reduced)
mnist.dat.reduced <- cbind(mnist.dat[,1], mnist.dat.reduced)
mnist.dat.reduced <- as.data.frame(mnist.dat.reduced)

mnist.dat.reduced[,1]<-factor(mnist.dat.reduced[,1])
#drawing 500 random samples
training.rows<-sample(nrow(mnist.dat.reduced),5000)
training.set<-mnist.dat.reduced[training.rows,] 
#using the remaining samples as a test set
test.set <-mnist.dat.reduced[-training.rows,]

#-----------------the regularized multinomial logit model (using the LASSO penalty)----------------------#
lasso.model<-cv.glmnet(as.matrix(training.set[,-1]),training.set[,1],family="multinomial",type.measure ="class")
print(lasso.model$lambda.min) #Best lambda value 0.001246766
print(coef(lasso.model,s="lambda.min"))
plot(lasso.model)

lasso.pred<-predict(lasso.model,as.matrix(test.set[,-1]),s="lambda.min", type="class")

lasso.confmat<- table(lasso.pred,test.set[,1])
lasso.accuracy <- sum(diag(lasso.confmat))/sum(lasso.confmat) 

print(lasso.confmat)
print(lasso.accuracy)

#---------------------------------------svm--------------------------------------------------

mnist.svm.tune <- tune.svm(x.train[,-1],x.train[,1],cost=c(0.1,0.5,1,5), gamma=c(0.01,0.0,.05,0.1,0.5))

mnist.svm.tune$performances

mnist.svm.tuned <- svm(x.train[, -1],x.train[,1],type="C-classification",kernel="radial",scale = FALSE)

mnist.svm.pred <- predict(mnist.svm.tuned,x.test[,-1])

sum(diag(table(x.test[,1],mnist.svm.pred)))/nrow(x.test)



#-----------------feed forward neural network----------------------#
