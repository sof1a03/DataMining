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
mnist.dat.reduced<-as.data.frame(down_sample_image(mnist.dat,2,gaussian_blur = FALSE))
mnist.dat.reduced$label<-factor(mnist.dat.reduced$label)
training.rows<-sample(nrow(mnist.dat.reduced),5000)
training.set<-mnist.dat.reduced[train.rows,] #drawing 500 random samples
test.set <-mnist.dat.reduced[-train.rows,]

#-----------------the regularized multinomial logit model (using the LASSO penalty)----------------------#
lasso.model<-cv.glmnet(as.matrix(training.set[,-1]),training.set[,1],family="multinomial",type.measure ="class")
print(lasso.model$lambda.min) #Best lambda value 0.0009904169
print(coef(lasso.model,s="lambda.min"))
plot(lasso.model)

lasso.pred<-predict(mnist.dat.lasso.cv,as.matrix(x.test[,-1]),type="class")

lasso.confmat<- table(test.set[,1],lasso.pred)
lasso.accuracy <- sum(diag(lasso.confmat))/sum(lasso.confmat) 

print(lasso.confmat)
print(lasso.accuracy)
#-----------------feed forward neural network----------------------#
