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
install.packages("caret")
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
library(caret)
set.seed(123)

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
pixel.minValue <- apply(mnist.dat.reduced[,-1], 2, min) #2 means by column
pixel.maxValue <- apply(mnist.dat.reduced[,-1], 2, max) #1 means by row
summary <- cbind(pixel.minValue, pixel.maxValue)
summary <- as.data.frame(summary)
useless.pixels <- summary[summary$pixel.minValue == summary$pixel.maxValue, ]
mnist.dat.reduced <- mnist.dat.reduced[,-c(2,3,4,5,9,10,12,14,15,16,29,30,44,58,72,114,128,142,156,170,184,185,197)]
#drawing 500 random samples
training.rows<-sample(nrow(mnist.dat.reduced),5000)
training.set<-mnist.dat.reduced[training.rows,] 
#using the remaining samples as a test set
test.set <-mnist.dat.reduced[-training.rows,]

#-----------------the regularized multinomial logit model (using the LASSO penalty)----------------------#
lasso.model<-cv.glmnet(as.matrix(training.set[,-1]),training.set[,1],family="multinomial",type.measure ="class")
print(lasso.model$lambda.min) #Best lambda value 0.001246766
plot(lasso.model)

lasso.pred<-predict(lasso.model,as.matrix(test.set[,-1]),s="lambda.min", type="class")

lasso.confmat<- table(lasso.pred,test.set[,1])
lasso.accuracy <- sum(diag(lasso.confmat))/sum(lasso.confmat) 

print(lasso.confmat)
print(lasso.accuracy)

#---------------------------------------svm--------------------------------------------------

tuned.parameters <- tune.svm(training.set[,-1],training.set[,1],gamma = 10^(-5:-1), cost = 10^(-3:1))
tuned.parameters$performances
summary (tuned.parameters)
#training.set <- training.set[, -c(6,8,13,17,18,31,86,100,196)]
#test.set <- test.set[, -c(6,8,13,17,18,31,86,100,196)]
training.set <- training.set[, -c(2,4,6,7,8,19,71,85,174)]
test.set <- test.set[, -c(2,4,6,7,8,19,71,85,174)]
mnist.svm.tuned <- svm(training.set[, -1],training.set[,1],cost = 1e+01, gamma = 1e-05)
mnist.svm.pred <- predict(mnist.svm.tuned,test.set[,-1])
mnist.svm.confmat <- table(mnist.svm.pred, test.set[,1])
mnist.svm.accuracy <- sum(diag(mnist.svm.confmat)/sum(mnist.svm.confmat))



#-----------------feed forward neural network----------------------#
x.train_labels<-mnist.dat.reduced[training.rows,1]
x.test_labels<-mnist.dat.reduced[-training.rows,1]
x.train_labels<-as.factor(x.train_labels)
x.test_labels<-as.factor(x.test_labels)
train_norm <- as.data.frame(lapply(mnist.dat.reduced[training.rows, -1], function(x) { 
  return(x / 255)
}))
test_norm <- as.data.frame(lapply(mnist.dat.reduced[-training.rows,-1], function(x) { 
  return(x / 255)
}))
train_labels_matrix = class.ind(x.train_labels)
head(x.train_labels)
TrainingParameters <- trainControl(method = "cv", number = 10)
grid_nn <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                       decay = seq(from = 0.1, to = 0.5, by = 0.1))
nn <- train(train_norm, x.train_labels,
            trControl= TrainingParameters,
            method = "nnet",
            tuneGrid = grid_nn,
            MaxNWts = 20000
)
nn
NNPredictions <-predict(nn, test_norm)
mnist.dat.nnet.confmat<- table(NNPredictions,x.test_labels)
sum(diag(mnist.dat.nnet.confmat))/sum(mnist.dat.nnet.confmat) 
#-----------------Mc Nemar test----------------------#
NNPredictions.is.correct <- sapply(c(1:length(NNPredictions)),function(index){
  if(NNPredictions[index] == x.test_labels[index]){
    return (1)
  }
  else (return (0))
})

lasso.pred.is.correct <- sapply(c(1:length(lasso.pred)),function(index){
  if(lasso.pred[index] == x.test_labels[index]){
    return (1)
  }
  else (return (0))
})

mnist.svm.pred.is.correct <- sapply(c(1:length(mnist.svm.pred)),function(index){
  if(mnist.svm.pred[index] == x.test_labels[index]){
    return (1)
  }
  else (return (0))
})
svm.vs.multinomial.conf.matrix <- table(mnist.svm.pred.is.correct, lasso.pred.is.correct)
mcnemar.test(svm.vs.multinomial.conf.matrix)
svm.vs.nn.conf.matrix <- table(mnist.svm.pred.is.correct, NNPredictions.is.correct)
mcnemar.test(svm.vs.nn.conf.matrix)
multinomial.vs.nn.conf.matrix <- table(lasso.pred.is.correct, NNPredictions.is.correct)
mcnemar.test(multinomial.vs.nn.conf.matrix)