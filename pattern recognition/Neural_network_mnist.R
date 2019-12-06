library(nnet)
library(caret)



#---------------------------Preparing the data---------------------------------------------

mnist.dat<-read.csv("C:/Users/Jagmeet's PC/Documents/mnist.csv")

mnist.dat.reduced<-as.data.frame(down_sample_image(mnist.dat,2,gaussian_blur = FALSE))

#-------------------------Splitting data----------------------------------------------------

train.rows<-sample(nrow(mnist.dat.reduced),5000)
x.train_labels<-mnist.dat.reduced[training.rows,1]
x.test_labels<-mnist.dat.reduced[-training.rows,1]

#-------------------------as factoring labels-----------------------------------------------

x.train_labels<-as.factor(x.train_labels)
x.test_labels<-as.factor(x.test_labels)

#-------------------------nnet section-----------------------------------------------




train_norm <- as.data.frame(lapply(mnist.dat.reduced[train.rows, -1], function(x) { 
  return(x / 255)
}))
test_norm <- as.data.frame(lapply(mnist.dat.reduced[-train.rows,-1], function(x) { 
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

mnist.dat.nnet.confmat<- table(x.test_labels,NNPredictions)

sum(diag(mnist.dat.nnet.confmat))/sum(mnist.dat.nnet.confmat) #ACCURACY is 0.84