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

#----------------set up code( please change the repository name)----------------------#
mnist.dat <- read.csv("C:/Users/ponti/Documents/GitHub/DataMining/pattern recognition/mnist.csv")
mnist.dat$label<-factor(mnist.dat$label) #Converting label to factor

#-----------------class distribution----------------------#
ggplot(data=mnist.dat, aes(label)) +geom_bar(fill = "#0073C2FF") +theme_pubclean()

frequency.dataframe <- mnist.dat %>%
  group_by(label) %>%
  summarise(counts = n())
frequency.dataframe

ggplot(frequency.dataframe, aes(x = label, y = counts)) + 
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() # histrogram with quantity of the bars

frequency.dataframe <- frequency.dataframe %>%
  arrange(desc(label)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
frequency.dataframe
ggpie(frequency.dataframe, x = "prop", label = "prop",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "label", color = "white",
  palette = "jco"
) #pie chart


#-----------------summary statistics----------------------#
pixel.minValue <- apply(mnist.dat[,-1], 2, min) #2 means by column
pixel.maxValue <- apply(mnist.dat[,-1], 2, max) #1 means by row
pixel.avgValue <- apply(mnist.dat[,-1], 2, mean)

pixel.nwhite <- apply(mnist.dat[,-1], 2, function(pixel){
  return (length(pixel[pixel ==0]))
})
pixel.nblack <- apply(mnist.dat[,-1], 2, function(pixel){
  return (length(pixel[pixel ==255]))
})
mean.minValue <- mean(pixel.minValue)
mean.maxValue <- mean( pixel.maxValue)
mean.avgValue <- mean(pixel.avgValue)
mean.nwhite <- mean(pixel.nwhite)
mean.nblack <- mean(pixel.nblack)
summary <- cbind(pixel.minValue, pixel.maxValue, pixel.avgValue, pixel.nwhite, pixel.nblack)
summary <- as.data.frame(summary)
useless.pixels <- summary[summary$pixel.minValue == summary$pixel.maxValue, ]
useless.pixels.name <- row.names(useless.pixels)
bottom.pixels <- summary[summary$pixel.minValue == 0, ]
top.pixels <- summary[summary$pixel.maxValue == 255, ]
num.bottom.pix <- nrow(bottom.pixels)
num.top.pix <- nrow(top.pixels)
#-----------------analysis of the dark points----------------------#
mnist.dat$darks <- apply(mnist.dat[,c(2:785)], 1, function( sample){
  return (length(sample[sample > 128]))
} )
blacks.mean <- aggregate(mnist.dat$blacks,by=list(mnist.dat$label),FUN=mean)
colnames(blacks.mean) <- c("label", "mean")
ggplot(data=blacks.mean, aes(x=label,y=mean))+geom_bar(stat="identity") #Depicting average number of black points.
imageShow(matrix(as.numeric(mnist.dat[380,-c(1,786)]),nrow=28,ncol=28,byrow=T)) #image with no filter
#verifying the filter
sample.image <- as.numeric(mnist.dat[380,-c(1,786)])
sample.image <- sapply(sample.image, function(pixel){
  if (pixel > 128){
    return (pixel)
  }
  else return (0)
})
                       
imageShow(matrix(sample.image,nrow=28,ncol=28,byrow=T)) #image with no filter

#-----------------how much ink ("density") ( mean and standard deviation)----------------------#
mnist.dat$density <- apply(mnist.dat[,c(2:785)], 1, sum)
density.mean <- aggregate(mnist.dat$density,by=list(mnist.dat$label),FUN=mean)
colnames(density.mean) <- c("label", "mean")
print(density.mean)
ggplot(data=density.mean, aes(x=label,y=mean))+geom_bar(stat="identity") #Depicting average intensity. 
#standard deviation
density.sd <- aggregate(mnist.dat$density,by=list(mnist.dat$label),FUN=sd)
print(density.sd)
colnames(density.sd) <- c("label", "sd")
ggplot(data=density.sd, aes(x=label,y=sd))+geom_bar(stat="identity") #Depicting standard deviation.  

#-----------------simple multinomial model ("density")----------------------#
mnist.dat$density <- as.vector(scale(mnist.dat$density))
input <- as.data.frame(mnist.dat$density, row.names = NULL, optional = TRUE)
colnames(input) <- "density"
multinom.model<-multinom(label~density,data = mnist.dat, maxit = 1000)
summary(multinom.model)
multinom.pred <- predict(multinom.model, input, type = "class")
multinom.conf.mat <- table(multinom.pred,mnist.dat$label) #confusion matrix
multinom.accuracy <- sum(diag(multinom.conf.mat))/sum(multinom.conf.mat)
print (multinom.conf.mat)
print(multinom.accuracy)

#-----------------new feature multinomial model ( distance of the dark points)----------------------#
mnist.dat$mean.distance <- apply(mnist.dat[,c(2:785)],1, function(current.example){
  current.example <- unlist(current.example)
  current.example <- as.vector(current.example)
  index.row <- lapply(c(0:27), function(row){
    rep(row, 28)
  })
  index.row <- unlist(index.row, recursive = FALSE)
  index.column <- lapply(c(0:27), function(column){
    c(0:27)
  })
  index.column <- unlist(index.column, recursive = FALSE)
  current.example.with.indexes <- cbind(current.example, index.row, index.column)
  current.example.points <- current.example.with.indexes[current.example.with.indexes[,1] > 128, ]
  mean.row <- current.example.points[1,2]
  mean.column <- current.example.points[1,3]
  point.distances <- lapply(c(1:nrow(current.example.points)), function(point){
    example <- current.example.points[point,]
  return ( ((pointDistance(current.example.points[point,-1], cbind(mean.row,mean.column), lonlat = FALSE))^2))
  })
  point.distances <- unlist (point.distances, recursive = FALSE)
  return (mean(point.distances))
})
#average value
mean.distance.mean <- aggregate(mnist.dat$mean.distance,by=list(mnist.dat$label),FUN=mean)
colnames(mean.distance.mean) <- c("label", "mean")
print(mean.distance.mean)
ggplot(mean.distance.mean, aes(x=label,y=mean))+geom_bar(stat="identity") #Depicting average distance to the center. 
#standard deviation
mean.distance.sd <- aggregate(mnist.dat$mean.distance,by=list(mnist.dat$label),FUN=sd)
colnames(mean.distance.sd) <- c("label", "sd")
print(mean.distance.sd)
ggplot(data=mean.distance.sd, aes(x=label,y=sd))+geom_bar(stat="identity") #Depicting standard deviation.  
#-----------------simple multinomial model ( with the second feature)----------------------#  
mnist.dat$mean.distance <- as.vector(scale(mnist.dat$mean.distance))
input <- as.data.frame(mnist.dat$mean.distance, row.names = NULL, optional = TRUE)
colnames(input) <- "mean.distance"
multinom.model<-multinom(label~mean.distance,data = mnist.dat, maxit = 1000)
summary(multinom.model)
multinom.pred <- predict(multinom.model, input, type = "class")
multinom.conf.mat <- table(multinom.pred,mnist.dat$label) #confusion matrix
multinom.accuracy <- sum(diag(multinom.conf.mat))/sum(multinom.conf.mat)
print (multinom.conf.mat)
print(multinom.accuracy)
#-----------------multinomial model with both features----------------------#  
data.training <- mnist.dat[, cbind("label", "mean.distance", "density")]
multinom.model<-multinom(label~.,data = data.training, maxit = 1000)
summary(multinom.model)
multinom.pred <- predict(multinom.model, data.training[,-1], type = "class")
multinom.conf.mat <- table(multinom.pred,data.training$label) #confusion matrix
multinom.accuracy <- sum(diag(multinom.conf.mat))/sum(multinom.conf.mat)
print (multinom.conf.mat)
print(multinom.accuracy)



