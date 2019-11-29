#-----------------packages----------------------#
install.packages("OpenImageR")
library( OpenImageR)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("dplyr")
library(dplyr)
theme_set(theme_pubr())
install.packages("nnet")
library(nnet)
install.packages("raster")
library(raster)
install.packages()

#-----------------set up code( please change the repository name)----------------------#
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
min_pixel <- apply(mnist.dat[,-1], 2, min)
max_pixel <- apply(mnist.dat[,-1], 2, max)
avg_pixel <- apply(mnist.dat[,-1], 2, mean)
colsums <- colSums(mnist.dat[,-1])
summary <- cbind(min_pixel, max_pixel, avg_pixel, colsums)
summary <- as.data.frame(summary)
useless.pixels <- summary[summary$min_pixel == summary$max_pixel, ]
useless.pixels.name <- row.names(useless.pixels)
#-----------------analysis of the black points----------------------#
mnist.dat$num.black <- apply(mnist.dat[,-1], 1, function( digit){
  digit <- unlist(digit)
  digit <- as.vector(digit)
  return (length(digit[digit > 100]))
} )


#-----------------how much ink ("density")----------------------#
mnist.dat$density <- apply(mnist.dat[,-1], 1, sum)
density.mean <- aggregate(mnist.dat$density,by=list(mnist.dat$label),FUN=mean)
colnames(density.mean) <- c("label", "mean")
print(density.mean)
ggplot(data=density.mean, aes(x=label,y=mean))+geom_bar(stat="identity") #Depicting average intensity. 
density.sd <- aggregate(mnist.dat$density,by=list(mnist.dat$label),FUN=sd)
colnames(density.sd) <- c("label", "sd")
ggplot(data=density.sd, aes(x=label,y=sd))+geom_bar(stat="identity") #Depicting standard deviation.  

#-----------------simple multinomial model----------------------#
mnist.dat$density <- scale(mnist.dat$density)
data.training <- mnist.dat
data.test <- mnist.dat
multinom.model<-multinom(label~density,data = data.training, maxit = 1000)
multinom.pred <- predict(multinom.model, data.training[, -1], type = "class")
summary(multinom.model)
multinom.conf.mat <- table(multinom.pred,mnist.dat$label) #confusion matrix
multinom.accuracy <- sum(diag(conf.mat))/sum(conf.mat)
print (multinom.conf.mat)
print(multinom.accuracy)

#-----------------new feature multinomial model ( distance of the black points)----------------------#
mmnist.dat$mean.distance <- apply(mnist.dat[,-1],1, function(current.example){
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
  current.example.points <- current.example.with.indexes[current.example.with.indexes[,1] > 100, ]
  mean.row <- mean (current.example.points[,2])
  mean.column <- mean (current.example.points[,3])
  point.distances <- lapply(c(1:nrow(current.example.points)), function(point){
    #to check
    pointDistance(current.example.points[point,-1], c(mean.row, mean.column), lonlat = FALSE)
  })
  
  point.distances <- unlist (point.distances, recursive = FALSE)
  return (mean(point.distances))
})
  

  


mnist.dat <- 
row_counter = 0
column_counter = 0
for ( counter in c(0:783)){
  
}


#-----------2nd part------------------












summary(myModel)