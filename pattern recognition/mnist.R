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

#-----------------how much ink ("density")----------------------#
mnist.dat$density <- apply(mnist.dat[,-1], 1, sum)
vector = for (label in c(0:9)){
  mnist.dat.current.class <- mnist.dat[mnist.dat$label == label, ]
  return (mean(mnist.dat.current.class$density))
}
print(vector)
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

#-----------------new feature multinomial model----------------------#
for (label in c(0:9)){
  
}




#-----------2nd part------------------












summary(myModel)