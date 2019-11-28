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

#-----------------set up code----------------------#
mnist.dat <- read.csv("C:/Users/ponti/Documents/GitHub/DataMining/pattern recognition/mnist.csv")
imageShow(matrix(as.numeric(mnist.dat[380,-1]),nrow=28,ncol=28,byrow=T))
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

#-----------------how much ink----------------------#
mnist.dat$ink <- apply(mnist.dat[,-1], 1, sum)
ink.mean <- aggregate(mnist.dat$ink,by=list(mnist.dat$label),FUN=mean)
colnames(ink.mean) <- c("label", "mean")
ggplot(data=ink.mean, aes(x=label,y=mean))+geom_bar(stat="identity") #Depicting average intensity. 
ink.sd <- aggregate(mnist.dat$ink,by=list(mnist.dat$label),FUN=sd)
colnames(ink.sd) <- c("label", "sd")
ggplot(data=ink.sd, aes(x=label,y=sd))+geom_bar(stat="identity") #Depicting standard deviation.  






#new.dat<-mnist.dat[, colSums(mnist.dat != 0) > 0] #Removing columns that add up to zero


#-------------Class Distribution-------------#

frequency.label<-count(mnist.dat,'label') #Checking the frequency of each class

maxLabel<-Frequency_label[which.max(Frequency_label$freq),] #The maximum class

as.integer(maxLabel$freq)/42000 #Probabilty of any item being in maxClass


#-----------2nd part------------------
mnist.dat$density<-NULL
mnist.dat$density<-(255-mnist.dat$intensity)/255 * 100 #Density Function

tapply(mnist.dat$density,new.dat[,1],mean)
tapply(mnist.dat$density,new.dat[,1],sd)

 
library(nnet)
#mnist.dat[,2:786] <- (mnist.dat[,2:786] - min(mnist.dat[,2:786])) / (max(mnist.dat[,2:786]) - min(mnist.dat[,2:786])) #Feature scaling
myModel<-multinom(label~density,data = mnist.dat)

optdigits.multinom.pred <- predict(myModel,
                                   mnist.dat[,c(-1)],type="class")

summary(myModel)
table(mnist.dat$label,optdigits.multinom.pred)

confmat <- table(mnist.dat[,1],
                 optdigits.multinom.pred)

sum(diag(confmat))/sum(confmat)


summary(myModel)