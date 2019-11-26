library(tm)
library(randomForest)
random.forest.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
  #training set
  training.dtm <- cleaning.function(training.corpus.dec,training.corpus.true)
  training.dtm <- DocumentTermMatrix(training.dtm)
  training.dtm <- removeSparseTerms(training.dtm,0.95)
  training.dtm = as.matrix(training.dtm)
  training.labels <- c(rep(0,320),rep(1,320))
  #test set
  test.dtm <- DocumentTermMatrix(cleaning.function (testing.corpus.dec,testing.corpus.true),list(dictionary=dimnames(training.dtm)[[2]]))
  test.dtm <- as.matrix(test.dtm)
  test.labels <- c(rep(0,80),rep(1,80))
  
  
  training.dtm <- as.data.frame(training.dtm)
  test.dtm <- as.data.frame(test.dtm)
  training.dtm$label <- training.labels
  training.dtm$label <- factor(training.dtm$label, levels = c(0, 1))
  OOB.matrix <- tuneRF(x = training.dtm[-318],
                       y = training.dtm$label,
                       ntreeTry = 500, doBest = FALSE)
  optimal.mtry <- OOB.matrix[which.min(OOB.matrix[,2]),1]
  classifier <- randomForest(x = training.dtm[-318],
    y = training.dtm$label, ntree = 1000,
    mtry = optimal.mtry, type = "classification", err.rate = TRUE)
  
  error_rates <- classifier$err.rate[,1]
  plot(classifier$err.rate[,1])
  error_rates <- cbind(error_rates, c(1:1000))
  optimal_ntree <- error_rates[which.min(error_rates[,1]), 2]
  classifier <- randomForest(x = training.dtm[-318],
                           y = training.dtm$label, mtry = optimal.mtry,
                            ntree = optimal_ntree, type = "classification", err.rate = TRUE)
     
  # Predicting the Test set results
  test.dtm <- as.data.frame(test.dtm)
  predictions <- predict(classifier, newdata = test.dtm)
  table(predictions,test.labels)
  
}




cleaning.function <- function(corpus.dec, corpus.true){
  reviews.dec <-VCorpus(DirSource(corpus.dec,encoding="UTF-8"))
  reviews.true<-VCorpus(DirSource(corpus.true,encoding="UTF-8"))
  review.all<-c(reviews.dec,reviews.true)
  
  #clean the data
  review.all <- tm_map(review.all, content_transformer(tolower))
  review.all <- tm_map(review.all, removeNumbers)
  review.all <- tm_map(review.all, removePunctuation)
  review.all <- tm_map(review.all, stripWhitespace)
  review.all <- tm_map(review.all,removeWords,stopwords("english"))
  return (review.all)
  
}

