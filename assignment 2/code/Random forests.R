library(tm)
library(randomForest)
random.forest.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
  #training set
  training.dtm <- cleaning.function(training.corpus.dec,training.corpus.true)
  
  #extraction of unigrams
  training.dtm.unigrams <- DocumentTermMatrix(training.dtm)
  training.dtm.unigrams <- removeSparseTerms(training.dtm.unigrams,0.95)
  training.dtm.unigrams <- as.matrix(training.dtm.unigrams)
  
  #extraction of bigrams
  BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  training.dtm.bigrams <- DocumentTermMatrix(training.dtm,control = list(tokenize = BigramTokenizer))
  training.dtm.bigrams <- removeSparseTerms(training.dtm.bigrams,0.95)
  training.dtm.bigrams <- as.matrix(training.dtm.bigrams)
  
  #training set with both unigrams and bigrams
  training.labels <- c(rep(0,320),rep(1,320))
  training.dtm <- cbind(training.dtm.unigrams, training.dtm.bigrams)
  
  ###################################################################################
  #test set
  test.dtm <- cleaning.function (testing.corpus.dec,testing.corpus.true)
  
  #unigrams
  test.dtm.unigrams<- DocumentTermMatrix(test.dtm,list(dictionary=dimnames(training.dtm.unigrams)[[2]]))
  test.dtm.unigrams <- as.matrix(test.dtm.unigrams)
  
  #bigrams
  test.dtm.bigrams<- DocumentTermMatrix(test.dtm,list(dictionary=dimnames(training.dtm.bigrams)[[2]]))
  test.dtm.bigrams <- as.matrix(test.dtm.bigrams)
  #test set with both unigrams and bigrams
  test.labels <- c(rep(0,80),rep(1,80))
  test.dtm <- cbind(test.dtm.unigrams, test.dtm.bigrams)
  #######################################################
  #with only unigrams
  training.dtm.unigrams <- as.data.frame(training.dtm.unigrams)
  test.dtm.unigrams <- as.data.frame(test.dtm.unigrams)
  OOB.matrix <- tuneRF(x =training.dtm.unigrams,
                       y = training.labels,
                       ntreeTry = 500, doBest = FALSE, plot = )
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
  predictions <- predict(classifier, newdata = test.dtm)
  table(predictions,test.labels)
  ############################################################
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
  predictions <- predict(classifier, newdata = test.dtm)
  table(predictions,test.labels)
  
}





