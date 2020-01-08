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
  training.labels <- as.factor(training.labels)
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
  test.labels <- as.factor(test.labels)
  test.dtm <- cbind(test.dtm.unigrams, test.dtm.bigrams)
  #######################################################
  #with only unigrams
  training.dtm.unigrams <- as.data.frame(training.dtm.unigrams)
  test.dtm.unigrams <- as.data.frame(test.dtm.unigrams)
  OOB.matrix.unigrams <- tuneRF(x =training.dtm.unigrams,
                       y = training.labels,
                       ntreeTry = 500, doBest = FALSE, plot = TRUE)
  print(OOB.matrix.unigrams)
  optimal.mtry.unigrams <- OOB.matrix.unigrams[which.min(OOB.matrix.unigrams[,2]),1]
  print(optimal.mtry.unigrams)
  classifier.unigrams <- randomForest(x = training.dtm.unigrams,
                             y = training.labels, ntree = 1000,
                             mtry = optimal.mtry.unigrams, type = "classification", err.rate = TRUE)
  err.rate.unigrams <- cbind( c(1:1000),classifier.unigrams$err.rate[,1])
  optimal.ntree.unigrams <- err.rate.unigrams[which.min(err.rate.unigrams[,2]), 1]
  print(optimal.ntree.unigrams)
  colnames(err.rate.unigrams) <- c( "ntree","OOB error")
  plot(err.rate.unigrams)
  classifier.unigrams <- randomForest(x = training.dtm.unigrams,
                             y = training.labels, mtry = optimal.mtry.unigrams,
                             ntree = optimal.ntree.unigrams, type = "classification")
  # Predicting the Test set results
  random.forests.predictions.unigrams <- predict(classifier.unigrams, newdata = test.dtm.unigrams)
  print(table(random.forests.predictions.unigrams,test.labels))
  ############################################################
  #with both unigrams and bigrams
  training.dtm <- as.data.frame(training.dtm)
  test.dtm <- as.data.frame(test.dtm)
  OOB.matrix <- tuneRF(x =training.dtm,
                                y = training.labels,
                                ntreeTry = 500, doBest = FALSE, plot = TRUE)
  print(OOB.matrix)
  optimal.mtry <- OOB.matrix[which.min(OOB.matrix[,2]),1]
  print(optimal.mtry)
  classifier <- randomForest(x = training.dtm,
                                      y = training.labels, ntree = 1000,
                                      mtry = optimal.mtry, type = "classification", err.rate = TRUE)
  
  err.rate <- cbind(c(1:1000),classifier$err.rate[,1] )
  optimal.ntree <- err.rate[which.min(err.rate[,2]), 1]
  print(optimal.ntree)
  colnames(err.rate) <- c( "ntree","OOB error")
  plot(err.rate)
  classifier <- randomForest(x = training.dtm,
                                      y = training.labels, mtry = optimal.mtry,
                                      ntree = optimal.ntree, type = "classification")
  # Predicting the Test set results
  random.forests.predictions <- predict(classifier, newdata = test.dtm)
  print(table(random.forests.predictions,test.labels))
  
}





