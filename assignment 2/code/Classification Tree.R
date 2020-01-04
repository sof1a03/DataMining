library(tm)
library(entropy)
library(SnowballC)
library(rpart)
library(rpart.plot)
classification.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
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
  # grow the tree with only unigrams
  
  reviews.rpart.unigrams <- rpart(label~.,
                           data=data.frame(training.dtm.unigrams,label = training.labels),
                           cp=0,method="class", minbucket = 1, minsplit = 2 )
  
  # tree with lowest cv error
  cp.unigrams <-  reviews.rpart.unigrams$cptable[which.min(reviews.rpart.unigrams$cptable[,"xerror"]),"CP"]
  print(cp.unigrams)
  plotcp(reviews.rpart.unigrams)
  print(reviews.rpart.unigrams$cptable)
  reviews.rpart.unigrams.pruned <- prune(reviews.rpart.unigrams,cp = reviews.rpart.unigrams$cptable[which.min(reviews.rpart.unigrams$cptable[,"xerror"]),"CP"] )
  post(reviews.rpart.unigrams.pruned)
  rpart.plot(reviews.rpart.unigrams.pruned, roundint = FALSE)
  # make predictions on the test set
  reviews.rpart.unigrams.pred <- predict(reviews.rpart.unigrams.pruned,
                                  newdata=data.frame(as.matrix(test.dtm.unigrams)),type="class")
  # show confusion matrix
  print(table(reviews.rpart.unigrams.pred,test.labels))
  #######################################################################################
  #unigrams and bigrams
  reviews.rpart <- rpart(label~.,
                    data=data.frame(training.dtm,label = training.labels),
                      cp=0,method="class", minbucket = 1, minsplit = 2)
  # tree with lowest cv error
  cp <-  reviews.rpart$cptable[which.min(reviews.rpart$cptable[,"xerror"]),"CP"]
  print(cp)
  plotcp(reviews.rpart)
  print(reviews.rpart$cptable)
  reviews.rpart.pruned <- prune(reviews.rpart,cp = reviews.rpart$cptable[which.min(reviews.rpart$cptable[,"xerror"]),"CP"] )
  rpart.plot(reviews.rpart.unigrams.pruned, roundint = FALSE)
  # make predictions on the test set
  reviews.rpart.pred <- predict(reviews.rpart.pruned,
                                         newdata=data.frame(as.matrix(test.dtm)),type="class")
  # show confusion matrix
  print(table(reviews.rpart.pred,test.labels))
}










