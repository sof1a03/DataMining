#--------------------------libraries---------------------#
library("glmnet")

logistic.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
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
  
  #first model (only unigrams)
  reviews.glmnet.unigrams <- cv.glmnet(training.dtm.unigrams,training.labels,
                              family="binomial",type.measure="class")
  print(coef(reviews.glmnet.unigrams,s="lambda.1se"))
  reviews.logreg.pred.unigrams <- predict(reviews.glmnet.unigrams,
                                 newx=test.dtm.unigrams,s="lambda.1se",type="class") #this code is wrong: what is labda.1se
  print(table(reviews.logreg.pred.unigrams,test.labels))
  
  #second model ( with bigrams)
  reviews.glmnet <- cv.glmnet(training.dtm,training.labels,
                                       family="binomial",type.measure="class")
  print(coef(reviews.glmnet,s="lambda.1se"))
  reviews.logreg.pred <- predict(reviews.glmnet,
                                          newx=test.dtm,s="lambda.1se",type="class") #this code is wrong: what is labda.1se
  print(table(reviews.logreg.pred.unigrams,test.labels))
}










