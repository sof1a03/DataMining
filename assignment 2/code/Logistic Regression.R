library("glmnet")
logistic.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
  #training set
  training.dtm <- cleaning.function(training.corpus.dec,training.corpus.true)
  training.dtm <- DocumentTermMatrix(training.dtm)
  training.dtm <- removeSparseTerms(training.dtm,0.95)
  training.dtm = as.matrix(training.dtm)
  training.labels <- c(rep(0,320),rep(1,320))
  #test set
  test.dtm <- DocumentTermMatrix(cleaning.function (testing.corpus.dec,testing.corpus.true),list(dictionary=dimnames(training.dtm)[[2]]))
  test.dtm = as.matrix(test.dtm)
  test.labels <- c(rep(0,80),rep(1,80))
  reviews.glmnet <- cv.glmnet(training.dtm,training.labels,
                              family="binomial",type.measure="class")
  print(coef(reviews.glmnet,s="lambda.1se"))
  reviews.logreg.pred <- predict(reviews.glmnet,
                                 newx=test.dtm,s="lambda.1se",type="class") #this code is wrong: what is labda.1se
  table(reviews.logreg.pred,test.labels)
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





