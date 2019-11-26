library(entropy)
naive.bayes.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
  #training set
  training.dtm <- cleaning.function(training.corpus.dec,training.corpus.true)

  #extraction of unigrams
  training.dtm.unigrams <- DocumentTermMatrix(training.dtm)
  training.dtm.unigrams <- removeSparseTerms(training.dtm.unigrams,0.95)
  training.dtm.unigrams = as.matrix(training.dtm.unigrams)
  training.labels <- c(rep(0,320),rep(1,320))
  
  #extraction of bigrams
  BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  training.dtm.bigrams <- DocumentTermMatrix(training.dtm,control = list(tokenize = BigramTokenizer))
  training.dtm.bigrams <- removeSparseTerms(training.dtm.bigrams,0.95)
  training.dtm.bigrams = as.matrix(training.dtm.bigrams)
  print(dim(training.dtm.bigrams))
  print(dim(training.dtm.unigrams))
  print(colnames(training.dtm.bigrams))
  
  #training set
  training.labels <- c(rep(0,320),rep(1,320))
  training.dtm <- cbind(training.dtm.unigrams, training.dtm.bigrams)
  print(dim(training.dtm))
  #test set
  test.dtm <- cleaning.function (testing.corpus.dec,testing.corpus.true)
  test.dtm <- DocumentTermMatrix(test.dtm,list(dictionary=dimnames(training.dtm)[[2]]))
  test.dtm = as.matrix(test.dtm)
  test.labels <- c(rep(0,80),rep(1,80))
  
  #feature selection
  training.dtm.mi <- apply(training.dtm,2,function(x,y){
                      mi.plugin(table(x,y)/length(y))},training.labels)
  training.dtm.mi.order <- order(training.dtm.mi,decreasing = T)

  #predicting
  model <- train.mnb(training.dtm[, ], training.labels)
  predictions <- predict.mnb(model, test.dtm[, ])
  table (predictions,test.labels)
  
}

#Training function for Naive Bayes
#labels = classes
train.mnb <- function (dtm,labels) {
  call <- match.call()
  V <- ncol(dtm) #vocabulary
  N <- nrow(dtm) #number of documents
  prior <- table(labels)/N
  labelnames <- names(prior)
  nclass <- length(prior)
  cond.probs <- matrix(nrow=V,ncol=nclass)
  dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
  dimnames(cond.probs)[[2]] <- labelnames
  index <- list(length=nclass)
  for(j in 1:nclass){
    index[[j]] <- c(1:N)[labels == labelnames[j]]
  }
  for(i in 1:V){
    for(j in 1:nclass){
      cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V)
    #Laplace smoothing
    }
  }
  x <- list(call=call,prior=prior,cond.probs=cond.probs)
  return (x)
}

predict.mnb <- function (model,dtm) {
  classlabels <- dimnames(model$cond.probs)[[2]]
  logprobs <- dtm %*% log(as.matrix(model$cond.probs))
  N <- nrow(dtm) #number of documents to classify
  nclass <- ncol(model$cond.probs) #number of classes
  logprobs <- logprobs+matrix(nrow=N,ncol=nclass,log(model$prior),byrow=T)
  x <- classlabels[max.col(logprobs)]  
  return (x)
}







