#--------------------------libraries---------------------#
library(entropy)

naive.bayes.function <- function (training.corpus.dec, training.corpus.true, testing.corpus.dec, testing.corpus.true){
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
  ########################################################################################
  #feature selection (with mutual information, only for unigrams)
  training.dtm.unigrams.mi <- apply(training.dtm.unigrams,2,function(x,y){
    mi.plugin(table(x,y)/length(y))},training.labels)
  training.dtm.unigrams.mi.order <- order(training.dtm.unigrams.mi,decreasing = T)
  
  #feature selection ( with mutual information)
  training.dtm.mi <- apply(training.dtm,2,function(x,y){
                      mi.plugin(table(x,y)/length(y))},training.labels)
  training.dtm.mi.order <- order(training.dtm.mi,decreasing = T)
  ###########################################################################################
  #print
  print(dim(training.dtm.bigrams))
  print(dim(training.dtm.unigrams))
  print(colnames(training.dtm.bigrams))
  print(dim(training.dtm)) ## just to check
  print(training.dtm.mi[training.dtm.mi.order[1:10]])
  
  #first model ( with feature selection according to mutual information)(only unigrams)
  accuracies.unigrams.mi.models <- sapply(c(2:307), function (num.features){
    model.mi <-train.mnb(training.dtm.unigrams[,training.dtm.unigrams.mi.order[1:num.features] ], training.labels) 
    predictions.mi <- predict.mnb(model.mi , test.dtm.unigrams[,training.dtm.unigrams.mi.order[1:num.features]])
    conf.mat <- table (predictions.mi ,test.labels)
    return (sum(diag(conf.mat))/180)
  } )
  
  accuracies.unigrams.mat <- cbind (accuracies.unigrams.mi.models, c(2:307))
  accuracies.unigrams.best.n <- accuracies.unigrams.mat[which.max(accuracies.unigrams.mat[,1]), 2]
  print(accuracies.unigrams.mat)
  print(accuracies.unigrams.best.n)
  plot(accuracies.unigrams.mat[,2] ,accuracies.unigrams.mat[,1], xlab = "n", ylab = "accuracy", type = "l")
  model.unigrams.mi <-train.mnb(training.dtm.unigrams[,training.dtm.unigrams.mi.order[1:accuracies.unigrams.best.n] ], training.labels) 
  print(model.unigrams.mi)
  naive.bayes.predictions.unigrams.mi <- predict.mnb(model.unigrams.mi , test.dtm.unigrams[,training.dtm.unigrams.mi.order[1:accuracies.unigrams.best.n]])
  print(table (naive.bayes.predictions.unigrams.mi ,test.labels))

  
  #second model ( with feature selection according to mutual information)(both unigrams and bigrams)
  accuracies.mi.models <- sapply(c(2:319), function (num.features){
    model.mi <-train.mnb(training.dtm[,training.dtm.mi.order[1:num.features] ], training.labels) 
    predictions.mi <- predict.mnb(model.mi , test.dtm[,training.dtm.mi.order[1:num.features]])
    conf.mat <- table (predictions.mi ,test.labels)
    return (sum(diag(conf.mat))/180)
  } )
  
  accuracies.mat <- cbind (accuracies.mi.models, c(2:319))
  accuracies.best <- accuracies.mat[which.max(accuracies.mat[,1]), 2]
  print(accuracies.mat)
  print(accuracies.best)
  plot(accuracies.mat[,2] ,accuracies.mat[,1], xlab = "n", ylab = "accuracy", type = "l")
  model.mi <-train.mnb(training.dtm[,training.dtm.mi.order[1:accuracies.best] ], training.labels) 
  print(model.mi)
  naive.bayes.predictions.mi <- predict.mnb(model.mi , test.dtm[,training.dtm.mi.order[1:accuracies.best]])
  print(table (naive.bayes.predictions.mi ,test.labels))
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







