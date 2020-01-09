reviews.logreg.pred.is.correct <- sapply(c(1:length(reviews.logreg.pred)),function(index){
  if(reviews.logreg.pred[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

reviews.logreg.pred.unigrams.is.correct <- sapply(c(1:length(reviews.logreg.pred.unigrams)),function(index){
  if(reviews.logreg.pred.unigrams[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

naive.bayes.predictions.mi.is.correct <- sapply(c(1:length(naive.bayes.predictions.mi)),function(index){
  if(naive.bayes.predictions.mi[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

naive.bayes.predictions.unigrams.mi.is.correct <- sapply(c(1:length(naive.bayes.predictions.unigrams.mi)),function(index){
  if(naive.bayes.predictions.unigrams.mi[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

random.forests.predictions.is.correct <- sapply(c(1:length(random.forests.predictions)),function(index){
  if(random.forests.predictions[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

random.forests.predictions.unigrams.is.correct <- sapply(c(1:length(random.forests.predictions.unigrams)),function(index){
  if(random.forests.predictions.unigrams[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

reviews.rpart.pred.is.correct <- sapply(c(1:length(reviews.rpart.pred)),function(index){
  if(reviews.rpart.pred[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})

reviews.rpart.unigrams.pred.is.correct <- sapply(c(1:length(reviews.rpart.unigrams.pred)),function(index){
  if(reviews.rpart.unigrams.pred[index] == test.labels[index]){
    return (1)
  }
  else (return (0))
})


naive.vs.logreg.conf.matrix <- table(naive.bayes.predictions.mi.is.correct, reviews.logreg.pred.is.correct)
mcnemar.test(naive.vs.logreg.conf.matrix)

naive.vs.logreg.conf.matrix.unigrams <- table(naive.bayes.predictions.unigrams.mi.is.correct, reviews.logreg.pred.unigrams.is.correct)
mcnemar.test(naive.vs.logreg.conf.matrix.unigrams)

random.forests.conf.matrix <- table(random.forests.predictions.unigrams.is.correct, random.forests.predictions.is.correct)
mcnemar.test(random.forests.conf.matrix )

naive.vs.random.conf.matrix <- table(naive.bayes.predictions.mi.is.correct, random.forests.predictions.is.correct)
mcnemar.test(naive.vs.random.conf.matrix)

logreg.vs.random.conf.matrix <- table(reviews.logreg.pred.is.correct, random.forests.predictions.is.correct)
mcnemar.test(logreg.vs.random.conf.matrix)

logreg.conf.matrix <- table(reviews.logreg.pred.is.correct, reviews.logreg.pred.unigrams.is.correct)
mcnemar.test(logreg.conf.matrix )