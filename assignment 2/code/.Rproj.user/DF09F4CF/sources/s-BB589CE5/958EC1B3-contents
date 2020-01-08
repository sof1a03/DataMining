NNPredictions.is.correct <- sapply(c(1:length(NNPredictions)),function(index){
  if(NNPredictions[index] == x.test_labels[index]){
    return (1)
  }
  else (return (0))
})

lasso.pred.is.correct <- sapply(c(1:length(lasso.pred)),function(index){
  if(lasso.pred[index] == x.test_labels[index]){
    return (1)
  }
  else (return (0))
})

mnist.svm.pred.is.correct <- sapply(c(1:length(mnist.svm.pred)),function(index){
  if(mnist.svm.pred[index] == x.test_labels[index]){
    return (1)
  }
  else (return (0))
})
svm.vs.multinomial.conf.matrix <- table(mnist.svm.pred.is.correct, lasso.pred.is.correct)
mcnemar.test(svm.vs.multinomial.conf.matrix)
svm.vs.nn.conf.matrix <- table(mnist.svm.pred.is.correct, NNPredictions.is.correct)
mcnemar.test(svm.vs.nn.conf.matrix)
multinomial.vs.nn.conf.matrix <- table(lasso.pred.is.correct, NNPredictions.is.correct)
mcnemar.test(multinomial.vs.nn.conf.matrix)