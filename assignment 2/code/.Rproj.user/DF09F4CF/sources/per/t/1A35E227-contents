#function used to clean the data
library(tm)
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