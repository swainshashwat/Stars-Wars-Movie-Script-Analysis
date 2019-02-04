# load libraries
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(radarchart)
library(RWeka)

# Read the data
ep4 <- read.table("dataset/SW_EpisodeIV.txt")
ep5 <- read.table("dataset/SW_EpisodeV.txt")
ep6 <- read.table("dataset/SW_EpisodeVI.txt")

# text transformation
cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("english"),
                   c("thats", "weve", "hes", "theres", "ive", "im", "will",
                     "can", "cant", "dont", "youve", "youre", "youll", "theyre",
                     "whats", "didnt"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  
  return(corpus.tmp)
}

# most frequent terms
frequentTerms <- function(text){
  s.cor <- Corpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm
}





