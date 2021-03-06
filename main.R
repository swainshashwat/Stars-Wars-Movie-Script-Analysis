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
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
}

# Number of dialogues in episode 4
print("No. of dialogues in ep4")
length(ep4$character)

# Number of dialogues in episode 5
print("No. of dialogues in ep5")
length(ep5$character)

# Number of dialogues in episode 6
print("No. of dialogues in ep6")
length(ep6$character)


# Top 20 chars with maximum dialogues
top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing = TRUE))[1:20,]

# Visualization
ggplot(data=top.ep4.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="blue", colour="black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")

# Wordcloud for Episode IV
wordcloud2(frequentTerms(ep4$dialogue), size=0.5,
           figPath = "dataset/wordcloud_masks/vader.png")

