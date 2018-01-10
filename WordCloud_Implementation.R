#########################################################
# This code is used to generate a wordcloud. The input
# to be provided is a txt file. Based on the frequency 
# of occurance of words , a word cloud will be
# generated.This provides a concise visual representation 
# of text data which can be used for analysis. 

# Author : Suhit Datta

# Date : January 2018
#########################################################

rm(list = ls())

install.packages("tm") 
install.packages("SnowballC") 
install.packages("wordcloud")
install.packages("RColorBrewer")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#Choose a txt file as an input for creating the wordcloud. This is prompted
stext <- readLines(file.choose())

#loading the data as a corpus
doc <- Corpus(VectorSource((stext)))

inspect(doc)

#transforming the text and replacing special characters if any
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
doc <- tm_map(doc, toSpace, "/")
doc <- tm_map(doc, toSpace, "@")
doc <- tm_map(doc, toSpace, "\\|")
doc <- tm_map(doc, toSpace, "-")

#cleaning the text - such as removing stop words 

# and stemming the text
# Convert the text to lower case
doc <- tm_map(doc, content_transformer(tolower))
# Remove numbers
doc <- tm_map(doc, removeNumbers)
# Remove english common stopwords
doc <- tm_map(doc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector if any
doc <- tm_map(doc, removeWords, c("nada", "zilch")) 
# Remove punctuations
doc <- tm_map(doc, removePunctuation)
# Eliminate extra white spaces
doc <- tm_map(doc, stripWhitespace)
# Text stemming
doc<- tm_map(doc, stemDocument)

# building a term document matrix : frequency of words 
tdm <- TermDocumentMatrix(doc)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# generating the word cloud
set.seed(123)
wordcloud(words = d$word, freq = d$freq , min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.15,colors = brewer.pal(8,"Dark2"))

findFreqTerms(tdm, lowfreq = 4)
