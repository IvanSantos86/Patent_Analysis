# Patent analysis using text mining
# Author:
# Ivan Santos
# License: MIT

library(tidyverse)
library(tm)

# Load Data
setwd("C://Users//santo//OneDrive//Documents//R//text mining")
reviews <- read.csv("reviews.csv", stringsAsFactors=FALSE)

# Combine rows
review_text <- paste(reviews$text, collapse =" ")

# Set up source and corpus
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)

# Clean corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Make a document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm

# Goal 1 - Compare term frequency over time ('98-'17) 
# Goal 2 - Cluster terms 
# Goal 3 - Cluster terms and compare with other features
# Goal 4 - How many times the term 'recombinant' shows up for each patent (row)
# Goal 5 - How many patents (rows) have at least the term 'recombinant'




