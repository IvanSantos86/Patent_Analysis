library(tm)

#loading in the data
setwd("C://Users//santo//OneDrive//Documents//R//text mining")
reviews <- read.csv("reviews.csv", stringsAsFactors=FALSE)

#combining all the reviews together 
review_text <- paste(reviews$text, collapse =" ")

#setting up source and corpus
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)

#cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#making a document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm
