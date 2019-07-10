# Patent analysis using text mining
# Author:
# Ivan Santos
# License: MIT

library(tidyverse)
library(tm)

# Load Data
reviews <- read.csv2("~/Patent_Analysis/data/bovino_sel.csv", 
                    stringsAsFactors=FALSE)


# Data prep ---------------------------------------------------------------

# Extract year from string using Regular Expressions
reviews$year <- str_extract(reviews$Priority.numbers, 
                            "([\\d+]{4})")

# Extract country from string using Regular Expressions
reviews$country <- str_extract(reviews$Priority.numbers, 
                            "([A-z]{2})")


# Extract assignees in a given column and separate them into multiple
# columns

reviews <- 
 separate(reviews, 
          Latest.standardized.assignees...inventors.removed,
          sep = "\n", 
          into = paste0("assignee.", 
                        seq(1, max(reviews$Assignees...Count))))

table_1 <- gather(reviews, "assignee.1", "assignee.2", "assignee.3", 
                  key = "assignee",
                  value = "assignee.name") %>%
  filter(assignee.name != "") %>%
  select(year, assignee.name)



a <- strsplit(reviews$IPC...International.classification, "\n")
a <- max(sapply(a, length))

reviews <- 
  separate(reviews, 
           IPC...International.classification,
           sep = "\n", 
           into = paste0("ipc.", 
                         seq(1, a)))

table_2 <- gather(reviews, str_subset(names(reviews), "ipc."), 
                  key = "ipc",
                  value = "ipc.code") %>%
  filter(ipc.code != "") %>%
  select(year, ipc.code)

# Extract simplified ICP code using Regular Expressions
table_2$ipc.s <- str_extract(table_2$ipc.code, "[^/]+")


# TM --------------------------------------------------------------



# Combine rows
reviews$text.title.abstract <- 
  paste(reviews$Title, reviews$Abstract,
        sep =" ")

#title.abstract <- paste(reviews$text.title.abstract,
#                        collapse = "  ")

# Set up source and corpus
review_source <- VectorSource(reviews$text.title.abstract)
corpus <- Corpus(review_source)

# Clean corpus
corpus.temp <- tm_map(corpus, content_transformer(tolower))
corpus.temp <- tm_map(corpus.temp, removePunctuation)
corpus.temp <- tm_map(corpus.temp, stripWhitespace)
corpus.temp <- tm_map(corpus.temp, removeWords, stopwords("english"))
corpus.temp <- tm_map(corpus.temp, stemDocument, language = "english")
teste <- tm_map(corpus.temp, stemCompletion, type = "prevalent", dictionary = corpus)


# Make a document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm

# Goal 1 - Compare term frequency over time ('98-'17) 
# Goal 2 - Cluster terms 
# Goal 3 - Cluster terms and compare with other features
# Goal 4 - How many times the term 'recombinant' shows up for each patent (row)
# Goal 5 - How many patents (rows) have at least the term 'recombinant'


# Create frequency table with ics simplified codes
View(table_2 %>% group_by(ipc.s) %>% summarise(f = n()) %>% arrange(desc(f)))

