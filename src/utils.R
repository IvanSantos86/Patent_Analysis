createTermFrequencyDf <- function(data, variable, uniqueKeyword = TRUE,
                                  rfidf = FALSE) {
  # Create data frame with term frequency data
  # Arguments:
  #   data: dataframe which must have an year column
  #   variable: variable of interest
  #   distinct: do you need distinct keywords for each document
  #             or the summed keywords? Default = TRUE
  
  vector.of.years <- unique(data$year)
  list.of.dataframes <- list()
  
  for (i in 1:length(vector.of.years)){
    temp.data <- filter(data, year == vector.of.years[i])
    review_source <- VectorSource(eval(parse(text = paste0("temp.data$", variable))))
    corpus <- VCorpus(review_source)
    corpus <- cleanCorpus(corpus)
    
    if (rfidf == TRUE) {
      dtm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
    } else {
      dtm <- TermDocumentMatrix(corpus)  
    }
    
    dtm.matrix <- as.matrix(dtm)
    
    if (uniqueKeyword == TRUE) {
      dtm.matrix <- apply(dtm.matrix, 2, function(x) ifelse(x > 0, 1, 0))
    }
    
    term.freq <- rowSums(dtm.matrix)
    temp.data <- data.frame(year = vector.of.years[i],
                            word = names(term.freq),
                            frequency = term.freq)
    
    temp.data <- arrange(temp.data, desc(frequency))
    
    list.of.dataframes[[i]] <- temp.data
    
  }
  data <- do.call(rbind, list.of.dataframes)
}

cleanCorpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

cleanText <- function(vector) {
  require(tm)
  vector <- tolower(vector)
  vector <- tm::removePunctuation(vector)
  vector <- tm::stripWhitespace(vector)
  vector <- tm::removeNumbers(vector)
  return(vector)
}

frequentTermCluster <- function(data, n_cluster) {
  
  df_filtered <- subset(data, data$cluster == n_cluster)
  
  corpus_filtered <- data.frame(doc_id = seq(1:nrow(df_filtered)),
                                text = df_filtered$text.title.abstract)
  
  corpus_filtered <- VCorpus(DataframeSource(corpus_filtered))
  corpus_filtered <- cleanCorpus(corpus_filtered)
  tda_filtered <- TermDocumentMatrix(corpus_filtered, 
                                     control = list(weighting = weightTfIdf))
  
  tda_filtered <- as.matrix(tda_filtered)
  words_list <- rowSums(tda_filtered)
  terms <- sort(words_list, decreasing = TRUE)
  
  return(terms[1:10])
  
}