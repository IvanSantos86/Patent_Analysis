# Patent analysis 
# Ivan Santos, Ph.D.

library(tidyverse)
library(tm)
library(lubridate)
library(ggthemes)
library(jtools)
library(stringr)
#library(qdap)

#funcoes
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


# Load Data
#Windows
data <- read.csv2("~/GitHub/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE) 
#Linux
data <- read.csv2("~/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE)

#remover documentos duplicados
#se nao for selecionar hospedeiro
data <- distinct(data, questel_id,.keep_all= TRUE)

# Selecionar banco de dados (ex: hospedeiro)
data <- filter(data, ï..host == "avian")


# 3. Associacao de palavras ----------------------------------------------------


##3.1: Associacoes -------------------
#com os termos "attenuated", "inactivated", "recombinant", "strain"
word_ass <- data.frame(doc_id = seq(1:nrow(data)),
                       text = data$text.title.abstract)

corpus.wa <- VCorpus(DataframeSource(word_ass))

corpus.wa <- cleanCorpus(corpus.wa)

tdm_wa <- TermDocumentMatrix(corpus.wa,control = list(weighting = weightTfIdf))

# TODO -  Explorar termos mais frequentes e de maior interesse da area para 
# encontrar eventuais padroes e tecnologias

associations.recom <- findAssocs(tdm.wa, 'recombinant', 0.2)
associations.recom <- as.data.frame(associations.recom)
associations.recom$terms <- row.names(associations.recom)
associations.recom$terms <- factor(associations.recom$terms, 
                                   levels = associations.recom$terms)

ggplot(associations.recom, aes( y = terms)) +
  geom_point(aes(x = recombinant), data = associations.recom,
             size = 5) +
  theme_gdocs() + 
  geom_text(aes(x = recombinant,
                label = recombinant),
            colour = "darkred", hjust = -.25, size = 8) +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank())

associations.inac <- findAssocs(tdm.wa, 'inactivated', 0.25)
associations.inac <- as.data.frame(associations.inac)
associations.inac$terms <- row.names(associations.inac)
associations.inac$terms <- factor(associations.inac$terms, 
                                  levels = associations.inac$terms)


ggplot(associations.inac, aes(y = terms)) +
  geom_point(aes(x = inactivated), data = associations.inac,
             size = 5) +
  theme_gdocs() + 
  geom_text(aes(x = inactivated,
                label = inactivated),
            colour = "darkred", hjust = -.25, size = 8)+
  theme(text = element_text(size = 20),
        axis.title.y = element_blank())

### 3.2 Word Network ------------
recombinant <- data[grep("recombinant", data$text.title.abstract,ignore.case = TRUE),]
word_network_plot(recombinant$text.title.abstract[1:10], stopwords = "english")

word_associate(word_ass$text,match.string = c('recombinant'),
               stopwords = Top200Words,network.plot = T,
               cloud.colors = c("gray85","darkred"))


# 3.3 Affinity Propagation Clustering -----------------------------------------

library(apcluster)
library(cluster)

# Combinar colunas titulo e resumo
data$text.title.abstract <- paste(data$title, 
                                  data$abstract,sep = " ")


# 1. Criar matrix patente vs. termo
matrix <- data.frame(doc_id = seq(1:nrow(data)),
                       text = data$text.title.abstract)

corpus.matrix <- VCorpus(DataframeSource(matrix))
corpus.matrix <- cleanCorpus(corpus.matrix)

tdm_matrix <- DocumentTermMatrix(corpus.matrix, control = list(weighting = weightTfIdf))
tdm_matrix <- as.matrix(tdm_matrix)

# Muldimensional scaling for ploting data
mds <- 
  tdm_matrix %>%
  dist() %>% #dissimilarity matrix 
  cmdscale() %>% # multidimensional scale - reduction for 2 dimensions
  as_tibble(.name_repair = "unique")
colnames(mds) <- c("Dim.1", "Dim.2")

# Preparar dados para algoritmo de propagacao por afinidade.
# datacluster <- dist(tdm_wa) 
datacluster <- as.matrix(mds)
negMat      <- negDistMat(datacluster, r = 2)
apmodel     <- apcluster(negMat)

show(apmodel)

plot(apmodel, datacluster)

heatmap(apmodel, negMat)

# Passar clusteres para banco de dados original
data$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))

# Filtrar cluster
frequentTermCluster(data, n_cluster = 3)  


# plotar experimentalmente os clusteres ----
# Criar variaveis para plotar abaixo
mds$country.r <- data$country.rec
mds$year <- data$year
mds$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))

# Plot clusters
library(ggpubr)
ggscatter(mds, x = "Dim.1", y = "Dim.2",
          color = "cluster",
          repel = TRUE) +
  facet_wrap(~ year) 