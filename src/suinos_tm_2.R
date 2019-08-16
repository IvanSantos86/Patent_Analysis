# Patent analysis using text mining
# Ivan Santos
## Vacinas suinos

library(tidyverse)
library(tm)
library(lubridate)

# Load Data
suinos <- read.csv2("~/GitHub/Patent_Analysis/data/suino_sel.csv", 
                    stringsAsFactors=FALSE)

### 1. Evolução Temporal dos depósitos
## Data preparation ---------------------------------------------------------------

# Ano de prioridade
# Extração do ano de prioridade
suinos$p.year <- str_extract(suinos$Priority.numbers,"([\\d+]{4})")


# Pais de prioridade
#Extração do país de prioridade
suinos$p.country <- str_extract(suinos$Priority.numbers,"([A-z]{2})")

table_suinos_p.country <- data.frame(suinos$p.year, suinos$p.country)

#1.1 grafico: Patentes x Pais de prioridade x Ano
table_suinos_p.country  %>%  count(suinos.p.year, suinos.p.country)%>% 
  ggplot(aes(suinos.p.year,n,fill=suinos.p.country)) +
  geom_bar(stat='identity')

##TODO: filtrar top 5 de cada ano

#IPCs
#Separar IPCs

a <- strsplit(suinos$IPC...International.classification, "\n")
a <- max(sapply(a, length))

suinos <- separate(suinos, 
           col=IPC...International.classification,
           sep = "\n", 
           into = paste0("ipc.", 
                         seq(1, a)))

table_suinos_ipc <- gather(suinos, str_subset(names(suinos), "ipc."), 
                  key = "ipc",
                  value = "ipc.code") %>%
  filter(ipc.code != "") %>%
  select(p.year, ipc.code)

# Extrair IPC nivel classe
table_suinos_ipc$ipc.s <- str_extract(table_suinos_ipc$ipc.code, "[^/]+")

#1.2 grafico: Patentes x IPC x Ano
table_suinos_ipc  %>%  count(p.year, ipc.s)%>% 
  ggplot(aes(p.year,n,fill=ipc.s)) +
  geom_bar(stat='identity')

##TODO: filtrar top 5 de cada ano

#Depositantes
#depositantes pos-openrefine
table_suinos_assignee <- read.csv2("~/GitHub/Patent_Analysis/data/suinos_sel_year.assignee.or.csv")

#1.3 grafico: Patentes x Depositante x Ano
table_suinos_assignee  %>%  count(priority.date.1, assignee)%>% 
  ggplot(aes(priority.date.1,n,fill=assignee)) +
  geom_bar(stat='identity')

##TODO: filtrar top 5 de cada ano


###Statistical Text Mining

##Frequencia de termos por ano baseado no titulo e resumo
# Combinar colunas titulo e resumos
suinos$text.title.abstract <- paste(suinos$Title, suinos$Abstract,sep =" ")

data <- suinos

# frequencia de termos por ano
#criar corpus
createTermFrequencyDf <- function (data, uniqueKeyword = TRUE){
  
  # Create data frame with term frequency data
  # Arguments:
  #   data: dataframe which must have an year column
  #   distinct: do you need distinct keywords for each document
  #             or the summed keywords? Default = TRUE
  
  vector.of.years <- unique(data$p.year)
  list.of.dataframes <- list()
  
  for (i in 1:length(vector.of.years)){
    temp.data <- filter(data, p.year == vector.of.years[i])
    review_source <- VectorSource(temp.data$text.title.abstract)
    corpus <- VCorpus(review_source)
    
    # Clean corpus
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    #corpus <- tm_map(corpus, stemDocument, language = "english")
    #corpus <- stemCompletion(corpus.temp, corpus, type = "prevalent") # Try to stem words
    
    dtm <- TermDocumentMatrix(corpus)
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

data.unique <- createTermFrequencyDf(suinos, uniqueKeyword = TRUE)
data1       <- createTermFrequencyDf(suinos, uniqueKeyword = FALSE)


#Tabela de frequencia de termos somando todos os anos
table_tm.tiabs <- 
  data1 %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))

terms.of.interest <- c("attenuated", "inactivated", "recombinant", "strain")

#2.1 Grafico: Frequencia de termos (1 por patente) x Ano
chart_1<- data.unique %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))
#linha
ggplot(data = chart_1, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#coluna
ggplot(data = chart_1, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


#2.2 Grafico: frequencia de termos total x Ano
chart_2 <- data1 %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))
#linha
ggplot(data = chart_2, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#coluna
ggplot(data = chart_2, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())



##Frequencia de termos por ano baseado nas reivindicações
data <- suinos

#criar corpus
createTermFrequencyDf.cl <- function (data, uniqueKeyword = TRUE){
  
  # Create data frame with term frequency data
  # Arguments:
  #   data: dataframe which must have an year column
  #   distinct: do you need distinct keywords for each document
  #             or the summed keywords? Default = TRUE
  
  vector.of.years.cl <- unique(data$p.year)
  list.of.dataframes.cl <- list()
  
  for (i in 1:length(vector.of.years.cl)){
    temp.data.cl <- filter(data, p.year == vector.of.years.cl[i])
    review_source_cl <- VectorSource(temp.data.cl$Claims)
    corpus.cl <- VCorpus(review_source_cl)
    
    # Clean corpus
    corpus.cl <- tm_map(corpus.cl, content_transformer(tolower))
    corpus.cl <- tm_map(corpus.cl, removePunctuation)
    corpus.cl <- tm_map(corpus.cl, stripWhitespace)
    corpus.cl <- tm_map(corpus.cl, removeWords, stopwords("english"))
    #corpus <- tm_map(corpus, stemDocument, language = "english")
    #corpus <- stemCompletion(corpus.temp, corpus, type = "prevalent") # Try to stem words
    
    dtm.cl <- TermDocumentMatrix(corpus.cl)
    dtm.matrix.cl <- as.matrix(dtm.cl)
    
    if (uniqueKeyword == TRUE) {
      dtm.matrix.cl <- apply(dtm.matrix.cl, 2, function(x) ifelse(x > 0, 1, 0))
    }
    
    term.freq.cl <- rowSums(dtm.matrix.cl)
    temp.data.cl <- data.frame(year = vector.of.years[i],
                            word = names(term.freq.cl),
                            frequency = term.freq.cl)
    
    temp.data.cl <- arrange(temp.data.cl, desc(frequency))
    
    list.of.dataframes.cl[[i]] <- temp.data.cl
    
  }
  data.cl <- do.call(rbind, list.of.dataframes.cl)
}

data.cl.unique <- createTermFrequencyDf(suinos, uniqueKeyword = TRUE)
data.cl.1       <- createTermFrequencyDf(suinos, uniqueKeyword = FALSE)


#Tabela de frequencia de termos somando todos os anos
table_tm.cl <- 
  data.cl.1 %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))

terms.of.interest <- c("attenuated", "inactivated", "recombinant", "strain")

#Grafico frequencia de termos (1 por patente) por anos
chart_1<- data.unique %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))
#linha
ggplot(data = chart_1, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#coluna
ggplot(data = chart_1, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


#Grafico frequencia de termos total por anos
chart_2 <- data1 %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))
#linha
ggplot(data = chart_2, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#coluna
ggplot(data = chart_2, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())
