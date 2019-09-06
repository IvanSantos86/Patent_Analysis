# Patent analysis 
# Ivan Santos, Ph.D.

library(tidyverse)
library(tm)
library(lubridate)
library(ggthemes)
library(jtools)
library(qdap)

# Load Data
data <- read.csv2("~/GitHub/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE)

# Selecionar qual banco de dados você fará a análise: 
# Valores possíveis: "avian", "avian_flu", "bovin", 
#                    "suin", "mult_sp" 
# data <- filter(data, host == "avian")


## Data preparation ---------------------------------------------------------------
# Selecionar qual banco de dados você fará a análise
data <- filter(data, host == "avian")

table_country <- data.frame(data$p.year, data$p.country)
table_country$p.year <- as.character(table_country$p.year)
table_country$p.year <- as.numeric(table_country$p.year)
table_country$p.country <- fct_lump(table_country$p.country, n = 5)  


# Extração do ano e país de prioridade
data$year    <- str_extract(data$priority_number,"([\\d+]{4})")
data$country <- str_extract(data$priority_number,"([A-z]{2})")

data$year        <- as.numeric(data$year)  # Convert year as numeric
data$country.rec <- fct_lump(data$country, # Reduce the number of factors
                             n = 5)  

# Separar IPCs
a <- strsplit(data$ipc, "\n")
a <- max(sapply(a, length))

data <- separate(data, 
                 col =  ipc,
                 sep = "\n", 
                 into = paste0("ipc.", 
                               seq(1, a)))

rm(a)

# Combinar colunas titulo e resumos
data$text.title.abstract <- paste(data$title, 
                                  data$abstract,sep = " ")


## Frequencia de termos por ano baseado no titulo e resumo
# Frequencia de termos por ano
# Criar corpus

createTermFrequencyDf <- function(data, variable, uniqueKeyword = TRUE){
  
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

# Create Frequency Terms for Title+Abstracts
title_abs_unique <- createTermFrequencyDf(data, "text.title.abstract", uniqueKeyword = TRUE)
title_abs_freq   <- createTermFrequencyDf(data, "text.titla.abstract", uniqueKeyword = FALSE)


# Create Frequency Terms for Claims
claims_unique  <- createTermFrequencyDf(data, "claims", uniqueKeyword = TRUE)
claims_freq    <- createTermFrequencyDf(data, "claims", uniqueKeyword = FALSE)


#Tabela de frequencia de termos somando todos os anos
chart_2.1 <- 
  title_abs_unique %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))

terms.of.interest <- c("attenuated", "inactivated", "recombinant", "strain")

#2.1 Grafico: Frequencia de termos (1 por patente) x Ano
chart_1 <- data.unique %>%
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

#criar corpus


#Tabela de frequencia de termos somando todos os anos
table_tm.cl <- 
  data.cl.1 %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))

terms.of.interest <- c("attenuated", "inactivated", "recombinant", "strain")

#Grafico frequencia de termos (1 por patente) por anos
chart_3 <- data.cl.unique %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))
#linha
ggplot(data = chart_3, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#coluna
ggplot(data = chart_3, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


#Grafico frequencia de termos total por anos
chart_4 <- data.cl.1 %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))
#linha
ggplot(data = chart_4, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#coluna
ggplot(data = chart_4, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


# Charts ------------------------------------------------------------------

# 1. Evolução Temporal dos depósitos de patentes ----

# Gráfico com todos os tipos de hospedeiros
## Fig1.1 - Todos os tipos de hosts ----
data %>% 
  group_by(host) %>%
  count(year, country.rec) %>%
  ggplot(aes(year, n, fill = country.rec)) +
  geom_bar(stat = "identity") + 
  facet_wrap( ~ host, nrow = 2) +
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") +
  scale_fill_brewer(palette = "Dark2", type = "qual")


# Fig1.2. IPC por ano ----
table_ipc <- gather(data, str_subset(names(data), "ipc."), 
                    key = "ipc",
                    value = "ipc.code") %>%
  filter(ipc.code != "") %>%
  select(year, ipc.code)

# Extrair IPC nivel classe
table_ipc$ipc.s   <- str_extract(table_ipc$ipc.code, "[^/]+")
table_ipc$ipc.s   <- fct_lump(table_ipc$ipc.s, n = 6) 
table_ipc$ipc.code   <- fct_lump(table_ipc$ipc.code, n = 6) 

# Remover códigos que já estavam na consulta da base de dados
table_ipc <- filter(table_ipc, ipc.s != "A61K-039")
table_ipc <- filter(table_ipc, !grepl("A61K-039*", ipc.code))

# Plot figure
table_ipc %>% 
  count(year, ipc.s) %>% 
  ggplot(aes(year, n, fill = ipc.s, col = ipc.s)) +
  #geom_bar(stat='identity') +
  geom_area() +
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") +
  scale_fill_brewer(palette = "Dark2", type = "qual") +
  scale_color_brewer(palette = "Dark2", type = "qual") 

# Depositantes - we need to get back here when we're done -----------------
# Passar dados
table_suinos_assignee <- read.csv2("~/Patent_Analysis/data/suinos_sel_year.assignee.or.csv")
table_suinos_assignee$assignee.s   <- fct_lump(table_suinos_assignee$assignee, n = 10) 

# #1.3 grafico: Patentes x Depositante x Ano
table_suinos_assignee %>% count(priority.date.1, assignee.s) %>% 
  ggplot(aes(x = priority.date.1, y = n, fill = assignee.s)) +
  geom_bar(stat = 'identity')

# Count assignees
table_count_assignees <- 
  table_suinos_assignee %>%
  group_by(assignee) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))


# Create attributes for patent valuation -----------------------------------

# 1. Select the first priority number
data$first.priority.number <- gsub("\n(.*)", "", data$priority_number)

# 2. Count inventors
data$inventors.fr <- str_count(data$inventors, pattern = "\n") + 1

# Count independent claims
data$indep.claim.fr <-  str_count(data$indep_claim, pattern = "\n") 

# Count frequency of patents by country

first <- str_extract(data$family_publication, "[A-Z]{2}")
temp <- str_extract_all(data$family_publication, "\n[A-Z]{2}")
temp <- lapply(temp, function(x) gsub("\n", "", x))
temp <- lapply(temp, function(x) c(x))
temp <- lapply(temp, function(x) unique(x))
temp <- lapply(temp, function(x) paste0(x, collapse = "|"))
temp <- unlist(temp)
temp <- paste(first , temp, sep = "|")
temp <- str_extract_all(temp, "[A-Z]{2}")
temp <- lapply(temp, function(x) unique(x))


# Criar variável para contagem de países únicos por patente
n <- lapply(temp, function(x) length(x))
data$n.unique.contries <- unlist(n) 

# Criar tabela com número de patentes por país
temp <- lapply(temp, function(x) paste0(x, collapse = "|"))
data$countries.list <- unlist(temp)
a <- strsplit(data$countries.list, "\\|")
a <- max(sapply(a, length))

data <- separate(data, 
                 col =  countries.list,
                 sep = "\\|", 
                 into = paste0("country.", 
                               seq(1, a)))

rm(a, temp, n)

table_country <- 
  gather(data, str_subset(names(data), "country\\.[1-99]"),
         key = "country",
         value = "country.code") %>%
  filter(country.code != "") %>%
  select(country.code) %>%
  group_by(country.code) %>%
  count(country.code) %>%
  arrange(desc(n)) %>%
  filter(country.code != "WO")


# Word Associations -------------------------------------------------------

## Associações com os termos "attenuated", "inactivated", "recombinant", "strain"
word_ass <- data.frame(doc_id=seq(1:nrow(data)),text=data$text.title.abstract)

corpus.wa <- VCorpus(DataframeSource(word_ass))

clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

corpus.wa <- clean.corpus(corpus.wa)

tdm.wa <- TermDocumentMatrix(corpus.wa,control = list(weighting = weightTf))

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

### Word Network
recombinant <- data[grep("recombinant", data$text.title.abstract,ignore.case = TRUE),]
word_network_plot(recombinant$text.title.abstract[1:10], stopwords = "english")

word_associate(word_ass$text,match.string = c('recombinant'),
               stopwords = Top200Words,network.plot = T,
               cloud.colors = c("gray85","darkred"))

