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


# Load Data
#Windows
data <- read.csv2("~/GitHub/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE) 
#Linux
data <- read.csv2("~/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE)


# Selecionar banco de dados (ex: hospedeiro)
data <- filter(data, ï..host == "avian")

# Extracao do ano e pais de prioridade
data$year    <- str_extract(data$priority_number,"([\\d+]{4})")
data$year        <- as.numeric(data$year)  # Convert year as numeric

data$country <- str_extract(data$priority_number,"([A-z]{2})")

data$country.rec <- fct_lump(data$country, # Reduce the number of factors
                             n = 5)

# 2 Mineracao do texto -------------------------------------

# 2.1 Preparacao dos dados ===================================================

# Combinar colunas titulo e resumo
data$text.title.abstract <- paste(data$title, 
                                  data$abstract,sep = " ")

## Frequencia de termos por ano baseado no titulo+resumo ou reivindicacoes
# Frequencia de termos por ano


# Frequency Terms for Title+Abstracts
title_abs_unique <- createTermFrequencyDf(data, 
                                          "text.title.abstract", 
                                          uniqueKeyword = TRUE, 
                                          rfidf = FALSE)

title_abs_freq   <- createTermFrequencyDf(data, "text.title.abstract", 
                                          uniqueKeyword = FALSE, 
                                          rfidf = FALSE)

# Frequency Terms for Claims
claims_unique  <- createTermFrequencyDf(data, "claims", uniqueKeyword = TRUE, 
                                        rfidf = FALSE)

claims_freq    <- createTermFrequencyDf(data, "claims", uniqueKeyword = FALSE, 
                                        rfidf = FALSE)

# Criar Dicionario
inactivated <- c("inactivat*", "split", "kill*", "death")
attenuated <- c("live", "attenuat*", "weak*")
fraction.component <- c("subunit vaccin*", "recombin* protein*", 
                         "recombin* polypeptide*", "recombin* peptide*", 
                         "protein vaccin*", "peptide vaccin*", 
                         "polypeptide vaccin*", "empty", "virosome*",
                         "virus-like", "viruslike", "vlp", "dna vaccin*", 
                         "nucleic acid* vaccin*", "deoxyribonucleic acid vaccin*",
                         "polynucleotide vaccin",  "rna", "recombinant vector*", 
                         "viral vector*")



# Usar estes descritores num segundo momento
#vaccine <- c("vaccin*", "preparation*", "composition*", "immunogen*", "antigen*")
#strain <- c("strain*", "serotype*", "genotype*", "cepa")
#synthetic <- c("synthetic")
#glycoconjugated <- c("glycoconjugated", "glycoprotein", "glycan")
#chimeric <- c("chimeric")
#diva <- c("diva", "marker")
#multivalent <- c("multivalent", "polyvalent", "bivalent", "trivalent", "tetravalent")
#adjuvant <- c("adjuvant*", "stabilizer*")
#delivery <- c("adminstrat", "parenteral", "mucosal", "nasal", "intranasal", "injected")
#process <- c("process", "method*", "step*", "purification")
#diagnosis <- c("diagnos*", "kit*")


# Criar variaveis para identificar se o dicionario esta presente numa determinada claim

data$is.inactivated <- ifelse(
  str_detect(data$claims, paste0(inactivated, collapse = "|")),
  1,
  0)

data$is.attenuated <- ifelse(
  str_detect(data$claims, paste0(attenuated, collapse = "|")),
  1,
  0)

data$is.fraction.component <- ifelse(
  str_detect(data$claims, paste0(fraction.component, collapse = "|")),
  1,
  0)


# Criar variaveis para identificar se o dicionario esta presente num determinado no ti/abs

data$is.inactivated <- ifelse(
  str_detect(data$text.title.abstract, paste0(inactivated, collapse = "|")),
  1,
  0)

data$is.attenuated <- ifelse(
  str_detect(data$text.title.abstract, paste0(attenuated, collapse = "|")),
  1,
  0)

data$is.fraction.component <- ifelse(
  str_detect(data$text.title.abstract, paste0(fraction.component, collapse = "|")),
  1,
  0)


# 2.2 Analise dos dados ==========================================================

# Tabela de frequencia de termos somando todos os anos - Tit/abs
chart_2.1 <- 
  title_abs_unique %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))

# Plotar grafico de linhas por tipo do dicionario por ano
chart <- 
  data %>%
  group_by(year) %>%
  summarise(
    inactivated = sum(is.inactivated) / n(),
    attenuated = sum(is.attenuated) / n(),
    fraction.component = sum(is.fraction.component) / n())

chart <- gather(chart, key = tipo, value = valor, -year)

ggplot(chart, aes(x = year, y = valor, color = tipo)) +
  geom_line()

ggplot(chart, aes(x = year, y = valor, color = tipo)) +
  geom_col()


# Plotar grafico de linhas por tipo do dicionario por ano
chart <- 
  data %>%
  group_by(year) %>%
  summarise(
    inactivated = sum(is.inactivated) / n(),
    attenuated = sum(is.attenuated) / n(),
    fraction.component = sum(is.fraction.component) / n())

chart <- gather(chart, key = tipo, value = valor, -year)

ggplot(chart, aes(x = year, y = valor, color = tipo)) +
  geom_line()

ggplot(chart, aes(x = year, y = valor, color = tipo)) +
  geom_col()


# Fig 2.1: Frequencia de termos de interesse (1 por patente) x Ano
chart_1 <- title_abs_unique %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(as.character(year), format = "%Y")),
         year_cut = cut(year, breaks = 2))


# Grafico de linha
ggplot(data = chart_1, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

#Grafico de coluna
ggplot(data = chart_1, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


#Fig2.2: frequencia de termos de interesse (total) x Ano
chart_2 <- title_abs_freq %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))

# Grafico de linha
ggplot(data = chart_2, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

# Grafico de coluna
ggplot(data = chart_2, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


#Tabela de frequencia de termos somando todos os anos - reivindica??es
table_tm.cl <- 
  data.cl.1 %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))


# Fig2.3: Frequencia de termos (1 por patente) por anos - reivindica??es
chart_3 <- data.cl.unique %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))

# Grafico de linha
ggplot(data = chart_3, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

# Grafico de coluna
ggplot(data = chart_3, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())


# Fig2.4: Frequencia de termos total por anos - reivindicacoes
chart_4 <- data.cl.1 %>%
  filter(word %in% terms.of.interest) %>%
  mutate(year = year(as.Date(year, format = "%Y")),
         year_cut = cut(year, breaks = 2))

# Grafico de linha
ggplot(data = chart_4, 
       aes(x = year, y = frequency,
           fill = word, colour = word)) +
  geom_line()

# Grafico de coluna
ggplot(data = chart_4, 
       aes(x = year_cut, fill = word)) +
  geom_bar(position = position_dodge())