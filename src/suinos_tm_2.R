# Patent analysis 
# Ivan Santos, Ph.D.

library(tidyverse)
library(tm)
library(lubridate)
library(ggthemes)
library(jtools)

# Load Data
data <- read.csv2("~/Patent_Analysis/data/data_all.csv", 
                    stringsAsFactors = FALSE)

# Selecionar qual banco de dados você fará a análise: 
# Valores possíveis: "avian", "avian_flu", "bovin", 
#                    "suin", "mult_sp" 
# data <- filter(data, host == "avian")

### 1. Evolução Temporal dos depósitos de patentes

## Data preparation ---------------------------------------------------------------

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
           col=  ipc,
           sep = "\n", 
           into = paste0("ipc.", 
                         seq(1, a)))

rm(a)

# Combinar colunas titulo e resumos
data$text.title.abstract <- paste(data$title, 
                                  data$abstract,sep =" ")


## Frequencia de termos por ano baseado no titulo e resumo
# Frequencia de termos por ano
# Criar corpus

createTermFrequencyDf <- function (data, variable, uniqueKeyword = TRUE){
  
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

#criar corpus


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


# Charts ------------------------------------------------------------------

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
# 
# table_suinos_assignee <- read.csv2("~/GitHub/Patent_Analysis/data/suinos_sel_year.assignee.or.csv")
# 
# #1.3 grafico: Patentes x Depositante x Ano
# table_suinos_assignee  %>%  count(priority.date.1, assignee)%>% 
#   ggplot(aes(priority.date.1,n,fill=assignee)) +
#   geom_bar(stat='identity')
