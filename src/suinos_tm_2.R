# Patent analysis using text mining
# Ivan Santos
## Vacinas suinos

library(tidyverse)
library(tm)
library(lubridate)

# Load Data
data <- read.csv2("~/Patent_Analysis/data/data_all.csv", 
                    stringsAsFactors = FALSE)

### 1. Evolução Temporal dos depósitos

## Data preparation ---------------------------------------------------------------

# Ano de prioridade
# Extração do ano de prioridade
data$p.year <- str_extract(data$priority_number,"([\\d+]{4})")

# Pais de prioridade
#Extração do país de prioridade
data$p.country <- str_extract(data$priority_number,"([A-z]{2})")


# Selecionar qual banco de dados você fará a análise
table(data$host)

table_country <- data.frame(data$p.year, data$p.country)
table_country$p.year <- as.character(table_country$p.year)
table_country$p.year <- as.numeric(table_country$p.year)
table_country$p.country <- fct_lump(table_country$p.country,
                                                    n = 5)  


#1.1 grafico: Patentes x Pais de prioridade x Ano
table_country %>% count(year, p.country) %>% 
  ggplot(aes(p.year, n, fill = p.country)) +
  geom_bar(stat = 'identity')


#IPCs
#Separar IPCs

a <- strsplit(data$ipc, "\n")
a <- max(sapply(a, length))

data <- separate(data, 
           col=  ipc,
           sep = "\n", 
           into = paste0("ipc.", 
                         seq(1, a)))

table_ipc <- gather(data, str_subset(names(data), "ipc."), 
                  key = "ipc",
                  value = "ipc.code") %>%
  filter(ipc.code != "") %>%
  select(p.year, ipc.code)

# Extrair IPC nivel classe
table_ipc$p.year  <- as.numeric(as.character((table_ipc$p.year)))
table_ipc$ipc.s   <- str_extract(table_ipc$ipc.code, "[^/]+")
table_ipc$ipc.s   <- fct_lump(table_ipc$ipc.s, n = 6) 
table_ipc$ipc.code   <- fct_lump(table_ipc$ipc.code, n = 6) 

table_ipc <- filter(table_ipc, ipc.s != "A61K-039")
table_ipc <- filter(table_ipc, !grepl("A61K-039*", ipc.code))

#1.2 grafico: Patentes x IPC x Ano
table_ipc  %>%  count(p.year, ipc.s)%>% 
  ggplot(aes(p.year,n,fill=ipc.s)) +
  geom_bar(stat='identity')

table_ipc  %>%  count(p.year, ipc.code)%>% 
  ggplot(aes(p.year,n,fill=ipc.code)) +
  geom_bar(stat='identity')


# Depositantes - we need to get back here when we're done -----------------
# 
# table_suinos_assignee <- read.csv2("~/GitHub/Patent_Analysis/data/suinos_sel_year.assignee.or.csv")
# 
# #1.3 grafico: Patentes x Depositante x Ano
# table_suinos_assignee  %>%  count(priority.date.1, assignee)%>% 
#   ggplot(aes(priority.date.1,n,fill=assignee)) +
#   geom_bar(stat='identity')


# Statistical text mining -------------------------------------------------

##Frequencia de termos por ano baseado no titulo e resumo
# Combinar colunas titulo e resumos
data$text.title.abstract <- paste(data$title, data$abstract,sep =" ")


# frequencia de termos por ano
#criar corpus
createTermFrequencyDf <- function (data, variable, uniqueKeyword = TRUE){
  
  # Create data frame with term frequency data
  # Arguments:
  #   data: dataframe which must have an year column
  #   distinct: do you need distinct keywords for each document
  #             or the summed keywords? Default = TRUE
  
  vector.of.years <- unique(data$p.year)
  list.of.dataframes <- list()
  
  for (i in 1:length(vector.of.years)){
    temp.data <- filter(data, p.year == vector.of.years[i])
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


data.unique <- createTermFrequencyDf(data, "text.title.abstract", uniqueKeyword = TRUE)
data1       <- createTermFrequencyDf(data, "text.titla.abstract", uniqueKeyword = FALSE)


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

#criar corpus
data.cl.unique  <- createTermFrequencyDf(data, "claims", uniqueKeyword = TRUE)
data.cl.1       <- createTermFrequencyDf(data, "claims", uniqueKeyword = FALSE)


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
