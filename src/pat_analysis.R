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

# Load data assignee names reviewed
# Windows 
table_assignee <- read.csv2("~/GitHub/Patent_Analysis/data/patentes_all_assignee_or.csv ")
# Linux
table_assignee <- read.csv2("~/Patent_Analysis/data/patentes_all_assignee_or.csv")


# 1. Evolucao temporal dos depositos de patentes --------------------------

# 1.1 Preparacao dos dados ================================================

# Selecionar qual banco de dados (hospedeiro)
data <- filter(data, ï..host == "avian")

# Extracao do ano e pais de prioridade
data$year    <- str_extract(data$priority_number,"([\\d+]{4})")
data$year        <- as.numeric(data$year)  # Convert year as numeric

data$country <- str_extract(data$priority_number,"([A-z]{2})")

data$country.rec <- fct_lump(data$country, # Reduce the number of factors
                             n = 5) 

# Separacao dos IPCs (International Patent Classification)
a <- strsplit(data$ipc, "\n")
a <- max(sapply(a, length))

data <- separate(data, 
                 col =  ipc,
                 sep = "\n", 
                 into = paste0("ipc.", 
                               seq(1, a)))

rm(a)


##Depositantes
#Preparar dados assignees
table_assignee$assignee.s   <- fct_lump(table_assignee$Latest.standardized.assignees...inventors.removed, n = 10) 
table_assignee$year.p    <- str_extract(table_assignee$Priority.dates,"([\\d+]{4})")



# 1.2 Analise dos dados ==========================================================

## Fig. 1.1 - Patentes x ano x Pais de prioridade  ---- Ok

# Tabela dos cinco principais paises por ano - fct_lump
table_country <- data.frame(year = data$year, country = data$country)
table_country$p.year <- as.character(table_country$year)
table_country$p.year <- as.numeric(table_country$p.year)
table_country$p.country <- fct_lump(table_country$country, n = 5)

# principais paises de prioridade em cada ano
countries_vector <- 
  data %>%
  group_by(year, country) %>%
  count() %>%
  arrange(year, desc(n)) %>%
  top_n(n, n = 5) %>%
  filter(n >= 4)

countries_vector <- unique(countries_vector$country)
data$country.rec2 <- ifelse(data$country %in% countries_vector, 
                            data$country,
                            "Other")

data %>% 
#  group_by(host) %>%
  count(year, country.rec) %>%
  ggplot(aes(year, n, fill = country.rec)) +
  geom_bar(stat = "identity") + 
#  facet_wrap( ~ host, nrow = 2) +
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") +
  scale_fill_brewer(palette = "Dark2", type = "qual")

#Fig 1.1.2 - Principais paises por ano sem fct_lump
data %>%
  group_by(year, country.rec2) %>%
  count() %>%
  ggplot(aes(year, n, fill = country.rec2)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")


# Fig. 1.2.1 Patentes x ano x IPC - 5 principais por ano - nivel classe ------

#Table IPC
table_ipc <- gather(data, str_subset(names(data), "ipc."), 
                    key = "ipc",
                    value = "ipc.code") %>%
  filter(ipc.code != "") %>%
  select(year, ipc.code)

# Extrair IPC nivel classe
table_ipc$ipc.s   <- str_extract(table_ipc$ipc.code, "[^/]+")
#table_ipc$ipc.s   <- fct_lump(table_ipc$ipc.s, n = 6) 
#table_ipc$ipc.code   <- fct_lump(table_ipc$ipc.code, n = 6) 

# Remover codigos que nao sao de interesse
table_ipc <- filter(table_ipc, ipc.s != "A61K-039")
table_ipc <- filter(table_ipc, !grepl("A61K-039*", ipc.code))
table_ipc <- filter(table_ipc, grepl("C12N-*", ipc.code)) # Caso queira fazer analise com C12N

# sem ser por fctlump
ipc_vector <-
  table_ipc %>%
  group_by(year, ipc.s) %>%
  count() %>%
  arrange(year, desc(n)) %>%
  top_n(n, n = 5) %>%
  filter(n >= 40)

ipc_vector <- unique(ipc_vector$ipc.s)
table_ipc$ipc.rec2 <- ifelse(table_ipc$ipc.s %in% ipc_vector, 
                             table_ipc$ipc.s,
                            "Other")

table_ipc %>%
  group_by(year, ipc.rec2) %>%
  count() %>%
  ggplot(aes(year, n, fill = ipc.rec2)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") 


#Fig. 1.2.2 - Patentes x ano x IPC - 5 principais por ano - ipc completo
# sem ser por fct_lump
ipc_vector <- 
  table_ipc %>%
  group_by(year, ipc.code) %>%
  count() %>%
  arrange(year, desc(n)) %>%
  top_n(n, n = 5) %>%
  filter(n >= 15)

ipc_vector <- unique(ipc_vector$ipc.code)
table_ipc$ipc.rec2 <- ifelse(table_ipc$ipc.code %in% ipc_vector, 
                             table_ipc$ipc.code,
                             "Other")

table_ipc %>%
  group_by(year, ipc.rec2) %>%
  count() %>%
  ggplot(aes(year, n, fill = ipc.rec2)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")

# Plot Fig. 1.2
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


# Fig. 1.3 - Patentes x ano x Depositantes
# Table assignee
table_count_assignees <- 
  table_assignee %>%
  group_by(assignee.s) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))

#Plot
table_assignee %>% count(year.p, assignee.s) %>% 
  ggplot(aes(x = year.p, y = n, fill = assignee.s)) +
  geom_bar(stat = 'identity')


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
fraction.component <- c("subunit", "fragment", 
                        "fraction", "protein*", "polypeptide",
                        "peptide", "virus-like", "viruslike", 
                        "like", "particle*", "vlp", "rna", 
                        "dna", "nucleic", "nucleotide", "vector", 
                        "plasmid*", "epitope", "engineering", 
                        "heterologous", "recombined", "pcr", 
                        "deletion*", "mutation*", "seq", "seq.", 
                        "sequence", "deleted", "replicon", "gene*", 
                        "genus", "plant", "edible")


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


# Fig2.4: Frequencia de termos total por anos - reivindica??es
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

# 1. Criar matrix patente vs. termo
tdm_wa <- DocumentTermMatrix(corpus.wa, control = list(weighting = weightTfIdf))
tdm_wa <- as.matrix(tdm_wa)

# Muldimensional scaling for ploting data
mds <- 
  tdm_wa %>%
  dist() %>% #dissimilarity matrix 
  cmdscale() %>% # multidimensional scale - reduction for 2 dimensions
  as_tibble(.name_repair = "unique")
colnames(mds) <- c("Dim.1", "Dim.2")

# Preparar dados para algoritmo de propagacao por afinidade.
# datacluster <- dist(tdm_wa) 
datacluster <- as.matrix(mds)
negMat      <- negDistMat(datacluster, r = 2)
apmodel     <- apcluster(negMat)

# Passar clusteres para banco de dados original
data$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))

# Filtrar cluster

frequentTermCluster <- function(data, n_cluster) {
  
  df_filtered <- subset(data, data$cluster == n_cluster)
  
  corpus_filtered <- data.frame(doc_id = seq(1:nrow(df_filtered)),
                                text = df_filtered$text.title.abstract)
  
  corpus_filtered <- VCorpus(DataframeSource(corpus_filtered))
  corpus_filtered <- clean.corpus(corpus_filtered)
  tda_filtered <- TermDocumentMatrix(corpus_filtered, 
                                     control = list(weighting = weightTfIdf))
  
  tda_filtered <- as.matrix(tda_filtered)
  words_list <- rowSums(tda_filtered)
  terms <- sort(words_list, decreasing = TRUE)
  
  return(terms[1:10])
  
}

frequentTermCluster(data, n_cluster = i)  


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


# 4 Analise de valor das patentes ----------------------------------------------

# 4.1 Create attributes for patent valuation -----------------------------------
# 1. Select the first priority number
data$first.priority.number <- gsub("\n(.*)", "", data$priority_number)

# 2. Count inventors
data$inventors.fr <- str_count(data$inventors, pattern = "\n") + 1

# 3. Count independent claims
data$indep.claim.fr <-  str_count(data$indep_claim, pattern = "\n") 

# 4. Count frequency of patents by country

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


# Criar variavel para contagem de paises unicos por patente
n <- lapply(temp, function(x) length(x))
data$n.unique.countries <- unlist(n) 

# Criar tabela com numero de patentes por pais
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



# 4.2 Analise dos dados ==========================================================

#Patentes com maior numero de titulares
table_assignee %>%
  group_by(Questel.unique.family.ID..FAN.) %>%
  count() %>%
  arrange(desc(n))

#Patentes com maior numero de inventores
data %>%
  arrange(desc(inventors.fr)) %>%
  select(first.priority.number, inventors.fr) %>%
  head(., n = 10)

#Patentes com maior numero de reivindicacoes independentes
data %>%
  arrange(desc(indep.claim.fr)) %>%
  select(first.priority.number, indep.claim.fr) %>%
  head(., n = 10)

#FamPats com maior numero de membros
data %>%
  arrange(desc(n.unique.countries)) %>%
  select(first.priority.number, n.unique.countries) %>%
  head(., n = 10)


# Patentes por pais de deposito
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
