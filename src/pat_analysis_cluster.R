# Patent analysis 
# Ivan Santos, Ph.D.
library(tidyverse)
library(tm)
library(lubridate)
library(ggthemes)
library(jtools)
library(stringr)
#library(qdap)

# Clustering
library(cluster)
library(apcluster)

# Funcoes
source("GitHub/Patent_Analysis/src/utils.R")

# Load Data
#Windows
data <- read.csv2("~/GitHub/Patent_Analysis/data/full_data.csv", 
                  stringsAsFactors = FALSE) 
#Linux
data <- read.csv2("~/Patent_Analysis/data/full_data.csv", 
                  stringsAsFactors = FALSE)

#remover documentos duplicados
#se nao for selecionar hospedeiro
data <- distinct(data, questel_id,.keep_all = TRUE)

# Selecionar banco de dados (ex: hospedeiro)
data <- filter(data, host == "avian")


# 3. Agrupamentos das patentes ----------------------------------------------------


# 3.1 Affinity Propagation Clustering -----------------------------------------

#Preparacao dos dados
# Extracao do ano e pais de prioridade
data$year    <- str_extract(data$priority_numbers,"([\\d+]{4})")
data$year    <- as.numeric(data$year)  # Convert year as numeric

# Combinar colunas titulo e resumo
data$text.title.abstract <- paste(data$title, 
                                  data$abstract,sep = " ")

# Frequency Terms
title.abs_unique <- createTermFrequencyDf(data, 
                                          "text.title.abstract", 
                                          uniqueKeyword = TRUE, 
                                          rfidf = FALSE)
claims_unique  <- createTermFrequencyDf(data, "claims", uniqueKeyword = TRUE, 
                                        rfidf = FALSE)

# Tabela de frequencia de termos somando todos os anos - Tit/abs
chart_frequency<- 
  title.abs_unique %>%
  #claims_unique %>%
  group_by(word) %>%
  summarise(freq = sum(frequency)) %>%
  arrange(desc(freq))

##Usar chart_frequency para criar dicionario de exclusao de termos
generic.words <- c("vaccine", "vaccines", "invention", "relate", "thereof", 
                  "used", "application","present", "provide", "comprises", 
                  "disclosed", "discloses", "effective","effectively", 
                  "effects", "also","wherein", "according", "novel", 
                  "veterinary", "immun*", "can", "disease", "high", "low", 
                  "good", "protect", "animal", "number", "belonging", 
                  "claim","comprises", "comprising", "characterized", 
                  "contains", "containing", "select", "least", "amount", 
                  "group","said", "consists", "acceptable", "will", 
                  "pharmaceutically", "show", "obtain", "prevent")

data$text.title.abstract <- tolower(data$text.title.abstract)
data$text.title.abstract <- removeWords(data$text.title.abstract, generic.words)

# 3.3.1. Criar matrix patente vs. termo - Tit/Abs ------
matrix <- data.frame(doc_id = seq(1:nrow(data)),
                       text = data$text.title.abstract)

corpus.matrix <- VCorpus(DataframeSource(matrix))
corpus.matrix <- cleanCorpus(corpus.matrix)

ptm<- DocumentTermMatrix(corpus.matrix, control = list(weighting = weightTfIdf))
ptm <- as.matrix(ptm)

# Muldimensional scaling for ploting data
mds <- 
  ptm %>%
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
#heatmap(apmodel, negMat)

# Passar clusteres para banco de dados original
data$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))


# 3.3.2. Criar matrix patente vs. termo - Claims ----- 
matrix <- data.frame(doc_id = seq(1:nrow(data)),
                     text = data$claims)

corpus.matrix <- VCorpus(DataframeSource(matrix))
corpus.matrix <- cleanCorpus(corpus.matrix)

tdm_matrix <- DocumentTermMatrix(corpus.matrix)
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
apmodel     <- apcluster(negMat, details=TRUE)

show(apmodel)
plot(apmodel, datacluster)
#heatmap(apmodel, negMat)

# Passar clusteres para banco de dados original
data$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))

# Filtrar cluster
frequentTermCluster(data, n_cluster = 1)  

# plotar experimentalmente os clusteres ----
# Criar variaveis para plotar abaixo
mds$country.r <- data$country.rec
mds$year <- data$year
mds$year.r <- data$year.inter
mds$id <- data$questel_id
mds$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))
mds$host <- data$host

# Plot clusters
library(ggpubr)
ggscatter(mds, x = "Dim.1", y = "Dim.2",
          color = "cluster",
          repel = TRUE) +
  facet_wrap(~ year.r) 

ggscatter(mds, x = "Dim.1", y = "Dim.2",
          color = "cluster",
          repel = TRUE) +
  facet_wrap(~ country.r) 



##3.2: Associacoes entre termos -------------------
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

### 3.3 Word Network ------------
recombinant <- data[grep("recombinant", data$text.title.abstract,ignore.case = TRUE),]
word_network_plot(recombinant$text.title.abstract[1:10], stopwords = "english")

word_associate(word_ass$text,match.string = c('recombinant'),
               stopwords = Top200Words,network.plot = T,
               cloud.colors = c("gray85","darkred"))
