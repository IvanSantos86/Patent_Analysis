# Patent analysis 
# Ivan Santos, Ph.D.

library(tidyverse)
library(tm)
library(lubridate)
library(ggthemes)
library(jtools)
library(stringr)

# Clustering
library(FactoMineR)
library(factoextra)
library(cluster)
library(apcluster)



#funcoes
source("GitHub/Patent_Analysis/src/utils.R")
# Linux
source("src/utils.R")

# Load Data
#Windows
data <- read.csv2("~/GitHub/Patent_Analysis/data/full_data.csv", 
                  stringsAsFactors = FALSE) 
#Linux
data <- read.csv2("data/full_data.csv", 
                  stringsAsFactors = FALSE)

# Remover documentos duplicados
# Se nao for selecionar hospedeiro
data <- distinct(data, questel_id, .keep_all = TRUE)

# Selecionar banco de dados (ex: hospedeiro)
# data <- filter(data, host == "avian")

# Extracao do ano de prioridade
data$year <- str_extract(data$priority_number,"([\\d+]{4})")
data$year <- as.numeric(data$year)  # Convert year as numeric

#criar intervalos de anos
data$year.inter <- ifelse(data$year <= 2001, "1998-2001", 
                          ifelse(data$year <= 2005, "2002-2005",
                                 ifelse(data$year <= 2009, "2006-2009",
                                        ifelse(data$year <= 2013, "2010-2013",
                                               ifelse(data$year <= 2017, "2014-2017", 
                                                      "Other")))))

# Extracao do pais de prioridade
data$country <- str_extract(data$priority_number,"([A-z]{2})")

# Retira códigos dos IPC's, remove duplicados e criar coluna ipc.resumido
removeDuplicatedCodes <- function(x) {
  tmp <- lapply(x, function(x) str_extract(x, "[^/]+"))
  tmp <- unique(tmp)
  tmp <- paste(tmp, collapse = " ")
  return(tmp)
}

a <- strsplit(data$ipc, "\n")

results <- list()
for (i in 1:length(a)) {
  results[i] <- removeDuplicatedCodes(a[[i]]) 
}

data$ipc.resumido <- unlist(results)


# 2 Mineracao do texto -------------------------------------
View(sort(table(data$ipc.resumido), decreasing = TRUE))

# Achar os termos mais frequentes
findFreqTerms(ptm)

# Associação entre termos
findAssocs(ptm, "a61k-009", 0.1)
findAssocs(ptm, "c12n-015", 0.05)


## Clusters 
## Propagação por afinididade
matrix <- data.frame(doc_id = seq(1:nrow(data)),
                       text = data$ipc.resumido)

corpus.matrix <- VCorpus(DataframeSource(matrix))

ptm <- DocumentTermMatrix(corpus.matrix,
                          control = list(weighting = weightTfIdf))

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
heatmap(apmodel, negMat)

fviz_nbclust(ptm, kmeans, method = "silhouette", k.max = 20)
fviz_nbclust(ptm, kmeans, method = "wss", k.max = 20)


# K Médias
km.res <- kmeans(ptm, 18, nstart = 25)

fviz_cluster(km.res, 
             data = ptm, 
             stand = TRUE,
             show.clust.cent = TRUE,
             xlim = c(0, 10),
             ylim = c(-1, 0))
