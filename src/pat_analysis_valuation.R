# Patent analysis 
# Ivan Santos, Ph.D.

library(tidyverse)
library(tm)
library(lubridate)
library(ggthemes)
library(jtools)
library(stringr)
#library(qdap)

# Load Data
#Windows
data <- read.csv2("~/GitHub/Patent_Analysis/data/full_data.csv", 
                  stringsAsFactors = FALSE) 
#Linux
data <- read.csv2("~/Patent_Analysis/data/full_data.csv", 
                  stringsAsFactors = FALSE)

# Load data assignee names reviewed
# Windows 
table_assignee <- read.csv2("~/GitHub/Patent_Analysis/data/patentes_all_assignee_or.csv ")
# Linux
table_assignee <- read.csv2("~/Patent_Analysis/data/patentes_all_assignee_or.csv")


#remover documentos duuplicados
data <- distinct(data, questel_id, .keep_all = TRUE)

# Selecionar banco de dados (e.g.: hospedeiro)
#data <- filter(data, host == "avian")


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
# Criar um dataframe as variaveis abaixo. 
#Para isso, usar como identificador primario questel.id. 
#Nosso banco de dados tera a seguinte estrutura:
# 1. questel.id, n.titulares, n.iventores, n.reivindicacoes, n.membros.
# Para juntar as colunas, usar left_join() do pacote dplyr. 
# 2. Aplicar uma matriz de correlacao para ver se indicadores estao associados
# 3. Normalizar e padronizar indicadores numericos
# 4. Aplicar K-medias.

# Patentes com maior numero de titulares
df_assignee <- 
  table_assignee %>%
  group_by(Questel.unique.family.ID..FAN.) %>%
  count() %>%
  arrange(desc(n))
colnames(df_assignee) <- c("questel_id", "n.assignees")

# Patentes com maior numero de inventores
df_inventors <- 
  data %>%
  arrange(desc(inventors.fr)) %>%
  select(questel_id, inventors.fr)

# Patentes com maior numero de reivindicacoes independentes
data %>%
  arrange(desc(indep.claim.fr)) %>%
  select(questel_id, indep.claim.fr) %>%
  head(., n = 10)

# FamPats com maior numero de membros
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


# Matriz de correlacao ----------------------------------------------------
# Preparar banco de dados
# Criar banco de dados
kmeans_df <- data[, c("questel_id", "inventors.fr", "indep.claim.fr", "n.unique.countries")]
kmeans_df <- left_join(kmeans_df, df_assignee, by = "questel_id")

# Plotar banco de dados para análise exploratória
#kcharts <- gather(kmeans_df, key = var, value = value, -questel_id)
#ggplot(kcharts, aes(x = value)) + geom_histogram() + 
#  facet_wrap(~ var)

# Escalar e centrar variáveis de interesse
kmeans_df_scaled <- kmeans_df
kmeans_df_scaled[, 2:5] <- sapply(kmeans_df_scaled[, 2:5], scale)

mds <- 
  kmeans_df_scaled %>%
  as.matrix() %>%
  dist() %>% #dissimilarity matrix 
  cmdscale() %>% # multidimensional scale - reduction for 2 dimensions
  as_tibble(.name_repair = "unique")
colnames(mds) <- c("Dim.1", "Dim.2")

datacluster <- as.matrix(mds)
negMat      <- negDistMat(datacluster, r = 2)
apmodel     <- apcluster(negMat)

# Passar clusteres para banco de dados original
kmeans_df_scaled$cluster <- as.factor(apcluster::labels(apmodel, type = "enum"))

library(ggpubr)
ggscatter(mds, x = "Dim.1", y = "Dim.2",
          color = "cluster",
          repel = TRUE)

View(kmeans_df_scaled)
