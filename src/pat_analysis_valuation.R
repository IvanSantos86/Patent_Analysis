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
data <- read.csv2("~/GitHub/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE) 
#Linux
data <- read.csv2("~/Patent_Analysis/data/data_all.csv", 
                  stringsAsFactors = FALSE)

#remover documentos duuplicados
data1 <- distinct(data, questel_id, .keep_all = TRUE)

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
# Criar um dataframe as variáveis abaixo. Para isso, usar como identificador primário
# questel.id. Nosso banco de dados terá a seguinte estrutura:
# 1. questel.id, n.titulares, n.iventores, n.reivindicacoes, n.membros.
# Para juntar as colunas, usar left_join() do pacote dplyr. 
# 2. Aplicar uma matriz de correlação para ver se indicadores estão associados
# 3. Normalizar e padronizar indicadores numéricos
# 4. Aplicar K-médias.

# Patentes com maior numero de titulares
table_assignee %>%
  group_by(Questel.unique.family.ID..FAN.) %>%
  count() %>%
  arrange(desc(n))

# Patentes com maior numero de inventores
data %>%
  arrange(desc(inventors.fr)) %>%
  select(first.priority.number, inventors.fr) %>%
  head(., n = 10)

# Patentes com maior numero de reivindicacoes independentes
data %>%
  arrange(desc(indep.claim.fr)) %>%
  select(first.priority.number, indep.claim.fr) %>%
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


# Matriz de correlação ----------------------------------------------------

kmeans <- data %>%
  select()
