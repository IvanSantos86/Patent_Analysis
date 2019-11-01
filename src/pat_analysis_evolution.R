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

# Load data assignee names reviewed
# Windows 
table_assignee <- read.csv2("~/GitHub/Patent_Analysis/data/patentes_all_assignee_or.csv ")
# Linux
table_assignee <- read.csv2("~/Patent_Analysis/data/patentes_all_assignee_or.csv")

# 1. Evolucao temporal dos depositos de patentes --------------------------
# 1.1 Preparacao dos dados ================================================

# Remover documentos duplicados
# Se nao for selecionar hospedeiro
data <- distinct(data, questel_id, .keep_all = TRUE)

# Selecionar banco de dados (e.g.: hospedeiro)
# data <- filter(data, host == "avian")

# Extracao do ano e pais de prioridade
data$year    <- str_extract(data$priority_number,"([\\d+]{4})")
data$year    <- as.numeric(data$year)  # Convert year as numeric
data$country <- str_extract(data$priority_number,"([A-z]{2})")

# Separacao dos IPCs (International Patent Classification)
a <- strsplit(data$ipc, "\n")
a <- max(sapply(a, length))

data <- separate(data, 
                 col =  ipc,
                 sep = "\n", 
                 into = paste0("ipc.", 
                               seq(1, a)))
rm(a)

#Preparar dados assignees
table_assignee$year.p <- str_extract(table_assignee$Priority.dates,"([\\d+]{4})")


# 1.2 Analise dos dados ==========================================================

## Fig. 1.1 - Patentes x ano x Pais de prioridade  ---- Ok

# Fig. 1.1 - Patentes x ano x Pais de prioridade
countries_vector <- 
  data %>%
  group_by(year, country) %>%
  count()  %>%
  arrange(year, desc(n)) %>%
  group_by(year) %>%
  top_n(n = 5, n) %>%
  filter(n >= 5)

countries_vector <- unique(countries_vector$country)
data$country.rec2 <- ifelse(data$country %in% countries_vector, 
                            data$country,
                            "Other")

#Fig 1.1 - Principais paises por ano
data %>%
  filter(country.rec2 != "Other") %>%
  group_by(year, country.rec2) %>%
  count() %>%
  ggplot(aes(year, n, fill = country.rec2)) +
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") +
  scale_fill_brewer(palette = "Dark2", type = "qual")


# Fig. 1.2.1 Patentes x ano x IPC - 5 principais por ano - nivel classe ------

#Table IPC
data$year.inter <- ifelse(data$year <= 2001, "1998-2001", 
                          ifelse(data$year <= 2005, "2002-2005",
                                 ifelse(data$year <= 2009, "2006-2009",
                                        ifelse(data$year <= 2013, "2010-2013",
                                               ifelse(data$year <= 2017, "2014-2017", 
                                                      "Other")))))

table_ipc <- gather(data, str_subset(names(data), "ipc."), 
                    key = "ipc",
                    value = "ipc.code") %>%
  filter(ipc.code != "") %>%
  select(year.inter, ipc.code)

# Extrair IPC nivel classe
table_ipc$ipc.s   <- str_extract(table_ipc$ipc.code, "[^/]+")

# Remover codigos que nao sao de interesse
table_ipc <- filter(table_ipc, ipc.s != "A61K-039")
table_ipc <- filter(table_ipc, !grepl("A61K-039*", ipc.code))

# Plot Fig. 1.2.1
ipc_vector <-
  table_ipc %>%
  group_by(year.inter, ipc.s) %>%
  count() %>%
  arrange(year.inter, desc(n)) %>%
  group_by(year.inter) %>%
  top_n(n, n = 5)# %>%
#  filter(n >= 40)

ipc_vector <- unique(ipc_vector$ipc.s)
table_ipc$ipc.rec2 <- ifelse(table_ipc$ipc.s %in% ipc_vector, 
                             table_ipc$ipc.s,
                             "Other")

table_ipc %>%
  filter(ipc.rec2 != "Other") %>%
  group_by(year.inter, ipc.rec2) %>%
  count() %>%
  ggplot(aes(year.inter, n, fill = ipc.rec2)) +
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") 


#Fig. 1.2.2 - Patentes x ano x IPC - 5 principais por ano - ipc completo 

#filtrar códigos tecnicos (C*)
table_ipc <- filter(table_ipc, grepl("C1*|C0*", ipc.code))

#Plot Fig.1.2.2
ipc_vector <- 
  table_ipc %>%
  group_by(year, ipc.code) %>%
  count() %>%
  arrange(year, desc(n)) %>%
  top_n(n, n = 5) %>%
  filter(n >= 15)

ipc_vector <- unique(ipc_vector$ipc.code)
table_ipc$ipc.rec3 <- ifelse(table_ipc$ipc.code %in% ipc_vector, 
                             table_ipc$ipc.code,
                             "Other")

table_ipc %>%
  group_by(year, ipc.rec3) %>%
  count() %>%
  ggplot(aes(year, n, fill = ipc.rec3)) +
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")


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


# principais assignees em cada ano
assignee_vector <- 
  table_assignee %>%
  group_by(year.p, Latest.standardized.assignees...inventors.removed) %>%
  count() %>%
  arrange(year.p, desc(n)) %>%
  top_n(n, n = 5) %>%
  filter(n >= 3)


assignee_vector <- unique(assignee_vector$Latest.standardized.assignees...inventors.removed)
table_assignee$ipc.rec4 <- ifelse(table_assignee$Latest.standardized.assignees...inventors.removed %in% assignee_vector, 
                             table_assignee$Latest.standardized.assignees...inventors.removed,
                             "Other")

table_assignee %>%
  group_by(year.p, ipc.rec4) %>%
  count() %>%
  ggplot(aes(year.p, n, fill = ipc.rec4)) +
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")