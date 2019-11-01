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

#remover documentos duplicados
#se nao for selecionar hospedeiro
data <- distinct(data, questel_id,.keep_all= TRUE)


# Selecionar banco de dados (ex: hospedeiro)
data <- filter(data, ï..host == "avian")

# Extracao do ano e pais de prioridade
data$year    <- str_extract(data$priority_number,"([\\d+]{4})")
data$year        <- as.numeric(data$year)  # Convert year as numeric

data$country <- str_extract(data$priority_number,"([A-z]{2})")

# data$country.rec <- fct_lump(data$country, n = 5) # Reduce the number of factors

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
# table_assignee$assignee.s   <- fct_lump(table_assignee$Latest.standardized.assignees...inventors.removed, n = 10) 
table_assignee$year.p    <- str_extract(table_assignee$Priority.dates,"([\\d+]{4})")


# 1.2 Analise dos dados ==========================================================

## Fig. 1.1 - Patentes x ano x Pais de prioridade  ---- Ok

# Tabela dos cinco principais paises por ano - fct_lump
# table_country <- data.frame(year = data$year, country = data$country)
# table_country$p.year <- as.character(table_country$year)
# table_country$p.year <- as.numeric(table_country$p.year)
# table_country$p.country <- fct_lump(table_country$country, n = 5)

# data %>% 
#   #  group_by(host) %>%
#   count(year, country.rec) %>%
#   ggplot(aes(year, n, fill = country.rec)) +
#   geom_bar(stat = "identity") + 
#   #  facet_wrap( ~ host, nrow = 2) +
#   theme_apa() + 
#   theme(legend.position = "bottom") +
#   ylab("") + xlab("") +
#   scale_fill_brewer(palette = "Dark2", type = "qual")


# Fig. 1.1 - Patentes x ano x Pais de prioridade
countries_vector <- 
  data %>%
  group_by(year, country) %>%
  count() %>%
  arrange(year, desc(n)) %>%
  top_n(n, n = 5) %>%
  filter(n >= 5)

countries_vector <- unique(countries_vector$country)
data$country.rec2 <- ifelse(data$country %in% countries_vector, 
                            data$country,
                            "Other")

#Fig 1.1 - Principais paises por ano - sem fct_lump
data %>%
  group_by(year, country.rec2) %>%
  count() %>%
  ggplot(aes(year, n, fill = country.rec2)) +
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")+
  scale_fill_brewer(palette = "Dark2", type = "qual")


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



# Plot Fig. 1.2.1
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
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")+
  scale_fill_brewer(palette = "Dark2", type = "qual") 


#Fig. 1.2.2 - Patentes x ano x IPC - 5 principais por ano - ipc completo 
# sem ser por fct_lump

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


# assignee_vector <- unique(assignee_vector$Latest.standardized.assignees...inventors.removed)
# table_assignee$assignee.rec <- ifelse(data$country %in% countries_vector, 
#                             data$country,
#                             "Other")
# 
# #Fig 1.1.2 - Principais paises por ano sem fct_lump
# data %>%
#   group_by(year, country.rec2) %>%
#   count() %>%
#   ggplot(aes(year, n, fill = country.rec2)) +
#   geom_bar(stat = "identity") + 
#   theme_apa() + 
#   theme(legend.position = "bottom") +
#   ylab("") + xlab("")+
#   scale_fill_brewer(palette = "Dark2", type = "qual")