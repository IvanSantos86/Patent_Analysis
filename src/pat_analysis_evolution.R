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

# 1. Evolucao temporal dos depositos de patentes --------------------------
# 1.1 Preparacao dos dados ================================================

# Remover documentos duplicados
# Se nao for selecionar hospedeiro
data <- distinct(data, questel_id, .keep_all = TRUE)

# Selecionar banco de dados (e.g.: hospedeiro)
# data <- filter(data, host == "avian")

# Extracao do ano e pais de prioridade
data$year    <- str_extract(data$priority_numbers,"([\\d+]{4})")
data$year    <- as.numeric(data$year)  # Convert year as numeric
data$country <- str_extract(data$priority_numbers,"([A-z]{2})")


#criar intervalos de anos
data$year.inter <- ifelse(data$year <= 2001, "1998-2001", 
                          ifelse(data$year <= 2005, "2002-2005",
                                 ifelse(data$year <= 2009, "2006-2009",
                                        ifelse(data$year <= 2013, "2010-2013",
                                               ifelse(data$year <= 2017, "2014-2017", 
                                                      "Other")))))


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
colnames(table_assignee)[colnames(table_assignee)=="Latest.standardized.assignees...inventors.removed"] <- "assignee.name"


table_assignee$year.inter <- ifelse(table_assignee$year.p <= 2001, "1998-2001", 
                                    ifelse(table_assignee$year.p <= 2005, "2002-2005",
                                           ifelse(table_assignee$year.p <= 2009, "2006-2009",
                                                  ifelse(table_assignee$year.p <= 2013, "2010-2013",
                                                         ifelse(table_assignee$year.p <= 2017, "2014-2017", 
                                                                "Other")))))

# 1.2 Analise dos dados ==========================================================

## Fig. 1.1 - Patentes x ano x Pais de prioridade  ---- Ok

# Fig. 1.1 - table
countries_vector <- 
  data %>%
  group_by(year, country) %>%
  count()  %>%
  arrange(year, desc(n)) %>%
  group_by(year) %>%
  top_n(n = 5, n) %>% ##checar top_n
  filter(n >= 4) #necess?rio pois aparecem muitos paises diferentes

#Fig 1.1 - Patentes x ano x Pais de prioridade
ggplot(data = countries_vector, 
       aes(year, n,  fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Dark2", type = "qual") +
  theme_apa() + 
  ylab("") + xlab("") +
  theme(legend.position = "top")


# Fig. 1.2.1 Patentes x ano x IPC - 5 principais por ano - nivel classe ------
#Fig. 1.2.1 - Table
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

#
ipc_vector <-
  table_ipc %>%
  group_by(year.inter, ipc.s) %>%
  count() %>%
  arrange(year.inter, desc(n)) %>%
  group_by(year.inter) %>%
  top_n(n = 5, n)# %>%
#  filter(n >= 40)

ipc_vector <- unique(ipc_vector$ipc.s)
table_ipc$ipc.rec <- ifelse(table_ipc$ipc.s %in% ipc_vector, 
                             table_ipc$ipc.s,
                             "Other")

# Fig. 1.2.1 - Plot
table_ipc %>%
  filter(ipc.rec != "Other") %>%
  group_by(year.inter, ipc.rec) %>%
  count() %>%
  ggplot(aes(year.inter, n, fill = ipc.rec)) + ##como fazer grafico de linhas?
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("") 


#Fig. 1.2.2 - Patentes x ano x IPC - 5 principais por ano - ipc completo 

#Fig. 1.2.2 - Table - filtrar codigos tecnicos (C*)
table_ipc <- filter(table_ipc, grepl("C1*|C0*", ipc.code))

#Plot Fig.1.2.2
ipc_vector2 <-
  table_ipc %>%
  group_by(year.inter, ipc.code) %>%
  count() %>%
  arrange(year.inter, desc(n)) %>%
  group_by(year.inter) %>%
  top_n(n = 5, n)%>%
  filter(n >= 20)

ipc_vector2 <- unique(ipc_vector2$ipc.code)
table_ipc$ipc.rec2 <- ifelse(table_ipc$ipc.code %in% ipc_vector2, 
                             table_ipc$ipc.code,
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


# Fig. 1.3 - Patentes x ano x Depositantes

# Table assignee
assignee_vector <- 
  table_assignee %>%
  group_by(year.inter,assignee.name) %>%
  count()  %>%
  arrange(year.inter, desc(n)) %>%
  group_by(year.inter) %>%
  top_n(n = 5, n) %>% 
  filter(n >= 5)

assignee_vector <- unique(assignee_vector$assignee.name)
table_assignee$country.rec <- ifelse(table_assignee$assignee.name %in% assignee_vector, 
                                    table_assignee$assignee.name,
                                    "Other")

#Plot
table_assignee %>%
  filter(country.rec != "Other") %>%
  filter(country.rec != "422") %>%
  group_by(year.inter, country.rec) %>%
  count() %>%
  ggplot(aes(year.inter, n, fill = country.rec)) +
  geom_bar(stat = "identity") + 
  theme_apa() + 
  theme(legend.position = "bottom") +
  ylab("") + xlab("")
