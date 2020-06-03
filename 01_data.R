############################################################################
# Preamble
############################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../00_R_functions/preamble.R")

rm(list=ls())
### Load extra functions
source("00_functions.R")

### Load packages
library(tidyverse)
library(magrittr)

library(DBI) # GEneral R database interface
library(RPostgres) # PostgreSQL interface driver
library(dbplyr) # for dplyr with databases

### Database connection

# set up connection to existing PostgreSQL database, just plug in own details
con <- dbConnect(drv = RPostgres::Postgres(), 
                 dbname = "patstat2019",
                 host = "127.0.0.1", port = 5432,
                 user = "danieldb", password = "postgres2019")

# Inspect DB:
db_list_tables(con) %>% sort()

# Create the relevant lazy tibbles
appln <-tbl(con, "tls201_appln") 
appln_ipc <-tbl(con, "tls209_appln_ipc") 
docdb_fam_citn <-tbl(con, "tls228_docdb_fam_citn") 


############################################################################
# Main Selection of patents
############################################################################

# Patents with the right technology classes
ipc_select <- "(B60)%"

pat_ipc <- appln_ipc %>% 
  filter(ipc_class_symbol %similar to% ipc_select) %>%
  distinct(appln_id) %>%
  compute()

# Filter main patent information dataframe
pat_data <- appln %>%
  semi_join(pat_ipc, by = "appln_id") %>%
  filter(appln_id == earliest_filing_id & # Only Priority patents ,
           appln_id > 0 & appln_id <= 900000000 & # filter out surrogates, cf. PATSTAT manual p.97
           appln_filing_year >= 1970 & appln_filing_year <= 2017 & # Timeframe of analysis
           appln_kind == "A" & ipr_type == "PI"  # Only patented inventions cf. PATSTAT manual p.99 (maybe also "W")
         ) %>%
  select(appln_id, appln_filing_year, appln_auth, docdb_family_id, nb_citing_docdb_fam) %>%
  compute()

# xtract the DocDB IDs  
pat_docdb <- pat_data %>%  
  distinct(docdb_family_id) %>%
  compute()

pat_data %>% saveRDS("temp/pat_data.rds")

############################################################################
# DocDB Citations
############################################################################

el_doccdb_cit <- docdb_fam_citn %>% 
  semi_join(pat_docdb, by = c('docdb_family_id' = 'docdb_family_id')) %>%
  semi_join(pat_docdb, by = c('cited_docdb_family_id' = 'docdb_family_id')) %>%
  filter(docdb_family_id != cited_docdb_family_id) %>%
  compute()

el_doccdb_cit %>% saveRDS("temp/el_doccdb_cit.rds")

############################################################################
# DocDB IPC Classes
############################################################################
docdb_ipc <- appln_ipc %>% 
  select(appln_id, ipc_class_symbol) %>%
  inner_join(pat_data %>% select(appln_id, docdb_family_id), by = 'appln_id') %>%
  collect()

docdb_ipc %<>%
  mutate(ipc4 = ipc_class_symbol %>% str_sub(1, 4)) %>%
  select(-ipc_class_symbol) %>%
  group_by(docdb_family_id) %>%
  mutate(w = 1 / n()) %>%
  count(ipc4, wt = w) %>%
  ungroup()

docdb_ipc %>% saveRDS("temp/docdb_ipc.rds")

############################################################################
# DocDB IPC Citations
############################################################################
el_docdb_cit_ipc <- el_doccdb_cit %>%
  collect() %>%
  left_join(docdb_ipc, by = c('docdb_family_id' = 'docdb_family_id')) %>%
  left_join(docdb_ipc, by = c('cited_docdb_family_id' = 'docdb_family_id')) 

el_docdb_cit_ipc %>% saveRDS("temp/el_docdb_cit_ipc.rds")
  
############################################################################
# DocDB IPC Co_occurence
############################################################################

appln_ipc_el <- appln_ipc %>% 
  select(appln_id, ipc_class_symbol) %>%
  inner_join(pat_data, by = "appln_id") %>%
  compute()

mat  <- data %>% el_project(i = i, j = j)





