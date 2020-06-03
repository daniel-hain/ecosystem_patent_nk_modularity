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

# Load data
pat_data <- readRDS("temp/pat_data.rds")

############################################################################
# DocDB IPC Citations
############################################################################

docdb_ipc <- readRDS("temp/docdb_ipc.rds")

el_docdb_cit_ipc <- readRDS("temp/el_docdb_cit_ipc.rds") %>%  
  rename(i = ipc4.x,
         j = ipc4.y,
         i_n = n.x,
         j_n = n.y)

# Restrict original list
docdb_select <- docdb_ipc %>% 
  count(ipc4, wt = n, sort = TRUE) %>%
  filter(n >= 1000) %>%
  pull(ipc4)

# GEnerating ipc class edgelist
el <- el_docdb_cit_ipc %>%
  filter(i %in% docdb_select & j %in% docdb_select) %>%
  group_by(i, j) %>%
  summarise(w = sum(i_n * j_n)) %>%
  ungroup() %>%
  filter(w >= 10) %>%
  mutate(w = w %>% round(0)) 

############################################################################
# Blockmodeling IPC / Citations
############################################################################

library(blockmodeling)
library(Matrix)
library(igraph)
library(tidygraph)

g <- el %>% graph.data.frame(directed = TRUE) 

g <- delete_vertices(g, strength(g, mode = "all") < 75)

mat <- g %>% get.adjacency(sparse = FALSE, attr='w')

diag(mat) <- 0

bm <- mat %>% optRandomParC(k = 4, 
                            rep = 10, 
                            mingr = 5,
                            #isSym = FALSE,
                            #diag = FALSE,
                            approaches = "val", 
                            blocks = c("com", "reg", "rfn", "cfn", "avg",  "nul")
                            )

plot(bm)

