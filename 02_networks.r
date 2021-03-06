############################################################################
# Preamble
############################################################################
rm(list=ls())
### Load extra functions
source("00_functions.R")

### Load packages
library(tidyverse)
library(magrittr)

# Load data
pat_data <- readRDS("../input/pat_data.rds")

############################################################################
# IPC classes
############################################################################

appln_ipc <- readRDS("../input/pat_appln_ipc.rds")

# Patent country by first inventor
pat_data %<>%
  left_join(
    appln_ipc %>%
      filter(ipc_position == "F") %>%
      select(appln_id, ipc_class_symbol), 
    by = 'appln_id') %>%
  rename(ipc = ipc_class_symbol) %>%
  mutate(ipc4 = ipc %>% str_sub(1,4))

############################################################################
# Person
############################################################################

person <- readRDS("../input/pat_pers.rds")
pers_appln <- readRDS("../input/pat_pers_appln.rds")

# Patent country by first inventor
pat_data %<>%
  left_join(
    pers_appln %>% 
      filter(invt_seq_nr == 1) %>% 
      left_join(person %>% select(person_id, person_ctry_code), by = 'person_id') %>% 
      select(appln_id, person_ctry_code) %>%
      distinct(appln_id, .keep_all = TRUE), 
    by = 'appln_id') %>%
  rename(country = person_ctry_code)


############################################################################
# DocDB IPC Citations
############################################################################

docdb_ipc <- readRDS("../input/docdb_ipc.rds")

el_docdb_cit_ipc <- readRDS("../input/el_docdb_cit_ipc.rds") %>%  
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
  summarise(weight = sum(i_n * j_n)) %>%
  ungroup() %>%
  filter(weight >= 10) %>%
  mutate(weight = weight %>% round(0)) 

############################################################################
# Blockmodeling IPC / Citations
############################################################################

library(blockmodeling)
library(Matrix)
library(tidygraph)
library(ggraph)

g <- el %>% graph.data.frame(directed = TRUE) %>% 
  simplify() %>%
  as_tbl_graph(directed = TRUE)

# Reduce grap
g <- g %N>%
  filter(centrality_degree(mode = 'all', normalized = TRUE, weights = weight) >= 10000)

bm <- g %>% 
  get.adjacency(sparse = FALSE, attr='weight') %>% 
  optRandomParC(k = 4, 
                rep = 10, 
                mingr = 5,
                #isSym = FALSE,
                #diag = FALSE,
                approaches = "val", 
                blocks = c("com", "reg", "rfn", "cfn", "avg",  "nul")
                            )

plot(bm)

# Add results
g <- g %N>%
  mutate(cluster_bm = bm$best$best1$clu)

# # Plot hierarchical
# layout_h <- layout_with_sugiyama(g, layers = V(g)$cluster_bm, hgap=10, vgap=10, weights = E(g)$weight, attributes = 'all')
# layout_h <- layout_h$extd_graph$layout
# plot(layout_h$extd_graph, vertex.label.cex=0.5, vertex.size=9)

layout_h$extd_graph %>%
  ggraph(layout = layout_h$extd_graph$layout) +
  geom_node_point(aes(size = centrality_degree(mode = "all"))) + 
  geom_edge_link(aes(width = weight), alpha = 0.25, col = 'skyblue')

# TRyout stuff
layout_h$extd_graph %>%
  ggraph(layout = layout_h$extd_graph$layout) +
  geom_edge_fan(aes(size = weight), 
                arrow = arrow(type = "closed", length = unit(2, "mm")),
                start_cap = circle(1, "mm"),
                end_cap = circle(1, "mm"),
                alpha = 0.5) + 
  geom_node_point(aes(size = centrality_degree(mode = "all"))) + 


geom_edge_fan(aes(size = weight,
                  color = year,
                  shape = year), 
              arrow = arrow(type = "closed", length = unit(2, "mm")),
              start_cap = circle(1, "mm"),
              end_cap = circle(1, "mm"),
              alpha = 0.5)
