############################################################################
# Preamble
############################################################################
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
  summarise(weight = sum(i_n * j_n)) %>%
  ungroup() %>%
  filter(weight >= 10) %>%
  mutate(weight = weight %>% round(0)) 

############################################################################
# Blockmodeling IPC / Citations
############################################################################

library(blockmodeling)
library(Matrix)
library(igraph)
library(tidygraph)
library(ggraph)

g <- el %>% graph.data.frame(directed = TRUE) 
g %<>% delete_vertices(strength(g, mode = "all") < 100)
g %<>% simplify()

mat <- g %>% get.adjacency(sparse = FALSE, attr='weight')

bm <- mat %>% optRandomParC(k = 4, 
                            rep = 10, 
                            mingr = 5,
                            #isSym = FALSE,
                            #diag = FALSE,
                            approaches = "val", 
                            blocks = c("com", "reg", "rfn", "cfn", "avg",  "nul")
                            )

plot(bm)

V(g)$opt.blocks <- bm$best$best1$clu

# Plot
plot.igraph(g, vertex.color=V(g)$opt.blocks) # plot in igraph

# Plot hierarchical
layout_h <- layout_with_sugiyama(g, layers = V(g)$opt.blocks, hgap=10, vgap=10, weights = E(g)$weight, attributes = 'all') 

plot(layout_h$extd_graph, vertex.label.cex=0.5, vertex.size=9)

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
