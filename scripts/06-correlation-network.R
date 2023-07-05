# ==============================================================================
# Correlation Graph
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(widyr)
library(ggraph)
library(igraph)
library(feather)
library(tidytext)
library(tidyverse)

# Changing ggplot2 Defaults ====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("point", list(size = 3, color = "salmon"))
theme_set(theme_minimal())
theme_update(legend.position = "none", 
             panel.border = element_rect(color = "black", fill = NA))

# Constants ====================================================================
DATA <- here("data", "features")
MIN_COUNT_ACROSS_CORPUS <- 15
MIN_VERTICES <- 5

# Helper Functions =============================================================
plot_graph <- function(graph, layout) {
  graph %>%
    ggraph(layout = layout) +
      geom_edge_link(aes(alpha = similarity)) +
      geom_node_point(color = "black", size = 4) +
      geom_node_point() +
      geom_node_text(aes(label = name), repel = TRUE, family = "Roboto") +
      theme_void() +
      theme(legend.position = "none")
}

# Loading Data =================================================================
counts <- read_feather(here(DATA, "counts.feather")) %>%
  add_count(term, wt = n, name = "total_n")

# Term Correlation =============================================================
# . Correlation (Adjacency) Matrix
similarity_matrix <-  counts %>%
  filter(total_n >= MIN_COUNT_ACROSS_CORPUS) %>%
  pairwise_similarity(item = term, feature = id, value = tf)

similarity_graph <- similarity_matrix %>%
  slice_max(order_by = similarity, n = 1500) %>%
  graph_from_data_frame() %>%
  decompose(min.vertices = MIN_VERTICES) %>%
  lapply(as_long_data_frame) %>%
  bind_rows() %>%
  select(from_name, to_name, similarity) %>%
  graph_from_data_frame() 

# Visualizations ===============================================================
similarity_graph %>%
  plot_graph(layout = "fr")
