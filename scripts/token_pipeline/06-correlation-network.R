# ==============================================================================
# Correlation Graph
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(widyr)
library(broom)
library(ggraph)
library(igraph)
library(feather)
library(showtext)
library(tidytext)
library(tidyverse)

# Changing ggplot2 Defaults ====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("point", list(size = 3, color = "salmon"))
theme_set(theme_minimal())
theme_update(legend.position = "none", 
             panel.border = element_rect(color = "black", fill = NA))

# Constants ====================================================================
DATA <- here("data")
MIN_COUNT_ACROSS_CORPUS <- 15
MIN_VERTICES <- 5

# Helper Functions =============================================================
plot_graph <- function(graph, layout) {
  graph %>%
    ggraph(layout = layout) +
      geom_edge_link(aes(alpha = correlation)) +
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
correlation_matrix <-  counts %>%
  filter(total_n >= MIN_COUNT_ACROSS_CORPUS) %>%
  pairwise_cor(term, id, sort = TRUE)

# . Correlation Graph
correlation_graph <- correlation_matrix %>%
  filter(correlation > 0) %>%
  slice_max(order_by = correlation, n = 1500) %>%
  graph_from_data_frame()

correlation_subgraphs <- decompose(correlation_graph, min.vertices = MIN_VERTICES) %>%
  lapply(as_long_data_frame) %>%
  bind_rows() %>%
  select(from_name, to_name, correlation) %>%
  graph_from_data_frame()

# Visualizations ===============================================================
# . Components with more than two nodes
correlation_subgraphs %>%
  plot_graph(layout = "fr")
