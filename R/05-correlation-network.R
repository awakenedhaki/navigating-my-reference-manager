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

# Constants ====================================================================
DTM_PATH <- here("data", "active", "feature_engineering")
MIN_THRESHOLD_COUNT_IN_DOCUMENT <- 5

# Helper Functions =============================================================
plot_graph <- function(graph, layout) {
  graph %>%
    ggraph(layout = layout) +
      geom_edge_link(aes(alpha = correlation)) +
      geom_node_point() +
      geom_node_text(aes(label = name), repel = TRUE, family = "Roboto") +
      theme_void() +
      theme(legend.position = "none")
}

# Loading Data Sets ============================================================
tf_idf_matrix <- read_feather(here(DTM_PATH, "tf_idf_matrix_long_1_0-05.feather"))

# Term Correlation =============================================================
# . Correlation (Adjacency) Matrix
correlation_matrix <-  tf_idf_matrix %>%
  filter(n > MIN_THRESHOLD_COUNT_IN_DOCUMENT) %>%
  pairwise_cor(term, id, sort = TRUE)

# . Correlation Graph
correlation_graph <- correlation_matrix %>%
  slice_max(order_by = correlation, n = 1000) %>%
  graph_from_data_frame()

graph_layout <- layout_with_fr(correlation_graph)
  
graph_components <- components(correlation_graph)
graph_memberships <- graph_components$membership
graph_component_sizes <- graph_components$csize

vertices <- V(correlation_graph)

# Visualizations ===============================================================
# . Query key words
correlation_matrix %>%
  filter(item1 %in% c("fap", "umap", "raf", "ovarian")) %>%
  group_by(item1) %>%
  top_n(10, wt = abs(correlation)) %>%
  ungroup() %>%
  mutate(item2 = reorder_within(item2, correlation, item1)) %>%
  ggplot(aes(x = item2, y = correlation, fill = item1)) +
    geom_col() +
    scale_x_reordered() +
    theme_minimal() +
    theme(legend.position = "none") +
    facet_wrap(~item1, scale = "free_y") +
    coord_flip()

# . Most vertices
longest_component <- which.max(graph_component_sizes)
correlation_graph %>%
  induced_subgraph(vertices[graph_memberships == longest_component]) %>%
  plot_graph(layout = graph_layout[graph_memberships == longest_component, ])

# . Components with more than two nodes
avg_node_count_by_membership <- ave(1:vcount(correlation_graph), 
                                    graph_memberships, 
                                    FUN = length)
correlation_graph %>%
  induced_subgraph(vertices[avg_node_count_by_membership > 2]) %>%
  plot_graph(layout = "fr")

