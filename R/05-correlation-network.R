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
theme_set(theme_void())

# Constants ====================================================================
ACTIVE_DATA <- here("data", "active")

# Loading Data Sets ============================================================
tf_idf_matrix <- read_feather(here(ACTIVE_DATA, "tf_idf_matrix_long.feather"))

# Correlation Graph ============================================================
correlation_matrix <-  tf_idf_matrix %>%
  filter(tf > 0.05) %>%
  pairwise_cor(term, id, sort = TRUE)

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

correlation_matrix %>%
  filter(correlation > 0) %>%
  arrange(desc(correlation)) %>%
  head(750) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
    geom_edge_link(aes(alpha = correlation)) +
    geom_node_point() +
    geom_node_text(aes(label = name), repel = TRUE, family = "Roboto")
