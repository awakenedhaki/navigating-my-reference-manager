# ==============================================================================
# Topic Modelling: Latent Dirichlet Allocation
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(broom)
library(feather)
library(tidylda)
library(tidytext)
library(tidyverse)

# Changing ggplot2 Defaults ====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("point", list(size = 3, color = "salmon"))
theme_set(theme_minimal())

# Constants ====================================================================
DTM_PATH <- here("data", 
                 "active", 
                 "feature_engineering", 
                 "tf_idf_matrix_long_2_0-075.feather")

ETA <- 5
ALPHA <- 0.1
N_TOPICS <- 5k
ITERATIONS <- 500

# Loading Data =================================================================
dtm <- read_feather(DTM_PATH) %>%
  cast_sparse(id, term, n)

# LDA ==========================================================================
set.seed(123)
topics <- tidylda(dtm, k = N_TOPICS, 
                  iterations = ITERATIONS, 
                  alpha = ALPHA,
                  eta = ETA)

topic_token_probs <- topics %>%
  tidy(matrix = "beta")

topic_document_probs <- topics %>%
  tidy(matrix = "theta")

# Visualizations ===============================================================
topic_token_probs %>%
  slice_max(order_by = beta, n = 15, by = topic) %>%
  mutate(topic = factor(topic),
         token = reorder_within(token, beta, topic)) %>%
  ggplot(aes(x = token, y = beta, fill = topic)) +
    geom_col() +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~topic, scales = "free_y")

topic_document_probs %>%
  filter(document == "10") %>%
  ggplot(aes(x = topic, y = theta)) +
    geom_col()
  
read_feather(here("data", "active", "metadata.feather")) %>%
  filter(id == "10") %>%
  select(journal, title)
