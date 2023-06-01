# ==============================================================================
# Topic Modelling: Latent Dirichlet Allocation
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(broom)
library(scales)
library(feather)
library(tidylda)
library(tidytext)
library(tidyverse)

# Changing ggplot2 Defaults ====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("point", list(size = 3, color = "salmon"))
theme_set(theme_minimal())
theme_update(legend.position = "none", 
             panel.border = element_rect(color = "black", fill = NA))

# Constants ====================================================================
ACTIVE_DATA <- here("data", "active")
DTM_PATH <- here(ACTIVE_DATA, 
                 "feature_engineering", 
                 "tf_idf_matrix_long_3_2_0-05.feather")

ETA <- 0.75
ALPHA <- 0.01
N_TOPICS <- 9
ITERATIONS <- 500

# Loading Data =================================================================
metadata <- read_feather(here(ACTIVE_DATA, "metadata.feather"))

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
# . Top Words per Topic
topic_labels <- setNames(str_c("Topic: ", 1:N_TOPICS), 1:N_TOPICS)
topic_token_probs %>%
  slice_max(order_by = beta, n = 15, by = topic) %>%
  mutate(topic = factor(topic),
         token = reorder_within(token, beta, topic)) %>%
  ggplot(aes(x = token, y = beta, fill = topic)) +
    geom_col() +
    scale_x_reordered() +
    labs(x = "Term", y = "Beta [P(Topic | Term)]") +
    coord_flip() +
    facet_wrap(~topic, scales = "free_y", labeller = as_labeller(topic_labels))

# . Topic Timeline Heatmap
topic_document_probs %>%
  left_join(metadata %>% select(id, publication_year), 
            by = c("document" = "id")) %>%
  slice_max(order_by = theta, n = 1, by = document, with_ties = FALSE) %>%
  count(year = factor(publication_year), topic = factor(topic), sort = TRUE) %>%
  complete(year, topic, fill = list(n = 0)) %>%
  mutate(prop = n / sum(n), .by = year) %>% {
    ggplot(data = ., aes(x = topic, y = year, fill = prop)) +
      geom_tile(color = "black") +
      geom_tile(data = subset(., n == 0), fill = "black") +
      geom_text(data = subset(., prop >= 0.2), 
                aes(label = round(prop * 100)), 
                size = 3,
                color = "black") +
      scale_fill_gradient2(low = "#381D2A", mid = "#AABD8C", high = "#E9E3B4",
                           midpoint = 0.40, label = label_percent()) +
      coord_equal() +
      labs(x = "Topic", y = "Publication Year", fill = "P(Document | Topic)") +
      theme(panel.grid = element_blank(), legend.position = "right")
  }
