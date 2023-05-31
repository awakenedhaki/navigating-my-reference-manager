# ==============================================================================
# Latent Semantic Analysis
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(broom)
library(scales)
library(feather)
library(viridis)
library(tidyverse)

# Setting ggplot2 Defaults =====================================================
update_geom_defaults("point", list(size = 3, color = "salmon"))
update_geom_defaults("bar", list(fill = "salmon"))
theme_set(theme_minimal())

# Constants ====================================================================
ACTIVE_DATA <- here("data", "active")
DTM_PATH <- here(ACTIVE_DATA, 
                 "feature_engineering", 
                 "tf_idf_matrix_long_5_0-05.feather")

# Loading Data =================================================================
metadata <- read_feather(here(ACTIVE_DATA, "metadata.feather"))

dtm <- read_feather(DTM_PATH) %>%
  cast_sparse(id, term, tf_idf)

# Latent Semantic Analysis =====================================================
set.seed(123)
decomposition <- svd(dtm)

token_projection <- decomposition %>%
  tidy(matrix = "v")

document_projection <- decomposition %>%
  tidy(matrix = "u")

eigenvalues <- decomposition %>%
  tidy(matrix = "d")

# Visualization ================================================================
document_projection %>%
  filter(PC <= 3) %>%
  mutate(PC = str_c("PC", PC)) %>%
  pivot_wider(names_from = PC, values_from = value) %>%
  ggplot(aes(x = PC1, y = PC2)) +
    geom_point()