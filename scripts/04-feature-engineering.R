# ==============================================================================
# Feature Engineering
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(feather)
library(tidytext)
library(tidyverse)

# Constants ====================================================================
DATA <- here("data")
CLEAN <- here(DATA, "cleaned")

# Loading Data Sets ============================================================
terms <- read_feather(here(CLEAN, "cleaned_tokens.feather"))

# Token Vectorization ==========================================================
counts <- terms %>%
  count(id, term) %>%
  add_count(term, wt = n, name = "total_n") %>%
  bind_tf_idf(term = term, document = id, n = n)

# Saving Vectorized Data =======================================================
write_feather(counts, here(DATA, "counts.feather"))
