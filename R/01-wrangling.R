# ==============================================================================
# Wrangling Reference Manager Data From Mendeley and Papers
# 
# . Reading, cleaning, and binding my Mendeley and Papers reference manager 
# . libraries. The libraries are split into metadata and abstract tibbles.
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(bib2df)
library(feather)
library(janitor)
library(tidyverse)

# Setting Constants ============================================================
RAW_DATA <- here("data", "raw")
ACTIVE_DATA <- here("data", "active")

# Loading Data Sets ============================================================
papers <- read_csv(here(RAW_DATA, "papers.csv")) %>%
  clean_names() %>%
  filter(!is.na(doi) | !is.na(pmid) | !is.na(pmcid),
         !is.na(journal),
         !is.na(abstract)) %>%
  select(id = item_id_read_only, 
         date_added = created_read_only, 
         publication_year = year,
         title,  author,  journal,  abstract, doi, pmid) %>%
  mutate(date_added = ymd_hms(date_added),  reference_manager = "papers")

mendeley <- bib2df(here(RAW_DATA, "/mendeley.bib")) %>%
  clean_names() %>%
  filter(!is.na(doi) | !is.na(pmid),
         !is.na(journal),
         !is.na(abstract)) %>%
  select(publication_year = year, title, abstract, author, journal, doi, pmid) %>%
  mutate(id = row_number(), .before = publication_year) %>%
  mutate(author = str_flatten_comma(unlist(author)), .by = "id") %>%
  mutate(publication_year = as.numeric(publication_year), 
         date_added = date(NA),
         reference_manager = "mendeley")

# Bindings Both Data Sets ======================================================
articles <- rbind(papers, mendeley) %>%
  # From duplicates, select the row from Papers
  filter(!duplicated(doi, fromLast = TRUE)) %>%
  filter(!duplicated(pmid), fromLast = TRUE)

# Seperating Data Sets =========================================================
# . Metadata: Descriptive information about an article
# . Abstracts: The focus of the analysis
# . Data sets are related by their `id` column
metadata <- articles %>% select(-abstract)
abstracts <- articles %>% select(id, abstract)

# Saving =======================================================================
write_feather(metadata, here(ACTIVE_DATA, "metadata.feather"))
write_feather(abstracts, here(ACTIVE_DATA, "abstracts.feather"))