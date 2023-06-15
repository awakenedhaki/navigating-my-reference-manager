# ==============================================================================
# Wrangling Reference Manager Data From Mendeley and Papers
# 
# . Reading, cleaning, and binding my Mendeley and Papers reference manager 
# . libraries. The libraries are split into metadata and abstract tibbles.
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(uuid)
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
  select(date_added = created_read_only, publication_year = year,
         title,  author,  journal,  abstract, doi, pmid, pmcid) %>%
  mutate(date_added = ymd_hms(date_added), reference_manager = "papers")

mendeley <- bib2df(here(RAW_DATA, "/mendeley.bib")) %>%
  clean_names() %>%
  select(publication_year = year, title, abstract, author, journal, doi, pmid) %>%
  mutate(id = row_number(), .before = publication_year) %>%
  mutate(author = str_flatten_comma(unlist(author)), .by = "id") %>%
  select(-id) %>%
  mutate(publication_year = as.numeric(publication_year), date_added = date(NA),
         pmcid = NA, reference_manager = "mendeley")

papers
mendeley

# Bindings Both Data Sets ======================================================
articles <- rbind(papers, mendeley) %>%
  mutate(missing_journal = is.na(journal),
         missing_abstract = is.na(abstract)) %>%
  mutate(across(.cols = c(pmcid, pmid, doi), 
                .fns = ~duplicated(.x, fromLast = TRUE, incomparables = NA),
                .names = "duplicated_{.col}")) %>%
  filter(!missing_abstract, !missing_journal,
         !(duplicated_pmcid | duplicated_pmid | duplicated_doi)) %>%
  select(-c(missing_abstract, 
            duplicated_pmcid, duplicated_pmid, duplicated_doi)) %>%
  mutate(id = UUIDgenerate(n = nrow(.)), .before = date_added)

# Seperating Data Sets =========================================================
# . Metadata: Descriptive information about an article
# . Abstracts: The focus of the analysis
# . Data sets are related by their `id` column
metadata <- articles %>% select(-abstract)
abstracts <- articles %>% select(id, abstract)

# Saving =======================================================================
write_feather(metadata, here(ACTIVE_DATA, "metadata.feather"))
write_feather(abstracts, here(ACTIVE_DATA, "abstracts.feather"))
