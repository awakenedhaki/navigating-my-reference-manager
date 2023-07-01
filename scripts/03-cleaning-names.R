# ==============================================================================
# Cleaning Author Names
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(glue)
library(feather)
library(tidyverse)
library(humaniformat)

# Constants ====================================================================
DATA <- here("data")

# Loading Data =================================================================
metadata <- read_feather(here(DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, author, title, reference_manager)

# Cleaning Authors Names =======================================================
authors <- metadata %>%
  select(id, author) %>%
  separate_longer_delim(cols = author, delim = ",") %>%
  mutate(first_name = first_name(author),
         middle_name = middle_name(author),
         last_name = last_name(author)) %>%
  mutate_at(.vars = vars(contains("name")),
            .funs = \(x) x <- str_remove_all(x, "\\.")) %>%
  mutate(author = str_remove(author, "^ "))

# Shift Names ==================================================================
shifted_names <- which(nchar(authors$last_name) == 1)

first_name <- authors[shifted_names, "middle_name"]
middle_name <- authors[shifted_names, "last_name"]
last_name <- authors[shifted_names, "first_name"]

authors[shifted_names, "first_name"] <- first_name
authors[shifted_names, "middle_name"] <- middle_name
authors[shifted_names, "last_name"] <- last_name

# Middle to First ==============================================================
swapped_given_names <- which(nchar(authors$first_name) == 1 & 
                               nchar(authors$middle_name) > 1)

first_name <- authors[swapped_given_names, "middle_name"]
middle_name <- authors[swapped_given_names, "first_name"]

authors[swapped_given_names, "first_name"] <- first_name
authors[swapped_given_names, "middle_name"] <- middle_name

# Fill NA First Name with Middle Name ==========================================
missing_first_names <- which(is.na(authors$first_name) & !is.na(authors$middle_name))

authors[missing_first_names, "first_name"] <- authors[missing_first_names, "middle_name"]
authors[missing_first_names, "middle_name"] <- NA

# Removing Initials from First Names ===========================================
authors <- authors %>%
  mutate(first_name = ifelse(str_detect(first_name, "[A-Z ]\\w{2,}"),
                             str_extract(first_name, "\\w{2,}"),
                             first_name) )

# Manual Correction ============================================================
set_name <- function(tbl, author_name, first_name, last_name) {
  author_idx <- which(tbl$author == author_name)
  tbl[author_idx, "first_name"] <- first_name
  tbl[author_idx, "last_name"] <- last_name
  
  return(tbl)
}

# . . Author Specific
authors <- set_name(authors, "Eric Z. Ma",        "Eric",    "Ma")
authors <- set_name(authors, "John S. Coon V",    "John",    "Coon")
authors <- set_name(authors, "M.S Waterman",      "Michael", "Waterman")
authors <- set_name(authors, "DR Rowley",         "David",   "Rowley")
authors <- set_name(authors, "Stephen S.T. Wong", "Stephen", "Wong")

# . . Document Specific
missing_last_name <- which(authors$id == "fbeece21-eb59-412a-b803-6595e25a2fc7")

authors[missing_last_name, ] <- authors[missing_last_name, ] %>%
  fill(first_name, .direction = "down")

authors <- authors %>%
  filter(!(id == "fbeece21-eb59-412a-b803-6595e25a2fc7" & is.na(last_name)))

shifted_names <- which(authors$id == "fbeece21-eb59-412a-b803-6595e25a2fc7")
first_name <- authors[shifted_names, ]$last_name
last_name <- authors[shifted_names, ]$first_name

authors[shifted_names, "first_name"] <- first_name
authors[shifted_names, "last_name"] <- last_name

# Imputing First Name from Initials and Neighbours =============================
last_names <- authors %>%
  filter(str_detect(first_name, "^[A-Z]$")) %>%
  distinct() %>%
  pull(last_name)

select_last_names <- which(authors$last_name %in% last_names)

# authors[select_last_names, "first_name"] <- 
authors[select_last_names, "first_name"] <- authors %>%
  filter(last_name %in% last_names) %>%
  mutate(first_initial = str_sub(first_name, start = 1, end = 1)) %>%
  group_by(last_name, first_initial) %>%
  mutate(imputed_first_name = ifelse(nchar(first_name) == 1,
                                     NA,
                                     first_name)) %>%
  fill(imputed_first_name, .direction = "downup") %>%
  ungroup() %>%
  mutate(first_name = ifelse(is.na(imputed_first_name),
                             first_name,
                             imputed_first_name)) %>%
  pull(first_name)

# Complete Author Names ========================================================
authors %>%
  mutate(name = glue("{first_name} {last_name}")) %>%
  select(id, name) %>%
  group_by(id) %>%
  summarize(names = paste(name, collapse = ",")) %>%
  write_feather(here(DATA, "cleaned", "author_names.feather"))
