# Navigating My Reference Manager

## Introduction

## Repository Structure

```         
.
├── README.md
├── navigating-my-reference-manager.Rproj
├── R
│   ├── 01-wrangling.R
│   ├── 02-normalization.R
│   ├── 03-cleaning.R
│   ├── 04-feature-engineering.R
│   └── 05-correlation-network.R
├── data
│   ├── active
│   │   ├── abstracts.feather
│   │   ├── clean
│   │   │   ├── cleaned_abstracts_lower-50_upper-825.feather
│   │   │   └── cleaned_abstracts_lower-75_upper-825.feather
│   │   ├── feature_engineering
│   │   │   ├── tf_idf_matrix_long_5_0-05.feather
│   │   │   ├── tf_idf_matrix_long_5_0-075.feather
│   │   │   └── tf_idf_matrix_long_5_0-1.feather
│   │   ├── metadata.feather
│   │   └── normalized_tokens.feather
│   └── raw
│       ├── mendeley.bib
│       └── papers.csv
└── resources
    ├── collapsing_tokens.csv
    ├── gene_name_corrections.csv
    ├── plural_removal.csv
    └── remove_words.csv
```
