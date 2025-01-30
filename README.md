
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matriz <img src="man/figures/logo.png" align="right" height="275"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of ‘matriz’ is to help you easily generate and manage
structured literature review matrices in R. The package aims to
streamline your research synthesis, track key study details, and
organize citations efficiently.

## Installation

You can install the development version of matriz like so:

``` r
devtools::install_github("jpmonteagudo28/matriz)
```

or you can download it from CRAN:

``` r
install.packages("matriz")
```

## Intro

This document walks through the main steps of using `matriz`, from
setting up your matrix to searching and refining it.

#### Importing Literature Matrices

The first step is to bring your literature data into R. If you have an
existing matrix in CSV or another format, use the `import_matrix()`
function. Otherwise, create an empty matrix using `init_matrix()`:

``` r
library(matriz)
#> 
#> Attaching package: 'matriz'
#> The following object is masked from 'package:base':
#> 
#>     truncate

lit_matrix <- init_matrix()

#Print empty matrix
str(lit_matrix)
#> 'data.frame':    0 obs. of  26 variables:
#>  $ year              : num 
#>  $ citation          : chr 
#>  $ keywords          : chr 
#>  $ profession        : chr 
#>  $ electronic        : logi 
#>  $ purpose           : chr 
#>  $ study_design      : chr 
#>  $ outcome_var       : chr 
#>  $ predictor_var     : chr 
#>  $ sample            : num 
#>  $ dropout_rate      : num 
#>  $ setting           : chr 
#>  $ inclusion_criteria: chr 
#>  $ ethnicity         : chr 
#>  $ age               : num 
#>  $ sex               : Factor w/ 0 levels: 
#>  $ income            : Factor w/ 0 levels: 
#>  $ education         : chr 
#>  $ measures          : chr 
#>  $ analysis          : chr 
#>  $ results           : chr 
#>  $ limitations       : chr 
#>  $ implications      : chr 
#>  $ ethical_concerns  : chr 
#>  $ biases            : chr 
#>  $ notes             : chr

# Start filling out individual record with article info
article <- list(year = 2025,citation = character(0),keywords = character(0),
                profession = "underwater basket weaver",electronic = "YES",
                purpose = "To investigate the depth of the oceans and retireve weaving materials",
                study_design = "ethnography", outcome_var = "perceived attitudes towards basket weaving",
                predictor_var = NA, sample = "a small school of clown fish", setting = "Italy",
                drop_rate = 0.13, inclusion_criteria = "clow fish in Adriatic Sea", 
                ehtnicity = "oceanic", age = "0 - 1 year",sex = "both",income = NULL,
                education = "none",measures = "perceived attitudes",
                analysis = "qualitative", results = "no significant differences",
                limitations = "small sample size", implications = "clow fish don't like humans taking their homes for their own basket weaving endeavors",ethical_concerns = "no informed consent given to school of clown fish",biases = "clownfish always try to be funny. Lack of seriounness",notes = "more research needed")


# Process and add the citation to the current record
bibtex <- system.file("examples","example.bib",package = "matriz")

cited_article <- process_citation(article,bibtex)


# Add the record to the literature matrix
lit_matrix <- add_record(lit_matrix, cited_article, .before = 1)

# Update record if mistake was made
lit_matrix <- update_record(lit_matrix, notes, where = year == 2025, set_to = "actually, the clow fish don't want us to come back.")
```

#### Merging Matrices

If you have multiple literature matrices and need to combine them, use
`merge_matrix()`. This function ensures that duplicate columns are
removed before merging.

``` r
# Merge two literature matrices by a common column (e.g., "study_id")
additional_matrix <- lit_matrix
combined_matrix <- merge_matrix(lit_matrix, additional_matrix, by = "year", all = TRUE)
#> Removing duplicate columns...

# if you rather bind the two matrices together by rows, use 'add_batch_record()'
```

#### Searching for Records

Once your matrix is set up, you might need to search for specific
studies based on keywords, author names, or topics. Use
`search_record()` to filter the matrix for relevant entries.

#### Exporting the Final Matrix

Once you’ve refined and categorized your literature review, you can
export the matrix for further use in Excel or other tools using
`export_matrix()`.

This structured workflow should make managing literature reviews more
efficient and streamlined.
