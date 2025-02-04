
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matriz <img src="man/figures/logo.png" align="right" height="275"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/matriz)](https://CRAN.R-project.org/package=matriz)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://codecov.io/gh/jpmonteagudo28/matriz/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jpmonteagudo28/matriz)
![Static Badge](https://img.shields.io/badge/epi-research-%2384B4BF)
<!-- badges: end -->

The goal of `matriz` is to help you easily generate and manage
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

# Get matriz structure to guide in creating records
matriz_names()
#>                        class
#> year                 numeric
#> citation           character
#> keywords           character
#> profession         character
#> electronic           logical
#> purpose            character
#> study_design       character
#> outcome_var        character
#> predictor_var      character
#> sample               numeric
#> dropout_rate         numeric
#> setting            character
#> inclusion_criteria character
#> ethnicity          character
#> age                  numeric
#> sex                   factor
#> income                factor
#> education          character
#> measures           character
#> analysis           character
#> results            character
#> limitations        character
#> implications       character
#> ethical_concerns   character
#> biases             character
#> notes              character

# Start filling out individual record with article info
article <- data.frame(year = 2025,
                citation = " ",
                keywords = " ",
                profession = "underwater basket weaver",
                electronic = "YES",
                purpose = "To investigate the depth of the oceans and retireve weaving materials",
                study_design = "ethnography", 
                outcome_var = "perceived attitudes towards basket weaving",
                predictor_var = NA, 
                sample = "a small school of clown fish", 
                setting = "Italy",
                drop_rate = 0.13, 
                inclusion_criteria = "clow fish in Adriatic Sea", 
                ehtnicity = "oceanic", 
                age = "0 - 1 year",
                sex = "both",
                income = " ",
                education = "none",
                measures = "perceived attitudes",
                analysis = "qualitative", 
                results = "no significant differences",
                limitations = "small sample size", 
                implications = "clow fish don't like humans taking their homes for their own basket weaving endeavors",
                ethical_concerns = "no informed consent given to school of clown fish",
                biases = "clownfish always try to be funny. Lack of seriounness",
                notes = "more research needed")


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
`merge_matrix()`if you intend to combine matrices with dissimilar column
names. If you are joining two equal matrices with distinct articles, use
`add_record()` to add one record at a time or `add_batch_record()` to
add multiple article summaries at once.

> **Note**: If your article summaries are lists and their element
> classes differ from those in the init_matrix data frame, using
> add_batch_record() may coerce all elements to lists instead of
> preserving their original classes.

``` r
# Merge two literature matrices by a common column (e.g., "study_id")
additional_matrix <- lit_matrix
combined_matrix <- merge_matrix(lit_matrix, additional_matrix, by = "year", all = TRUE)
#> Removing duplicate columns...

# if you rather bind the two matrices together by rows, use 'add_batch_record()'
lit_matrix <- add_batch_record(lit_matrix, additional_matrix)
```

#### Searching for Records

Once your matrix is set up, you might need to search for specific
studies based on keywords, author names, or topics. Use
`search_record()` to filter the matrix for relevant entries.

``` r
 # Let's find all the electronic records we've summarized so far
 useless_search <- search_record(lit_matrix, where = electronic == "YES")
head(useless_search,1)
#>   year
#> 1 2024
#>                                                                          citation
#> 1 Smith J, Jones K. Example Title. Journal Name. 2024;1:1-10. doi:10.1234/example
#>   keywords               profession electronic
#> 1     <NA> underwater basket weaver        YES
#>                                                                 purpose
#> 1 To investigate the depth of the oceans and retireve weaving materials
#>   study_design                                outcome_var predictor_var
#> 1  ethnography perceived attitudes towards basket weaving          <NA>
#>                         sample dropout_rate setting        inclusion_criteria
#> 1 a small school of clown fish        Italy    0.13 clow fish in Adriatic Sea
#>   ethnicity        age  sex income education            measures    analysis
#> 1   oceanic 0 - 1 year both             none perceived attitudes qualitative
#>                      results       limitations
#> 1 no significant differences small sample size
#>                                                                            implications
#> 1 clow fish don't like humans taking their homes for their own basket weaving endeavors
#>                                    ethical_concerns
#> 1 no informed consent given to school of clown fish
#>                                                  biases                notes
#> 1 clownfish always try to be funny. Lack of seriounness more research needed

 # Change format to paper record by updating electronic to "NO"
 lit_matrix <- update_record(lit_matrix,electronic,where = electronic == "YES", set_to = "NO")
```

#### Exporting the Final Matrix

Once you’ve refined and categorized your literature review, you can
export the matrix for further use in Excel or other tools using
`export_matrix()`.

``` r
# Let's export our matrix
export_matrix(lit_matrix,"lit_matrix.txt",format = "txt")
#> Successfully imported matrix with 26 columns (26 required + 0 additional).
#> Data successfully exported to lit_matrix.txt
```

This structured workflow should make managing literature reviews more
efficient and streamlined.

## Project status

Actively developed, though the pace has slowed now that I’m busier with
other packages and my school work. I use it almost every day so it’s not
going anywhere. But I have no plans to substantially enlarge or extend
it before really testing it through daily use.

## Contributions

If you would like to contribute to this package, I’d love your help!
Please read the guidelines for submitting a pull request.

## Code of Conduct

Please note that the `matriz` project is released with a [Contributor
Code of Conduct](https://matriz.jpmonteagudo.com/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
