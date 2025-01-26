#' Initialize a Literature Review Matrix
#'
#' @description
#' Creates a standardized data frame for systematic literature review with predefined columns,
#' allowing the addition of custom columns if needed.
#'
#' @param ... Optional. Additional column names (as character strings) to be appended to the matrix.
#'
#' @return A data frame with predefined columns for literature review analysis.
#'
#' @details
#' The matrix includes the following predefined columns:
#' - `year`: Numeric. Year of publication.
#' - `citation`: Character. Citation or reference details.
#' - `keywords`: Character. Keywords or tags for the study.
#' - `profession`: Character. Profession of the study participants or target audience.
#' - `electronic`: Logical. Indicates whether the study is available electronically.
#' - `purpose`: Character. Purpose or objective of the study.
#' - `study_design`: Character. Study design or methodology.
#' - `outcome_var`: Character. Outcome variables measured in the study.
#' - `predictor_var`: Character. Predictor variables considered in the study.
#' - `sample`: Numeric. Sample size.
#' - `dropout_rate`: Numeric. Dropout or attrition rate.
#' - `setting`: Character. Study setting (e.g., clinical, educational).
#' - `inclusion_criteria`: Character. Inclusion criteria for participants.
#' - `ethnicity`: Character. Ethnic background of participants.
#' - `age`: Numeric. Age of participants.
#' - `sex`: Factor. Sex of participants.
#' - `income`: Factor. Income level of participants.
#' - `education`: Character. Educational background of participants.
#' - `measures`: Character. Measures or instruments used for data collection.
#' - `analysis`: Character. Analytical methods used.
#' - `results`: Character. Summary of results or findings.
#' - `limitations`: Character. Limitations of the study.
#' - `implications`: Character. Implications or recommendations from the study.
#' - `ethical_concerns`: Character. Ethical concerns addressed in the study.
#' - `biases`: Character. Potential biases in the study.
#' - `notes`: Character. Additional notes or observations.
#'
#' Custom columns can also be added by passing their names via the `...` argument.
#'
#' @examples
#' # Create a basic literature review matrix
#' lit_matrix <- init_matrix()
#'
#' # Add custom columns
#' lit_matrix <- init_matrix("research_funding", "publication_type")
#'
#' @export


init_matrix <- function(...){

  lit_matrix <- data.frame(
    year = numeric(1L),
    citation = character(1L),
    keywords = character(1L),
    profession = character(1L),
    electronic = logical(1L),
    purpose = character(1L),
    study_design = character(1L),
    outcome_var = character(1L),
    predictor_var = character(1L),
    sample = numeric(1L),
    dropout_rate = numeric(1L),
    setting = character(1L),
    inclusion_criteria = character(1L),
    ethnicity = character(1L),
    age = numeric(1L),
    sex = factor(1L),
    income = factor(1L),
    education = character(1L),
    measures = character(1L),
    analysis = character(1L),
    results = character(1L),
    limitations = character(1L),
    implications = character(1L),
    ethical_concerns = character(1L),
    biases = character(1L),
    notes = character(1L)
  )

  if (!missing(...)) {
    extra_args <- deparse_dots(...)
    # Example: Use extra_args to perform additional tasks if needed
    message("Extra arguments passed: ", paste(extra_args, collapse = ", "))

    for (col_name in extra_args) {
      lit_matrix <- append_column(lit_matrix, new_col = NA, .after = NULL)
      names(lit_matrix)[ncol(lit_matrix)] <- col_name
    }
  }
  return(lit_matrix)
}
