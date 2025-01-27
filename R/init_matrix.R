#' Initialize a Literature Review Matrix
#'
#' @description
#' Creates a standardized data frame for systematic literature review with predefined columns,
#' allowing the addition of custom columns if needed.
#'
#' @param ... Optional. Additional column names (as character strings) to be appended to the matrix.
#' @param nrow Numeric. Number of rows to initialize the matrix with. Default is 1.
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
#' lit_matrix <- init_matrix("research_funding", "publication_type",nrow = 5)
#'
#' @export


init_matrix <- function(...,nrow = 1){

  stopifnot(is.numeric(nrow), !nrow < 0)

  lit_matrix <- data.frame(
    year = numeric(length = nrow),
    citation = character(length = nrow),
    keywords = character(length = nrow),
    profession = character(length = nrow),
    electronic = logical(length = nrow),
    purpose = character(length = nrow),
    study_design = character(length = nrow),
    outcome_var = character(length = nrow),
    predictor_var = character(length = nrow),
    sample = numeric(length = nrow),
    dropout_rate = numeric(length = nrow),
    setting = character(length = nrow),
    inclusion_criteria = character(length = nrow),
    ethnicity = character(length = nrow),
    age = numeric(length = nrow),
    sex = factor(character(length = nrow)),
    income = factor(character(length = nrow)),
    education = character(length = nrow),
    measures = character(length = nrow),
    analysis = character(length = nrow),
    results = character(length = nrow),
    limitations = character(length = nrow),
    implications = character(length = nrow),
    ethical_concerns = character(length = nrow),
    biases = character(length = nrow),
    notes = character(length = nrow)
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
