#'
#' This function imports a matrix (data frame) from various file formats (CSV, TSV, RDS, XLSX, XLS, TXT) and ensures it contains the required columns.
#' It also allows the user to control whether extra columns should be dropped or kept.
#'
#' @param path A character string specifying the path to the file to be imported.
#' @param format A character string specifying the file format. If not provided, the format is automatically detected based on the file extension.
#'   Supported formats: "csv", "tsv", "rds", "xlsx", "xls", "txt".
#' @param drop_extra A logical value indicating whether extra columns (not in the list of required columns) should be dropped. Default is `FALSE`.
#' @param extra_columns A character vector of column names that are allowed in addition to the required columns. By default, no extra columns are allowed.
#' @param silent A logical value indicating whether to suppress messages. Default is `FALSE`.
#' @param ... Additional arguments passed to the specific file-reading functions (e.g., `read.csv`, `read.delim`, `readRDS`, `readxl::read_xlsx`, `readxl::read_xls`, `read.table`).
#'   Refer to the documentation of the corresponding read function for the list of valid arguments.
#'
#' @return A data frame containing the imported matrix, with the required columns and any allowed extra columns.
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
#' Extra columns beyond the required ones are handled via the `extra_columns` argument. If the `drop_extra` argument is set to `TRUE`,
#' extra columns will be removed. If `drop_extra` is `FALSE`, extra columns will remain in the imported data, and a message will be shown.
#'
#' The `...` argument allows you to pass additional parameters directly to the read functions. For instance:
#' - For `read.csv`, `...` could include `header = TRUE`, `sep = ","`, or `stringsAsFactors = FALSE`.
#' - For `read.delim`, `...` could include `header = TRUE`, `sep = "\t"`, or `stringsAsFactors = FALSE`.
#' - For `readRDS`, `...` could include `refhook = NULL`.
#' - For `readxl::read_xlsx`, `...` could include `sheet = 1` or `col_names = TRUE`.
#' - For `readxl::read_xls`, `...` could include `sheet = 1` or `col_Names = TRUE`.
#' - For `read.table`, `...` could include `header = TRUE`, `sep = "\t"`, or `stringsAsFactors = FALSE`.
#'
#' @examples
#' \dontrun{
#' # Import a CSV file with extra columns
#' data <- import_matrix("data.csv", format = "csv", drop_extra = FALSE)
#'
#' # Import a TSV file and drop extra columns
#' data <- import_matrix("data.tsv", format = "tsv", drop_extra = TRUE)
#'
#' # Import an RDS file and allow additional columns
#' data <- import_matrix("data.rds", format = "rds", extra_columns = c("extra_column1", "extra_column2"))
#' }
#'
#' @importFrom dplyr select
#' @importFrom tools file_ext
#' @importFrom readxl read_xlsx read_xls
#' @export
import_matrix <- function(path,
                          format = NULL,
                          drop_extra = FALSE,
                          extra_columns = NULL,
                          silent = FALSE,
                          ...) {

  if (!file.exists(path)) {
    stop("The specified file does not exist.")
  }

  # Automatically detect format if not provided
  if (is.null(format)) {
    format <- tools::file_ext(path)
  }

  dots <- list(...)

  # Read the file based on format
  data <- switch(format,
                 csv = do.call(read.csv, c(list(path), dots)),
                 tsv = do.call(read.delim, c(list(path), dots)),
                 rds = readRDS(path),
                 xlsx = do.call(openxlsx::read.xlsx, c(list(path), dots)),
                 xls = do.call(gdata::read.xls, c(list(path), dots)),
                 txt = do.call(read.table, c(list(path), dots)),
                 stop("Unsupported file format. Please specify csv, tsv, rds, xlsx, xls, or txt.")
  )

  data <- as.data.frame(data)

  if (!silent && anyDuplicated(colnames(data))) {
    message("Removing duplicate columns...")
  }
  data <- rid_dups(data)

  # Validate and clean the imported data
  data <- validate_columns(data,
                           extra_columns = extra_columns,
                           drop_extra = drop_extra,
                           silent = silent
                           )

  return(data)
}

#' Validate and Clean Imported Data Matrix
#'
#' This function ensures that the imported data contains all required columns,
#' optionally removes unwanted extra columns, and provides informative messages
#' about the dataset's structure.
#'
#' @param data A data frame containing the imported matrix.
#' @param extra_columns A character vector of allowed additional columns beyond the required ones. Defaults to NULL.
#' @param drop_extra A logical value indicating whether to remove extra columns that are not in `extra_columns`. Defaults to FALSE.
#' @param silent A logical value indicating whether to suppress messages. Defaults to FALSE.
#'
#' @return A cleaned data frame with required columns intact and, optionally, extra columns removed.
#'
#' @details
#' The function checks whether all required columns are present in the data. If any required columns are
#' missing, it stops execution and informs the user.
#'
#' It also identifies extra columns beyond the required set and compares them against the allowed `extra_columns`.
#' If `drop_extra = TRUE`, it removes any extra columns not listed in `extra_columns`.
#' If `drop_extra = FALSE`, it retains the extra columns but issues a message unless `silent = TRUE`.
#'
#' @note The function assumes that column names in `data` are correctly formatted and case-sensitive.
#'
#' @examples
#' data <- init_matrix(nrow = 5,extra1)
#' validate_columns(data, extra_columns = "extra1", drop_extra = TRUE)
#'
#' @export
validate_columns <- function(data,
                             extra_columns = NULL,
                             drop_extra = FALSE,
                             silent = FALSE) {

  # Ensure the imported data matches expected structure
  required_columns <- c("year", "citation", "keywords", "profession", "electronic",
                        "purpose", "study_design", "outcome_var", "predictor_var", "sample",
                        "dropout_rate", "setting", "inclusion_criteria", "ethnicity", "age",
                        "sex", "income", "education", "measures", "analysis", "results",
                        "limitations", "implications", "ethical_concerns", "biases", "notes")

  # Check if all required columns exist
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("The imported data is missing the following required columns: ",
         paste(missing_columns, collapse = ", "))
  }

  # Check for extra columns
  existing_columns <- colnames(data)
  extra_columns_found <- setdiff(existing_columns, required_columns)

  # Allow additional columns via 'extra_columns' argument
  allowed_extra <- if (!is.null(extra_columns)) extra_columns else character(0)
  unwanted_extra <- setdiff(extra_columns_found, allowed_extra)

  # If drop_extra is FALSE, the unwanted extra columns are not dropped
  if (length(unwanted_extra) > 0 && drop_extra) {
    if (!silent) {
      message("Dropping the following extra columns: ", paste(unwanted_extra, collapse = ", "))
    }

    # Only drop unwanted extra columns if drop_extra is TRUE
    data <- data |>
      dplyr::select(-all_of(unwanted_extra))
  } else {
    # No action is taken if drop_extra is FALSE
    if (length(unwanted_extra) > 0 && !silent) {
      message("Extra columns present but not dropped. To drop them, set 'drop_extra' to TRUE.")
    }
  }

  # Inform the user about the import status
  if (!silent) {
    message("Successfully imported matrix with ", ncol(data), " columns (",
            length(required_columns), " required + ", ncol(data) - length(required_columns),
            " additional).")
  }

  return(data)
}
