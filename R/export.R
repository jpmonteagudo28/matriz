#' Export a Data Matrix to Various File Formats
#'
#' This function exports a data frame to a specified file format, including CSV, TSV, RDS, XLSX, and TXT.
#' If the format is not provided, it is inferred from the file extension.
#'
#' @param .data A data frame or tibble to be exported.
#' @param file A character string specifying the file name and path.
#' @param format A character string specifying the file format. If `NULL`, the format is inferred from the file extension.
#'              Supported formats: `"csv"`, `"tsv"`, `"rds"`, `"xlsx"`, `"txt"`.
#' @param drop_extra Logical. If `TRUE`, removes columns not listed in `extra_columns` before exporting. Default is `FALSE`.
#' @param extra_columns A character vector specifying additional columns to retain if `drop_extra = TRUE`. Default is `NULL`.
#' @param silent Logical. If `TRUE`, suppresses messages. Default is `FALSE`.
#' @param ... Additional arguments passed to the underlying export functions (`write.csv`, `writexl::write_xlsx`, etc.).
#'
#' @return Exports the data to a file and returns `NULL` invisibly.
#' @export
#' @importFrom writexl write_xlsx
#' @importFrom readxl write_tsv
#'
#' @examples
#' \donttest{
#' # Create a sample data frame
#' data <- data.frame(year = 2025,
#'' citation = " ",
#' keywords = " ",
#' profession = "underwater basket weaver",
#' electronic = "YES",
#' purpose = "To investigate the depth of the oceans and retireve weaving materials",
#' study_design = "ethnography",
#' outcome_var = "perceived attitudes towards basket weaving",
#' predictor_var = NA,
#' sample = "a small school of clown fish",
#' setting = "Italy",
#' drop_rate = 0.13,
#' inclusion_criteria = "clow fish in Adriatic Sea",
#' ehtnicity = "oceanic",
#' age = "0 - 1 year",
#' sex = "both",
#' income = " ",
#' education = "none",
#' measures = "perceived attitudes",
#' analysis = "qualitative",
#' results = "no significant differences",
#' limitations = "small sample size",
#' implications = "clow fish don't like humans taking their homes for their own basket weaving endeavors",
#' ethical_concerns = "no informed consent given to school of clown fish",
#' biases = "clownfish always try to be funny. Lack of seriounness",
#' notes = "more research needed")
#'
#' # Export as CSV
#' export_matrix(data, "output.csv")
#'
#' # Export as Excel file
#' export_matrix(data, "output.xlsx")
#'
#' # Export as RDS file
#' export_matrix(data, "output.rds")
#'
#' # Export silently
#' export_matrix(data, "output.csv", silent = TRUE)
#' }
export_matrix <- function(.data,
                          file,
                          format = NULL,
                          drop_extra = FALSE,
                          extra_columns = NULL,
                          silent = FALSE,
                          ...){

  if(is.null(file)){
    stop("Please specify a file.")
  }

  # Automatically detect format if not provided
  if (is.null(format)) {
    format <- tools::file_ext(file)
  }

  dots <- list(...)

  if (!silent && anyDuplicated(colnames(data))) {
    message("Removing duplicate columns...")
  }
  .data <- rid_dups(.data)

  # Validate and clean the imported data
  .data <- validate_columns(.data,
                           extra_columns = extra_columns,
                           drop_extra = drop_extra,
                           silent = silent
                          )

   switch(format,
            csv = do.call(utils::write.csv, c(list(.data, file), dots)),
            tsv = do.call(readr::write_tsv, c(list(.data, file), dots)),
            rds = saveRDS(.data, file),
            xlsx = do.call(writexl::write_xlsx, c(list(.data), dots, list(path = file))),
            txt = do.call(utils::write.table, c(list(.data, file), dots)),
            stop("Unsupported file format. Please specify csv, tsv, rds, xlsx, xls, or txt.")
         )

  if (!silent) {
    message("Data successfully exported to ", file)
  }

}
