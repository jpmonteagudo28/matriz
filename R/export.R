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

  data <- switch(format,
                 csv = do.call(write.csv, c(list(data), file,dots)),
                 tsv = do.call(readxl::write_tsv, c(list(data),file, dots)),
                 rds = saveRDS(data),
                 xlsx = do.call(readxl::write_excel_csv, c(list(data),file, dots)),
                 xls = do.call(readxl::read_xls, c(list(data),file, dots)),
                 txt = do.call(write.table, c(list(data),file, dots)),
                 stop("Unsupported file format. Please specify csv, tsv, rds, xlsx, xls, or txt.")
  )

}
