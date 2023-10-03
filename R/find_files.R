#' @export
#' @inheritParams sifr
find_files <- function(
    pattern,
    dir = ".",
    markers = interactive() && requireNamespace("rstudioapi", quietly = TRUE),
    fixed = FALSE,
    case_sensitive = TRUE,
    path_pattern = getOption("sifr.path_pattern"),
    path_case_sensitive = FALSE,
    recursive = TRUE,
    encoding = "unknown"
){
  stopifnot(
    is_scalar_character(pattern),
    is_scalar_bool(fixed),
    is_scalar_bool(case_sensitive),
    is_scalar_character(path_pattern),
    is_scalar_bool(path_case_sensitive),
    is_scalar_bool(recursive)
  )

  assert_dirs_exist(dir)


  files <- list.files(
    dir,
    full.names = TRUE,
    pattern = path_pattern,
    ignore.case = !path_case_sensitive,
    recursive = recursive,
    all.files = TRUE
  )

  res <- lapply(files, function(f) grep_paths(
    f,
    pattern = pattern,
    case_sensitive = case_sensitive,
    fixed = fixed,
    highlight = TRUE,
    encoding = encoding
  ))

  if (length(res)){
      res <- data.table::rbindlist(res)
  } else {
    res <- data.table(
      path = character(),
      ln = integer(),
      pos = list(),
      contents = character()
    )
  }

  res <- as_find_files_result(res, pattern)

  if (markers && nrow(res) > 0){
    source_markers(res)
    invisible(res)
  } else {
    res
  }
}
