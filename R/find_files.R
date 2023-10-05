#' Find files
#'
#' Find files based on a regex pattern matched against the file path. Mainly
#' useful for interactive use.
#'
#' @param pattern `character scalar`. A pattern for which to look in filenames
#'
#' @export
#' @seealso [open_files()]
#' @inheritParams sif
find_files <- function(
    pattern,
    dir = ".",
    markers = interactive() && getOption("sifr.use_markers") && requireNamespace("rstudioapi", quietly = TRUE),
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

  assign(".last_find_files", res, sifr_last)

  if (markers && nrow(res) > 0){
    as_source_markers(res)
    invisible(res)
  } else {
    res
  }
}




#' Open files
#'
#' Conveniently open files found via [find_files()]. Requires \pkg{rstudioapi}.
#'
#' @param ... either
#' * `integer` vectors: open file(s) number x of the last [find_files()] result
#' * `character` vectors: file paths
#'
#' @export
#' @seealso [find_files()]
#' @examples
#' \dontrun{
#'
#' find_files("sif", markers = FALSE)
#'
#' #> Results for sif
#' #>
#' #> 1 ./R/sif.R
#' #> 2 ./R/sif_results.R
#' #> 3 ./R/sifr-package.R
#' #> 4 ./tests/testthat/test_sif.R
#' #> 5 ./tests/testthat/testdata/sif_test.r
#' #> 6 ./tests/testthat/testdata/sif_test.rMd
#'
#' open_files(3, 4)
#' }
open_files <- function(...){
  stopifnot(requireNamespace("rstudioapi", quietly = TRUE))

  x <- c(...)

  last_find_files <- get(".last_find_files", pos = sifr_last)

  if (rlang::is_integerish(x)){
    paths <- last_find_files$contents[x]
  } else {
    paths <- x
  }

  for (path in paths){
    rstudioapi::navigateToFile(path)
  }

  invisible(paths)
}


sifr_last <- new.env()
