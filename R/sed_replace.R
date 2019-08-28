#' Replace a matched regular expression pattern accross multiple files
#'
#' Search and replace a regex pattern across several files. This only works on
#' POSIX compatible systems where [sed](https://en.wikipedia.org/wiki/Sed)
#' is available. It is recommended to use this feature only on files checked
#' into a version control system like *git*.
#'
#' @section Notes:
#'
#' *sed* has its own flavour of regular expression that does not support all
#' features. The exact features supported are platform dependent (e.g.
#' case insensitive matching is not supported on macOS).
#'
#' @param pattern `character` scalar. A *sed* compatible regular expression
#'   pattern for which to search in files.
#' @param replace `character` scalar.
#' @param case_sensitive `logical` scalar. If `TRUE` `pattern` is matched
#'   case sensitvely. Please not that this is not supported on BSD
#'   platforms such as macOS. See
#'   [this stackoverflow post](https://stackoverflow.com/questions/4412945/case-insensitive-search-and-replace-with-sed).
#' @inheritParams sif
#'
#' @return A `sif_result` `data.table` that contains all matched lines (before replacement took place)
#' @section Side Effects: Replaces text that matches `pattern` accross all
#'   files that match `path_pattern` with `replace`.
#' @export
#'
sed_replace <- function(
  pattern,
  replace,
  dir = ".",
  case_sensitive = TRUE,
  path_pattern = getOption("sifr.path_pattern"),
  path_case_sensitive = FALSE,
  recursive = TRUE
){
  m = sif(
    pattern = pattern,
    dir = dir,
    case_sensitive = case_sensitive,
    path_case_sensitive = path_case_sensitive,
    path_pattern = path_pattern,
    recursive = recursive,
    fixed = FALSE,
    markers = FALSE
  )

  if (identical(nrow(m), 0L)){
    message("Nothing to replace\n")
    return(invisible())
  }

  opt_case <- {if (case_sensitive) {
    if (Sys.info()["sysname"] == "Darwin"){
      warning("`case_sensitive` == FALSE is not supported on MacOS")
    }
    "I"
  } else {
    ""
  }}

  for (f in m$path){
    system2(
      "sed",
      sprintf("-i 's/%s/%s/%s' %s", pattern, replace, opt_case, f)
    )
  }

  m
}
