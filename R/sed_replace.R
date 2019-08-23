#' Replace a matched Regular Expression Pattern Accross Multiple Files
#'
#' Uses **sed** to replace text in files inplace. Only works on POSIX
#' systems where **sed** is available.
#'
#' @param pattern `character` scalar. A regular expression pattern for which to
#'   search in files.
#' @param replace `character` scalar.
#' @inheritParams sif
#'
#' @return A `sif_result` `data.table` that contains all matched lines (before replacement took place)
#' @section Side Effects: Replaces text that matches `pattern` accross all
#'   files that match `file_pattern` with `replace`.
#' @export
#'
sed_replace <- function(
  pattern,
  replace,
  dir = ".",
  case_sensitive = TRUE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)",
  file_case_sensitive = FALSE,
  recursive = TRUE
){
  m = sif(
    pattern = pattern,
    dir = dir,
    case_sensitive = case_sensitive,
    file_case_sensitive = file_case_sensitive,
    file_pattern = file_pattern,
    recursive = recursive,
    regex = TRUE
  )

  if (identical(nrow(m), 0L)){
    message("Nothing to replace\n")
    return(invisible())
  }

  for (f in m$file){
    system2(
      "sed",
      sprintf("-i 's/%s/%s/' %s", pattern, replace, f)
    )
  }

  m
}
