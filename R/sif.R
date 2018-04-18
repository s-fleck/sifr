#' Title
#'
#' @param x
#' @param pattern
#' @param case_sensitive
#' @param regex
#' @param file_pattern
#' @param file_case_sensitive
#' @param recursive
#'
#' @return
#' @export
#'
#' @examples
sif <- function(
  x,
  pattern,
  case_sensitive = TRUE,
  regex = FALSE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)",
  file_case_sensitive = FALSE,
  recursive = TRUE
){
  files <- list.files(
    x,
    full.names = TRUE,
    pattern = file_pattern,
    ignore.case = !file_case_sensitive,
    recursive = recursive,
    all.files = TRUE
  )

  res <- lapply(files, grep_file, pattern = pattern, case_sensitive = case_sensitive, regex = regex, highlight = TRUE)
  sif_result(data.table::rbindlist(res))

}




grep_file <- function(
  x,
  pattern,
  regex = FALSE,
  case_sensitive = TRUE,
  highlight = FALSE
){
  lines <- readr::read_lines(x)

  if (!regex){
    sel <- stringi::stri_detect_fixed(
      lines,
      pattern,
      opts_regex = stringi::stri_opts_fixed(case_insensitive = !case_sensitive)
    )
  } else {
    sel <- stringi::stri_detect_regex(
      lines,
      pattern,
      opts_regex = stringi::stri_opts_regex(case_insensitive = !case_sensitive)
    )
  }

  if (!any(sel)){
    return(NULL)
  }

  if (!regex && highlight){
    data.table(
      file = x,
      ln = which(sel),
      text = stringi::stri_replace_all_fixed(lines[sel], pattern, colt::clt_maybe(pattern)
    ))

  } else {
    data.table(
      file = x,
      ln = which(sel),
      text = lines[sel]
    )

  }




}
