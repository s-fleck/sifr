#' Detect pattern accross multiple files
#'
#' @param pattern `character` scalar. A pattern for which to search in files.
#' @param regex `logical` scalar. If `TRUE` `pattern` ins intepreted as regular
#'   expression.
#' @param dir `character` scalar. A file system path to the directory in which
#'   files should be search.
#' @param case_sensitive `logical`.
#'   if `TRUE` `pattern` is matched case sensitvely
#' @param file_pattern `character` scalar. A regular expression pattern to match
#'   file paths against.
#' @param file_case_sensitive `logical`. If `TRUE` `file_pattern` is matched
#'   case sensitively
#' @param recursive `logical` scalar. If `TRUE` files are searched recursively
#'   starting from `dir`.
#'
#' @return A `sif_result` `data.table` that contains all matched lines
#' @export
#'
sif <- function(
  pattern,
  dir = ".",
  regex = TRUE,
  case_sensitive = TRUE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)",
  file_case_sensitive = FALSE,
  recursive = TRUE,
  mc.cores = getOption("mc.cores", 2L)
){
  stopifnot(
    is_scalar_character(pattern),
    dir.exists(dir),
    is_scalar_logical(regex),
    is_scalar_logical(case_sensitive),
    is_scalar_character(file_pattern),
    is_scalar_logical(file_case_sensitive),
    is_scalar_logical(recursive)
  )

  files <- list.files(
    dir,
    full.names = TRUE,
    pattern = file_pattern,
    ignore.case = !file_case_sensitive,
    recursive = recursive,
    all.files = TRUE
  )

  pb <- progress::progress_bar$new(
    total = length(files)
  )

  res <- parallel::mclapply(
    files,
    mc.cores = mc.cores,
    function(f) {
      pb$tick()
      grep_file(
        f,
        pattern = pattern,
        case_sensitive = case_sensitive,
        regex = regex,
        highlight = TRUE
      )
    }
  )

  res <- data.table::rbindlist(res)

  if (identical(nrow(res), 0L)){
    message(sprintf(
      "No files found in '%s' that containt the%spattern '%s'\n",
      colt::clt_chr_accent(normalizePath(dir)),
      ifelse(regex, " regex ", " "),
      colt::clt_chr_accent(pattern)
    ))

    invisible(NULL)
  } else {
    sif_result(res)
  }
}




grep_file <- function(
  x,
  pattern,
  regex = FALSE,
  case_sensitive = TRUE,
  highlight = FALSE
){
  lines <- suppressWarnings(readLines(x))

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

  lines <- lines[sel]

  if (highlight){
    if (regex){
      lines <- stringr::str_replace_all(lines, pattern, colt::clt_maybe)
    } else {
      lines <- stringi::stri_replace_all_fixed(lines, pattern, colt::clt_maybe(pattern))
    }
  }

  data.table(
    file = x,
    ln = which(sel),
    text = lines
  )
}
