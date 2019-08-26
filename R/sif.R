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
#' @param encoding see [base::readLines()]
#'
#' @return A `sif_result` `data.table` that contains all matched lines
#' @export
#'
sif <- function(
  pattern,
  dir = getOption("sifr.default_dir", "."),
  regex = TRUE,
  case_sensitive = TRUE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)",
  file_case_sensitive = FALSE,
  recursive = TRUE,
  encoding = "unknown"
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


  res <- lapply(
    files,
    function(f) {
      grep_file(
        f,
        pattern = pattern,
        case_sensitive = case_sensitive,
        regex = regex,
        highlight = TRUE,
        encoding = encoding
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
    as_sif_result(res)
  }
}




#' @rdname sif
#' @export
sifkw <- function(
  keywords,
  dir = getOption("sifr.default_dir", "."),
  regex = FALSE,
  case_sensitive = FALSE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)|(.*\\.Rnw$))",
  file_case_sensitive = FALSE,
  recursive = TRUE
){
  r <- sifr::sif(
    paste0("keyword.*"),
    dir = dir,
    regex = TRUE,
    case_sensitive = case_sensitive,
    file_pattern = file_pattern,
    file_case_sensitive = file_case_sensitive,
    recursive = recursive
  )


  if (regex){
    r[, pos := list(stringi::stri_locate_all_regex(r$text, paste0("(", keywords , ")", collapse  = "|"))) ]

  } else {
    matches <- list()

    for (kw in keywords){
      matches[[kw]] <- stringi::stri_locate_all_fixed(r$text, kw)
    }

    matches <- do.call(
      mapply,
      c(list(rbindsort, SIMPLIFY = FALSE, USE.NAMES = FALSE), matches)
    )

    r[, pos := matches]
  }

  sel <- !vapply(r$pos, anyNA, logical(1))
  as_sifkw_result(r[sel == TRUE, ])
}




# utils -------------------------------------------------------------------

rbindsort <- function(...){
  res <- rbind(...)
  res[order(res[, 1]), ]
}




grep_file <- function(
  x,
  pattern,
  regex = FALSE,
  case_sensitive = TRUE,
  highlight = FALSE,
  encoding = "unknown"
){
  lines <- suppressWarnings(readLines(x, encoding = encoding))


  opts_regex <- stringi::stri_opts_fixed(case_insensitive = !case_sensitive)

  if (regex){
    detector <- stringi::stri_detect_regex
    locator  <- stringi::stri_locate_all_regex
  } else {
    detector <- stringi::stri_detect_fixed
    locator  <- stringi::stri_locate_all_fixed
  }

  sel   <- detector(lines, pattern, opts_regex = opts_regex)
  if (sum(sel) == 0)  return(NULL)

  lines <- lines[sel]

  res <- data.table(
    file = x,
    ln   = which(sel),
    pos  = locator(lines, pattern, opts_regex = opts_regex),
    text = lines
  )
}
