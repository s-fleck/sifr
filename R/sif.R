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
  dir = ".",
  markers = interactive() && requireNamespace("rstudioapi", quietly = TRUE),
  fixed = FALSE,
  case_sensitive = TRUE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)",
  file_case_sensitive = FALSE,
  recursive = TRUE,
  encoding = "unknown"
){
  stopifnot(
    is_scalar_character(pattern),
    dir.exists(dir),
    is_scalar_logical(fixed),
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
        fixed = fixed,
        highlight = TRUE,
        encoding = encoding
      )
    }
  )
  res <- data.table::rbindlist(res)

  if (identical(nrow(res), 0L)){
    message(sprintf(
      "No files found in '%s' that containt the%spattern '%s'\n",
      style_accent(normalizePath(dir)),
      ifelse(fixed, " ", " regex "),
      style_accent(pattern)
    ))

    return(invisible())
  }

  if (markers){
    rstudioapi::sourceMarkers(
      name = "sifr results",
      markers = data.frame(
        type = "info",
        file = res$file,
        line = res$ln,
        column = 1,
        message = res$text,
        stringsAsFactors = FALSE
      ),
      basePath = fs::path_common(res$file)
    )
    invisible(as_sif_result(res))
  } else {
    as_sif_result(res)
  }

}




#' @rdname sif
#' @export
sifkw <- function(
  keywords,
  dir = ".",
  markers = interactive() && requireNamespace("rstudioapi", quietly = TRUE),
  fixed = FALSE,
  case_sensitive = FALSE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)|(.*\\.Rnw$))",
  file_case_sensitive = FALSE,
  recursive = TRUE
){
  assert(is.character(keywords))
  res <- sifr::sif(
    paste0("keyword"),
    dir = dir,
    fixed = FALSE,
    case_sensitive = case_sensitive,
    file_pattern = file_pattern,
    file_case_sensitive = file_case_sensitive,
    recursive = recursive,
    markers = FALSE
  )

  if (!fixed){
    res[, pos := list(stringi::stri_locate_all_regex(res$text, paste0("(", keywords , ")", collapse  = "|"))) ]

  } else {
    matches <- list()

    for (kw in keywords){
      matches[[kw]] <- stringi::stri_locate_all_fixed(res$text, kw)
    }

    matches <- do.call(
      mapply,
      c(list(rbindsort, SIMPLIFY = FALSE, USE.NAMES = FALSE), matches)
    )

    res[, pos := matches]
  }

  res <- res[!vapply(res$pos, anyNA, logical(1))]

  if (markers){
    rstudioapi::sourceMarkers(
      name = "sifr results",
      markers = data.frame(
        type = "info",
        file = res$file,
        line = res$ln,
        column = 1,
        message = res$text,
        stringsAsFactors = FALSE
      ),
      basePath = fs::path_common(res$file)
    )
    invisible(as_sifkw_result(res))
  } else {
    as_sifkw_result(res)
  }
}




# utils -------------------------------------------------------------------

rbindsort <- function(...){
  res <- rbind(...)
  res[order(res[, 1]), ]
}




grep_file <- function(
  x,
  pattern,
  fixed = FALSE,
  case_sensitive = TRUE,
  highlight = FALSE,
  encoding = "unknown"
){
  lines      <- suppressWarnings(readLines(x, encoding = encoding))
  opts_regex <- stringi::stri_opts_fixed(case_insensitive = !case_sensitive)

  if (fixed){
    detector <- stringi::stri_detect_fixed
    locator  <- stringi::stri_locate_all_fixed
  } else {
    detector <- stringi::stri_detect_regex
    locator  <- stringi::stri_locate_all_regex
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
