#' Detect pattern across multiple files
#'
#' `sif()` recursively searches all files inside a directory for a term
#' (usually a regex pattern). If used interactively from RStudio it defaults
#' to displaying the results in the Markers pane. This is very similar to
#' RStudios *find in files* feature, but has the advantage that the search
#' results are also returned as a `data.frame` that can be further processed.
#'
#' @param pattern `character` scalar. A pattern for which to search in files.
#' @param fixed `logical` scalar. If `FALSE` `pattern` is
#'   interpreted as regular expression, else the string is matched as-is.
#' @param dir `character` scalar. A file system path to the directory in which
#'   files should be search.
#' @param case_sensitive `logical` scalar. If `TRUE` `pattern` is matched
#'   case sensitively.
#' @param path_pattern `character` scalar. A regular expression pattern to match
#'   file paths against. Defaults to common source files for R
#'   (`.R`, `.Rmd`, `Rnw`, `Rhtml`, `c`, `cpp`). Please note that `.Rd` files
#'   are *not* included by default. You can modify the default behavior by
#'   setting the [option()] `sifr.path_pattern`.
#' @param path_case_sensitive `logical`. If `TRUE` `path_pattern` is matched
#'   case sensitively.
#' @param recursive `logical` scalar. If `TRUE` files are searched recursively
#'   starting from `dir`.
#' @param encoding passed on to [base::readLines()]
#' @param markers `logical` scalar. Display results in the RStudio source
#'   markers pane.
#'
#' @return
#'   A `sif_result` or `sifkw_results` `data.table` with the columns:
#'
#'   * `path` - path to the file,
#'   * `ln` - line number,
#'   * `pos` - a list column of numeric two-column `matrices` indicating start
#'      and end of each match, see [stringi::stri_locate_all()]
#'   * `contents` - the text contents of the line
#'
#'   The returned object may also contain the attributes `"pattern"`
#'   (for `sif()`) or `"keywords"` (for `sifkw()`) that contain the original
#'   search terms
#'
#' @export
#'
#' @examples
#'  tf <- tempfile(fileext = ".csv")
#'  write.csv(iris, tf)
#'
#'  x <- sif("5.5", dir = dirname(tf), markers = FALSE, path_pattern = ".*\\.csv$")
#'
#'  print(x)
#'  as.data.frame(x)
#'  attr(x, "pattern")
#'
#'  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)){
#'    print(x, markers = TRUE)
#'  }
#'  unlink(tf)  #cleanup
sif <- function(
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
    dir.exists(dir),
    is_scalar_bool(fixed),
    is_scalar_bool(case_sensitive),
    is_scalar_character(path_pattern),
    is_scalar_bool(path_case_sensitive),
    is_scalar_bool(recursive)
  )

  files <- list.files(
    dir,
    full.names = TRUE,
    pattern = path_pattern,
    ignore.case = !path_case_sensitive,
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

  res <- as_sif_result(res, pattern)

  if (markers && nrow(res) > 0){
    source_markers(res)
    invisible(res)
  } else {
    res
  }
}




#' `sifkw()` is like `sif()` but only searches in rows that contain the term
#' `keyword`. This is useful for searching rmarkdown documents that
#' contain a `keywords: ` directive in the YAML header or `@keywords` roxygen
#' tags.
#'
#' @param keywords a `character` vector of keywords
#'
#' @rdname sif
#' @export
sifkw <- function(
  keywords,
  dir = ".",
  markers = interactive() && requireNamespace("rstudioapi", quietly = TRUE),
  fixed = FALSE,
  case_sensitive = FALSE,
  path_pattern = getOption("sifr.path_pattern"),
  path_case_sensitive = FALSE,
  recursive = TRUE
){
  assert(is.character(keywords))

  res <- sifr::sif(
    paste0("keyword"),
    dir = dir,
    fixed = FALSE,
    case_sensitive = case_sensitive,
    path_pattern = path_pattern,
    path_case_sensitive = path_case_sensitive,
    recursive = recursive,
    markers = FALSE
  )

  if (nrow(res) > 0){
    pos <- NULL  # for R CMD CHECK
    if (!fixed){
      res[, pos := list(stringi::stri_locate_all_regex(res$contents, paste0("(", keywords , ")", collapse  = "|"))) ]

    } else {
      matches <- list()

      for (kw in keywords){
        matches[[kw]] <- stringi::stri_locate_all_fixed(res$contents, kw)
      }

      matches <- do.call(
        mapply,
        c(list(rbindsort, SIMPLIFY = FALSE, USE.NAMES = FALSE), matches)
      )

      res[, pos := matches]
    }
  }

  res <- as_sifkw_result(
    res[!vapply(res$pos, anyNA, logical(1))],
    keywords
  )

  if (markers && nrow(res) > 0){
    source_markers(res)
    invisible((res))
  } else {
    res
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
    path = x,
    ln   = which(sel),
    pos  = locator(lines, pattern, opts_regex = opts_regex),
    contents = lines
  )
}
