as_find_files_result <- function(x, pattern){
  class(x) <- c("find_files_result", "data.table", "data.frame")
  attr(x, "pattern") <- pattern
  x
}




#' Printing sif results
#'
#' @param x a `find_files_result`
#' @inheritParams sif
#' @param ... ignored
#'
#' @return `x` (invisibly)
#' @export
#'
print.find_files_result <- function(
    x,
    markers = FALSE,
    ...
){
  assert(is_scalar_bool(markers))
  cat("Results for", style_accent(attr(x, "pattern")), "\n\n")
  ln <- .N <- contents <- NULL

  # early exits
  if (nrow(x) < 1){
    cat("No matches found")
    return(invisible(x))

  } else if (markers){
    as_source_markers(x)
    return(invisible(x))
  }

  # logic
  dd <- data.table::copy(x)
  dd[, ln := 1:.N]
  dd[, ln := style_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]

  dd[, contents := contents]

  for (i in seq_len(nrow(dd))){
    cat(dd$ln[[i]], " ", color_at_pos(dd$contents[[i]], dd[i]$pos[[1]]), "\n", sep = "")
  }

  invisible(x)
}




is_find_files_results <- function(x){
  inherits(x, "find_files_results")
}
