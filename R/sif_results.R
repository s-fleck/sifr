as_sif_result <- function(x, pattern){
  class(x) <- c("sif_result", "data.table", "data.frame")
  attr(x, "pattern") <- pattern
  x
}




as_sifkw_result <- function(x, keywords){
  class(x) <- c("sifkw_result", "sif_result", "data.table", "data.frame")
  attr(x, "keywords") <- keywords
  x
}




#' Printing sif results
#'
#' @param x a `sif_result`
#' @inheritParams sif
#' @param ... ignored
#'
#' @return `x` (invisibly)
#' @export
#'
print.sif_result <- function(
  x,
  markers = FALSE,
  ...
){
  assert(is_scalar_bool(markers))
  cat("Results for", style_accent(attr(x, "pattern")), "\n")
  ln <- pos <- NULL

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
    dd[, ln := style_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]

    path_old <- ""
    for (i in seq_len(nrow(dd))){
      path <- dd[i]$path

      if (!identical(path, path_old)){
        path_old <- path
        cat("\n", style_path(path), "\n", sep = "")
      }

      cat(dd$ln[[i]], " ", color_at_pos(dd$contents[[i]], dd[i]$pos[[1]]), "\n")
    }

  invisible(x)
}




#' @rdname print.sif_result
#' @export
print.sifkw_result <- function(
  x,
  markers = FALSE,
  ...
){
  assert(is_scalar_bool(markers))
  ln <- pos <- NULL

  kw <- vapply(attr(x, "keywords"), style_accent, character(1))
  kw <-paste(kw, collapse = ", ")

  cat("Results for the keywords:", kw, "\n")

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
    dd[, ln := style_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]

    path_old <- ""
    for (i in seq_len(nrow(dd))){
      path <- dd[i]$path

      if (!identical(path, path_old)){
        path_old <- path
        cat("\n", style_path(path), "\n", sep = "")
      }

      s <- color_at_pos(dd[i]$contents, dd[i]$pos[[1]], style_accent)
      s <- stringi::stri_replace_first_regex(s, "keyword[s]{0,1}", style_kw("$0"))

      cat(dd[i]$ln, s, "\n")
    }

  invisible(x)
}





is_sif_results <- function(x){
  inherits(x, "sif_results")
}




color_at_pos = function(text, pos, color = style_accent){
  stringi::stri_sub_all(text, pos[, "start"], pos[, "end"]) <-
    color(stringi::stri_sub(text, pos[, "start"], pos[, "end"]))
  text
}
