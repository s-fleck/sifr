as_sif_result <- function(x){
  class(x) <- c("sif_result", "data.table", "data.frame")
  x
}




as_sifkw_result <- function(x){
  class(x) <- c("sifkw_result", "sif_result", "data.table", "data.frame")
  x
}




#' Printing Sif Results
#'
#' @param x a `sif_result`
#' @param ... ignored
#'
#' @return `x` (invisibly)
#' @export
#'
print.sif_result <- function(x, ...){
  if (nrow(x) < 1){
    cat("No matches found")
    return(x)
  }

  dd <- data.table::copy(x)
  dd[, ln := style_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]

  path_old <- ""
  for (i in seq_len(nrow(dd))){
    path <- dd[i]$file

    if (!identical(path, path_old)){
      path_old <- path
      cat("\n", style_path(path), "\n", sep = "")
    }

    cat(dd$ln[[i]], " ", color_at_pos(dd$text[[i]], dd[i]$pos[[1]]), "\n")
  }

  invisible(x)
}




#' Printing Sif Results
#'
#' @param x a `sif_result`
#' @param ... ignored
#'
#' @return `x` (invisibly)
#' @export
#'
print.sifkw_result <- function(
  x,
  ...
){
  if (nrow(x) < 1){
    cat("No matches found")
    return(x)
  }

  dd <- data.table::copy(x)
  dd[, ln := style_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]

  path_old <- ""
  for (i in seq_len(nrow(dd))){
    path <- dd[i]$file

    if (!identical(path, path_old)){
      path_old <- path
      cat("\n", style_path(path), "\n", sep = "")
    }

    s <- color_at_pos(dd[i]$text, dd[i]$pos[[1]], style_accent)
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
