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

  dd <- data.table::copy(x)
  dd[, ln := colt::clt_chr_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]

  for (i in seq_len(nrow(dd))){
    cat(color_at_pos(dd[i]$text, dd[i]$pos[[1]]), "\n")
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
print.sifkw_result <- function(x, ...){
  dd <- data.table::copy(x)
  dd[, ln := colt::clt_chr_subtle(stringi::stri_pad_left(ln, max(nchar(ln)))) ]


  for (i in seq_len(nrow(dd))){
    s <- color_at_pos(dd[i]$text, dd[i]$pos[[1]], colt::clt_chr_accent)
    s <- stringi::stri_replace_first_regex(
      s,
      "keyword[s]{0,1}",
      colt::clt_error("$0")
    )
    cat(s, "\n")
  }

  invisible(x)
}





is_sif_results <- function(x){
  inherits(x, "sif_results")
}




color_at_pos = function(text, pos, color = colt::clt_chr_accent){
  stringi::stri_sub_all(text, pos[, "start"], pos[, "end"]) <-
    color(stringi::stri_sub(text, pos[, "start"], pos[, "end"]))
  text
}
