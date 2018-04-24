sif_result <- function(x){
  class(x) <- c("sif_result", "data.table", "data.frame")
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

  lapply(unique(dd$file), function(.f){
    cat(colt::clt_chr_accent(.f), "\n")
    sel <- which(x$file == .f)

    for (i in sel){
      cat("   ", dd[i, ln], dd[i, text], "\n" )
    }
    cat("\n")

  })

  invisible(x)
}




is_sif_results <- function(x){
  inherits(x, "sif_results")
}
