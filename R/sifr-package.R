#' @keywords internal
#' @importFrom data.table data.table
#'
#' @section Options:
#'
#' \describe{
#' \item{`sifr.path_pattern`}{A `character` scalar containing a `regex` pattern
#'   to match file paths against. This is used as the default for [sif()]
#'   and co.}
#' }
#'
"_PACKAGE"





.onLoad <- function(...){
  op <- options()
  op.this <- list()

  op.this[["sifr.path_pattern"]] <-
    "(.*\\.R$)|(.*\\.Rmd$)|(.*\\.Rnw$)|(.*\\.Rhtml$)|(.*\\.cpp$)|(.*\\.c$))"


  toset <- !(names(op.this) %in% names(op))
  if(any(toset)) options(op.this[toset])


  # +- colors --------------------------------------------------------------
  if (requireNamespace("colt", quietly = TRUE) && crayon::has_color()){
    style_accent     <- colt::clt_chr_accent
    style_path   <- colt::clt_warning
    style_subtle <- colt::clt_chr_subtle
    style_kw     <- colt::clt_error

  } else if (requireNamespace("crayon", quietly = TRUE)  && crayon::has_color()){
    style_accent     <- crayon::make_style("#ca2c92", colors = 256)
    style_path   <- crayon::make_style("#EEBB50", colors = 256)
    style_subtle <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
    style_kw     <- crayon::make_style("#BB3333", colors = 256)

  } else {
    style_accent     <- function(...) paste(...)
    style_path   <- style_accent
    style_subtle <- style_accent
    style_kw     <- style_accent
  }

  # make color functions available inside the package
  assign("style_accent", style_accent, envir = parent.env(environment()))
  assign("style_path", style_path, envir = parent.env(environment()))
  assign("style_subtle", style_subtle, envir = parent.env(environment()))
  assign("style_kw", style_kw, envir = parent.env(environment()))


}

