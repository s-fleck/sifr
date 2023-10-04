#' Convert sifr result objects to RStudio source markers
#'
#' @param x Any supported \R object
#'
#' @return An [rstudioapi::sourceMarkers] object
#'
#' @export
as_source_markers <- function(x){
  if (!requireNamespace("rstudioapi", quietly = TRUE)){
    stop("Source markers are only available in RStudio", call. = FALSE)
  }

  UseMethod("as_source_markers")
}




#' @rdname as_source_markers
#' @export
as_source_markers.sifkw_result <- function(x){

  contents <- NULL
  name <- paste("sifkw:", paste(attr(x, "keywords"), collapse = ", "))

  x <- data.table::copy(x)
  x[, message := contents]
  for (i in seq_len(nrow(x))){
    x[i, message := color_at_pos(x$contents[[i]], x$pos[[i]], style_kw)]
  }

  rstudioapi::sourceMarkers(
    name = name,
    markers = data.frame(
      type = "info",
      file = x$path,
      line = x$ln,
      column = 1,
      message = x$message,
      stringsAsFactors = FALSE
    ),
    basePath = fs::path_common(fs::path_real(x$path))
  )
}




#' @rdname as_source_markers
#' @export
as_source_markers.sif_result<- function(x){

  contents <- NULL
  name <- paste("sif:", attr(x, "pattern"))

  x <- data.table::copy(x)
  x[, message := contents]
  for (i in seq_len(nrow(x))){
    x[i, message := color_at_pos(x$contents[[i]], x$pos[[i]], style_kw)]
  }

  rstudioapi::sourceMarkers(
    name = name,
    markers = data.frame(
      type = "info",
      file = x$path,
      line = x$ln,
      column = 1,
      message = x$message,
      stringsAsFactors = FALSE
    ),
    basePath = fs::path_common(fs::path_real(x$path))
  )
}




#' @rdname as_source_markers
#' @export
as_source_markers.find_files_result<- function(x){

  contents <- NULL
  name <- paste("find files:", attr(x, "pattern"))

  x <- data.table::copy(x)
  x[, message := contents]
  for (i in seq_len(nrow(x))){
    x[i, message := color_at_pos(x$contents[[i]], x$pos[[i]], style_kw)]
  }

  rstudioapi::sourceMarkers(
    name = name,
    markers = data.frame(
      type = "info",
      file = x$contents,
      line = 1,
      column = 1,
      message = x$message,
      stringsAsFactors = FALSE
    ),
    basePath = fs::path_common(fs::path_real(x$contents))
  )
}
