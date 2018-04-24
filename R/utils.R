is_scalar <- function(x){
  identical(length(x), 1L)
}


is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}



is_scalar_logical <- function(x){
  is.logical(x) && is_scalar(x)
}
