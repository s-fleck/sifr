sed_replace <- function(
  x,
  pattern,
  replace,
  case_sensitive = TRUE,
  file_case_sensitive = FALSE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)",
  recursive = TRUE
){
  m = sif(
    x = x,
    pattern = pattern,
    case_sensitive = case_sensitive,
    file_case_sensitive = file_case_sensitive,
    file_pattern = file_pattern,
    recursive = recursive,
    regex = FALSE
  )

  if (identical(nrow(m), 0L)){
    message("Nothing to replace")
    return(invisible())
  }

  for (f in m$file){
    scan(pipe(sprintf("sed -i 's/%s/%s/' %s", pattern, replace, f)))
  }

  m
}
