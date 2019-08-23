sifkw <- function(
  keywords,
  dir = ".",
  regex = FALSE,
  case_sensitive = FALSE,
  file_pattern = "(.*\\.R$)|(.*\\.Rmd$)|(.*\\.Rnw$))",
  file_case_sensitive = FALSE,
  recursive = TRUE,
  mc.cores = getOption("mc.cores", 2L)
){
  r <- sifr::sif(
    paste0("keyword.*"),
    dir = dir,
    regex = TRUE,
    case_sensitive = case_sensitive,
    file_pattern = file_pattern,
    file_case_sensitive = file_case_sensitive,
    recursive = recursive,
    mc.cores = mc.cores
  )


  if (regex){
    r[, pos := list(stringi::stri_locate_all_regex(r$text, paste0("(", keywords , ")", collapse  = "|"))) ]

  } else {
    matches <- list()

    for (kw in keywords){
      matches[[kw]] <- stringi::stri_locate_all_fixed(r$text, kw)
    }

    matches <- do.call(
      mapply,
      c(list(rbindsort, SIMPLIFY = FALSE, USE.NAMES = FALSE), matches)
    )

    r[, pos := matches]
  }


  as_sifkw_result(r)
}




rbindsort <- function(...){
  res <- rbind(...)
  res[order(res[, 1]), ]
}

