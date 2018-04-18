context("sif_replace")


test_that("sif_replace works as expected", {

  x <- "~/rpkgs/sifr"

  sed_replace(x, pattern = "match_me_sif_3", "match_me_sif_33", file_pattern = "sif_test\\.r")

  sif(x, "match_me_sif_3")
  sed_replace(x, pattern = "match_me_sif_33", "match_me_sif_3", file_pattern = "sif_test\\.r")


  y


})
