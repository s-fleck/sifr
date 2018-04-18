context("sif")


test_that("sif works as expected", {

  x <- "~/rpkgs"
  y <- sif(x, pattern = "match_me_sif_.*")
  y <- sif(x, pattern = "match_me_sif_.*", fixed = FALSE)

  sif(x, pattern = "dint::date")

  class(y)

  y


  expect_true(any(grepl("sif_test.r", , names(y), fixed = TRUE)))
  expect_true(any(grepl("sif_test.rMd", pattern = "match_me_sif_.*", names(y), fixed = TRUE)))

  grep_file(
    rprojroot::find_testthat_root_file("testdata", "sif_test.r"),
    "match_me_sif_.*"
  )



})
