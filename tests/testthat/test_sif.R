context("sif")


test_that("sif works as expected", {

  x <- "~/rpkgs"
  y <- sif(x, pattern = "match_me_sif_.*")
  y <- sif(x, pattern = "match_me_sif_.*", regex = TRUE)

  sif(x, pattern = "dint::date")

  class(y)

  y


  expect_true(any(grepl("sif_test.r", y$file)))
  expect_true(any(grepl("sif_test.rMd", y$file)))

  grep_file(
    rprojroot::find_testthat_root_file("testdata", "sif_test.r"),
    "match_me_sif_.*"
  )

})
