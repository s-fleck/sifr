context("sed_replace")


test_that("sed_replace works as expected", {
  skip("test manually")

  dir <- testthis::find_testdata()
  sed_replace(pattern = "match_me_sif_3", "match_me_sif_33", dir = dir, path_pattern = "sif_test\\.r")

  sif("match_me_sif_3", markers = FALSE)
  sed_replace(pattern = "match_me_sif_33", "match_me_sif_3", dir = dir, path_pattern = "sif_test\\.r")
})
