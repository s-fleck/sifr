context("sif")


test_that("sif works as expected", {

  d <- testthis::find_testdata()
  y <- sif(d, pattern = "match_me_sif_.*", regex = FALSE)
  expect_null(y)
  y <- sif(d, pattern = "match_me_sif_.*", regex = TRUE)

  expect_true(any(grepl("sif_test.r", y$file)))
  expect_true(any(grepl("sif_test.rMd", y$file)))
})




test_that("grep_file", {
  res <-
    grep_file(testthis::find_testdata("sif_test.r"), pattern = "match_me_sif")

})




test_that("color_at_pos", {
  pos <- matrix(c(5, 10, 8, 12), ncol = 2, dimnames = list(NULL, c("start", "end")))

  expect_identical(
    color_at_pos("foo-blue-red-bar", pos),
    "foo-\033[38;5;117mblue\033[39m-\033[38;5;117mred\033[39m-bar"
  )
})




test_that("color_at_pos", {
  r1 <- sifkw(c("bar", "ash"), testthis::find_testdata(), regex = TRUE)
  r2 <- sifkw(c("bar", "ash"), testthis::find_testdata(), regex = FALSE)
  as.data.frame(r1)
  as.data.frame(r2)
  expect_identical(r1, r2)
})



test_that("visual verrification", {

  d <- testthis::find_testdata()

  cat("\n\n")
  print(sif(d, pattern = "match_me_sif", regex = FALSE))

  cat("\n\n")
  print(sif(d, pattern = "match_me_sif", regex = TRUE))

  cat("\n\n")
  print(sif(d, pattern = "match_me_sif_.*", regex = TRUE))

  cat("\n\n")
  print(sifkw(c("bar", "ash"), testthis::find_testdata(), regex = TRUE))
  print(sifkw(c("bar", "ash"), testthis::find_testdata(), regex = FALSE))
  cat("\n\n")
})
