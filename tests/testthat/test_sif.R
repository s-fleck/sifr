context("sif")


test_that("sif works as expected", {
  d <- testthis::find_testdata()
  y <- sif(d, pattern = "match_me_sif_.*", fixed = TRUE, markers = FALSE)
  expect_null(y)
  y <- sif(d, pattern = "match_me_sif_.*", fixed = FALSE, markers = FALSE)

  expect_true(any(grepl("sif_test.r", y$file)))
  expect_true(any(grepl("sif_test.rMd", y$file)))
})




test_that("color_at_pos", {

  pos <- matrix(c(5, 10, 8, 12), ncol = 2, dimnames = list(NULL, c("start", "end")))

  expect_identical(
    color_at_pos("foo-blue-red-bar", pos, color = crayon::cyan),
    "foo-\033[36mblue\033[39m-\033[36mred\033[39m-bar"
  )
})




test_that("color_at_pos", {
  r1 <- sifkw(c("bar", "ash"), testthis::find_testdata(), fixed = FALSE, markers = FALSE)
  r2 <- sifkw(c("bar", "ash"), testthis::find_testdata(), fixed = TRUE, markers = FALSE)
  as.data.frame(r1)
  as.data.frame(r2)
  expect_identical(r1, r2)
})




test_that("sifkw works as expected", {

  res <-
    sifkw(c("bar", "ash"), testthis::find_testdata(), fixed = FALSE, markers = FALSE)

  expect_equal(
    res$pos[[1]],
    matrix(c(11, 13, 19, 21), byrow = TRUE, ncol = 2, dimnames = list(c(), c("start", "end")))
  )

  expect_equal(
    res$pos[[2]],
    matrix(c(14, 16, 19, 21), byrow = TRUE, ncol = 2, dimnames = list(c(), c("start", "end")))
  )
  # print(res)


  res_fixed <-
    sifkw(c("bar", "ash"), testthis::find_testdata(), fixed = TRUE, markers = FALSE)

  expect_identical(res, res_fixed)
})
