context("sed_replace")


test_that("sed_replace works as expected", {

  if (!identical(Sys.info()[["sysname"]], "Linux")){
    skip("sed_replace only fully supported on linux")
  }

  td <- file.path(tempdir(), "sifr-tests")
  tf <- file.path(td, "sif_test.r")
  on.exit({
    unlink(tf)
    unlink(td, recursive = TRUE)
  })
  dir.create(td)
  file.copy(testthis::find_testdata("sif_test.r"), tf)

  md5_ori <- unname(tools::md5sum(testthis::find_testdata("sif_test.r")))

  sed_replace(pattern = "match_me_sif_3", "SedCaseSensitive", dir = td, path_pattern = "sif_test\\.r")

  md5_scs <- unname(tools::md5sum(tf))
  expect_true(!identical(md5_ori, md5_scs))

  sed_replace(pattern = "sedcasesensitive", "xxxxx", dir = td, path_pattern = "sif_test\\.r", case_sensitive = TRUE)
  expect_identical(md5_scs, unname(tools::md5sum(tf)))

  sed_replace(pattern = "sedcasesensitive", "match_me_sif_3", dir = td, path_pattern = "sif_test\\.r", case_sensitive = FALSE)
  expect_identical(md5_ori, unname(tools::md5sum(tf)))
})
