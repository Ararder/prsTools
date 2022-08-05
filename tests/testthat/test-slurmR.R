

test_that("Errors if no name is passed", {
  fs::file_create("test_gwas.ma")
  expect_error(make_sbayesr_job("test_gwas.ma"))
  fs::file_delete("test_gwas.ma")
})

test_that("Detects the environmental variables", {
  fs::file_create("test_gwas.ma")
  gctb <- '/nfs/home/arvhar/gctb'
  ld <- '/nfs/home/arvhar/gctb/ldmatrix'
  prs <- glue::glue('{getwd()}')

  withr::local_envvar(GCTB=gctb)
  withr::local_envvar(GCTB_LDMATRIX=ld)
  withr::local_envvar(PRS_ARCHIVE="")
  expect_error(make_sbayesr_job("test_gwas.ma", name = "test_gwas"))

  # i expect the job to error out if i dont pass arguments for ldmatrix or gctb,
  # and they dont exist in environment

  # Check GCTB
  withr::local_envvar(GCTB="")
  withr::local_envvar(GCTB_LDMATRIX=ld)
  withr::local_envvar(PRS_ARCHIVE="")
  expect_error(make_sbayesr_job("test_gwas.ma", name="test_gwas"))

  # Check LDMATRIX
  withr::local_envvar(GCTB=gctb)
  withr::local_envvar(GCTB_LDMATRIX="")
  withr::local_envvar(PRS_ARCHIVE="")
  expect_error(make_sbayesr_job("test_gwas.ma", name="test_gwas"))
  fs::file_delete("test_gwas.ma")

})


test_that("Creates a file based on sumstat name",{

  gctb <- '/nfs/home/arvhar/gctb'
  ld <- '/nfs/home/arvhar/gctb/ldmatrix'
  prs <- glue::glue('{getwd()}')
  withr::local_envvar(GCTB=gctb)
  withr::local_envvar(GCTB_LDMATRIX=ld)
  withr::local_envvar(PRS_ARCHIVE=prs)

  fs::file_create("my_gwas.ma")
  run_sbayesr_job("my_gwas.ma", sbatch = FALSE)
  expect_true(fs::file_exists("my_gwas_sbatch.sh"))
  fs::file_delete("my_gwas.ma")
  fs::file_delete("my_gwas_sbatch.sh")

})




