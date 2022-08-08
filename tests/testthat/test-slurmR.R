

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

  strings <- "some stuff to write"
  gctb <- '/nfs/home/arvhar/gctb'
  ld <- '/nfs/home/arvhar/gctb/ldmatrix'
  gwas <- withr::local_tempfile(lines = strings, pattern = "gwas.ma")
  dir <- fs::path_dir(gwas)
  file <- fs::path_file(gwas)

  withr::local_envvar(GCTB=gctb)
  withr::local_envvar(GCTB_LDMATRIX=ld)
  withr::local_envvar(PRS_ARCHIVE=dir)

  print(glue::glue("prs archive is {dir}"))
  print(glue::glue("gwas sumstats in {gwas}"))
  print(glue::glue("gwas file name is {file}"))


  run_sbayesr_job(gwas, sbatch = FALSE)
  print(fs::dir_ls(dir))
  expect_true(fs::dir_exists(dir))

  # print(glue::glue("making a folder in {dir} named {gwas}"))
  # fs::file_delete("my_gwas.ma")
  # fs::file_delete("my_gwas_sbatch.sh")

})

test_that("a directory is created", {
    dir <- withr::local_tempdir()
    withr::local_envvar(GCTB="home/gctb")
    withr::local_envvar(GCTB_LDMATRIX="home/ldpaths.txt")
    withr::local_envvar(PRS_ARCHIVE=dir)

    gwas_path <- fs::file_touch(paste0(dir, "/", "made_up_gwas.ma"))

    run_sbayesr_job(gwas_path, sbatch = FALSE)


    expect_true(fs::dir_exists(
      paste0(
        dir, "/", "made_up_gwas"
      )))

    expect_true(fs::file_exists(
      paste0(
        dir, "/", "made_up_gwas", "/", "made_up_gwas_sbatch.sh"
      )))



})



