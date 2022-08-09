#' Title
#'
#' @param infile filepath to a .ma file to be recaled
#' @param out where to put output files
#' @param pi pi parameter for sbayesr
#' @param gamma gmama paramter for sbayesr
#' @param chain_length paramter for sbayers
#' @param burn_in paramter for sbayesr
#' @param thin parameter for sbayesr
#' @param out_freq parameters for sbayesr
#' @param ldmatrix filepath for the ldmatrix needed for sbayesr
#' @param gctb filepath for sbayesr
#' @param name prefix name for all the output files
#' @param from_env if TRUE, the function looks up the filepaths by looking in
#' sys environment using GCTB, GCTB_LDMATRIX and PRS_ARCHIVE
#'
#' @return a string
#' @export
#'
#' @examples \dontrun{
#' make_sbayesr_job("test_infile.ma", "my_out_dir", name="demo")
#' }
make_sbayesr_job <- function(infile,
                             pi="0.95,0.02,0.02,0.01",
                             gamma="0.0,0.01,0.1,1", chain_length=10000,
                             burn_in=4000, thin=10, out_freq=10, out,
                             ldmatrix, gctb, name, from_env=TRUE) {

  stopifnot("Provide a name as prefix for the output files" = !missing(name))

  # use environment to figure out filepaths for GCTB and the ld matrix
  if(from_env){
    gctb <- Sys.getenv("GCTB")
    ldmatrix <- Sys.getenv("GCTB_LDMATRIX")

    stopifnot("Cannot find GCTB in environmental variables" = !gctb == "")
    stopifnot("Cannot find GCTB_LDMATRIX in environmental variables" = !ldmatrix == "")

    # if the out variable is missing, set it to PRS_ARCHIVE if that exists
     if(missing(out)) {
       # check that the PRS_ARCHIVE filepath exists
      stopifnot(!Sys.getenv("PRS_ARCHIVE") == "")
      out <- Sys.getenv("PRS_ARCHIVE")
    }
  }

  glue::glue(

"{gctb} ",
  "--sbayes R ",
  "--mldm {ldmatrix} ",
  "--gwas-summary {infile} ",
  "--robust ",
  "--pi {pi} ",
  "--gamma {gamma} ",
  "--impute-n ",
  "--ambiguous-snp ",
  "--chain-length {chain_length} ",
  "--burn-in {burn_in} ",
  "--thin {thin} ",
  "--out-freq {out_freq} ",
  "--no-mcmc-bin ",
  "--out {out}/{name}"
  )
}



#' Title
#'
#' @param sumstat filepath to sumstat to rescale using sbayesr
#' @param name prefix name for output files
#' @param sbatch should the script be sbatched?
#'
#' @return write a text file to prs archive
#' @export
#'
#' @examples \dontrun{
#' run_sbayesr_job("path/to/my_sumstats.ma")
#' }

run_sbayesr_job <- function(sumstat, name, sbatch=TRUE) {
  # if the name argument has not been passed, use filename of the sumstat
  if(missing(name)) name <- fs::path_ext_remove(fs::path_file(sumstat))

  # get the filepath of where to make new folders
  archive <- Sys.getenv("PRS_ARCHIVE")

  # make the output folder
  outdir <- paste0(archive, "/", name)
  fs::dir_create(outdir)
  slurmfile <- glue::glue("{outdir}/{name}-%j.out")

  header <- make_slurm_header(time = 480, ntasks = 4, mem = 40000, "-o" = slurmfile)

  # make the gctb job
  sbayes_job <- make_sbayesr_job(infile = sumstat, name = name, out = outdir)

  # make the filepath for the bash script, write it file
  fp_job <- glue::glue(outdir,"/{name}_sbatch.sh")
  writeLines(c(header, sbayes_job),fp_job)

  # dispatch job
  if(sbatch) system(glue::glue("sbatch {fp_job}"))


}

#' Title
#'
#' @param bfile filepath to genetic bed bim fam genetic data
#' @param score filepath to sumstat file to score
#' @param out filepath to out directory
#' @param threads number of threads
#' @param name prefix for file names.
#'
#' @return a string
#' @export
#'
#' @examples
#' make_plink_score_job(bfile = "GENODATA/bedbimfam", score = "mdd_rescaled.snpRes", "my_dir/results", name = "mdd2021")
#'
make_plink_score_job <- function(bfile, score, out, threads=4, name){
  glue::glue(
    "plink2 ",
    "--bfile {bfile} ",
    "--score {score} 2 5 8 header ",
    "--threads {threads} ",
    "--variance-standardize ",
    "--out {out}/{name}",
  )
}

parse_arg <- function(flag, arg, slurm=TRUE){
  # add #SBATCH if slurm flag is TRUE
  if(slurm) {
    # are flags given in quick format? e.x (-J, -A, -T)
    if(stringr::str_detect(flag, "-")) {
      glue::glue("#SBATCH {flag} {arg}")
    } else {
      glue::glue("#SBATCH --{flag} {arg}")
    }

  } else {
    glue::glue("--{flag} {arg}")
  }

}



#' Title
#'
#' @param ... key-value pairs of slurm flags. Accepts both (ntasks = 6) and
#' shortform (-N 6)
#'
#' @return a character vector
#' @export
#'
#' @examples make_slurm_header("-A" = "sens2017519", ntasks = 6, "-T" = "24:00:00")
make_slurm_header <- function(...) {
  args <- list(...)
  flags <- names(args)

  all_args <- purrr::map2(names(args), args, parse_arg) %>% purrr::reduce(c)
  c("#!/bin/bash", all_args) %>%
    purrr::reduce(paste, sep = "\n")

}

#' Make a bash job from inside R
#'
#' @param exe filepath for program to execute
#' @param ... key-value pair arguments to apply to program
#'
#' @return a character vector
#' @export
#'
#' @examples make_bash_job(exe = "/home/gcta64", bfile = "geno/bedbimfam", "maf" = 0.01, out = "/home/arvhar/results")
make_bash_job <- function(exe, ...) {
  args <- list(...)
  flags <- names(args)

  all_args <- purrr::map2(names(args), args, parse_arg, slurm = FALSE) %>% purrr::reduce(c)
  c(glue::glue("{exe}"), all_args) %>%
    purrr::reduce(paste, sep = " ")


}





