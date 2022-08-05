#' Construct the slurm header for a script to be sent via sbatch
#'
#' @param t TIME, d:h:m:s 00:16:30:30
#' @param cores number of cores to be used
#' @param mem memory in megabytes
#' @param out where to save the slurm output file
#' @param name the name of the script
#'
#' @return a string
#' @export
#'
#' @examples make_slurm_header()
make_slurm_header <- function(t=480, cores=4, mem=10000, out=Sys.getenv("HOME"), name="slurm") {
  glue::glue("
  #!/bin/bash
  #SBATCH --time {t}
  #SBATCH --ntasks {cores}
  #SBATCH --mem {mem}
  #SBATCH -o {out}/{name}-%j.out
  ",)
}

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

  stopifnot(!missing(name))

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
  if(missing(name)) name <- stringr::str_remove(fs::path_file(sumstat), ".ma")
  out <- Sys.getenv("PRS_ARCHIVE")


  header <- make_slurm_header(name = name)
  sbayes_job <- make_sbayesr_job(infile = sumstat, name = name)
  fp_job <- glue::glue(out,"/{name}_sbatch.sh")
  writeLines(c(header, sbayes_job),fp_job)
  if(sbatch) system(glue::glue("sbatch {fp_job}"))
}




