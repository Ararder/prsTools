

make_sbayesr_job <- function(infile, out, pi="0.95,0.02,0.02,0.01",
                             gamma="0.0,0.01,0.1,1", chain_length=10000,
                             burn_in=4000, thin=10, out_freq=10,
                             ldmatrix, gctb, name, from_env=TRUE) {

  stopifnot(!missing(name))

  # use environment to figure out filepaths for GCTB and the ld matrix
  if(from_env){
    gctb <- Sys.getenv("GCTB")
    ldmatrix <- Sys.getenv("GCTB_LDMATRIX")

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

sbayes_rescale_sumstats <- function(sumstat, name, sbatch=TRUE) {



  # Look in the environment for the
  stopifnot()
  out <- paste0(archive, "/", name)

  # create the output directory if it doesnt exist
  if(!fs::dir_exists(out)) fs::dir_create(out)

  # create the slurm job using default settings
  job <- make_sbayesr_job(
    gctb = gctb,
    ldmatrix = ldmatrix,
    infile = sumstat,
    out = out,
    name = name
  )

  if(sbatch){
    sh <- glue::glue(out,"/", "{name}.","sbatch.sh")
    writeLines(job, sh)
    system(glue::glue("sbatch {sh} --ntasks 16 --nodelist scalar4 --time 360 --mem 32000"))
  } else {
    job
  }


}
make_slurm_header <- function(t=480, cores=4, mem=10000, out="nfs/home/arvhar", name="slurm") {
  glue::glue("
  #!/bin/bash
  #SBATCH --time {t}
  #SBATCH --ntasks {cores}
  #SBATCH --mem {mem}
  #SBATCH -o {out}/{name}-%j.out
  ",)
}
run_sbayesr_job <- function(sumstat, name) {
  # if the name argument has not been passed, use filename of the sumstat
  if(missing(name)) name <- stringr::str_remove(fs::path_file(sumstat), ".ma")
  make_slurm_header(sumstat)
}

