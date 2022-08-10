#' Title
#'
#' @param bfile filepath to genotype data
#' @param score filepath to sumstat, a1 is column 2, column 5 is effect size
#' @param out output directory and filename
#' @param threads number of threads
#' @param format bgen/pgen/etc
#'
#' @return bash code
#' @export
#'
#' @examples \dontrun{
#' plink_score_default("/cohort_x/name_genotypes", "my/rescaled/gwas.ma", "/store/prs/here/name")
#' }
plink_score_default <- function(bfile, score, out, threads=16,format ="pgen"){
  glue::glue(
    "plink2 ",
    "--{format} {bfile} ",
    "--score {score} 2 5 8 ",
    "--threads {threads} ",
    "--variance-standardize ",
    "--out {out} "
  )
}

#' Title
#'
#' @param infile discovery gwas in .ma format
#' @param name prefix for what the output file should be called. .snpRes will be added
#' @param pi pi paramter in gctb
#' @param gamma paramter in gctb
#' @param chain_length paramter in gctb
#' @param burn_in paramter in gctb
#' @param thin paramter in gctb
#' @param out_freq paramter in gctb
#' @param seed paramter in gctb
#' @param out paramter in gctb
#' @param ldmatrix location of ldmatrix filepaths, defaults by looking an environment
#' @param gctb location of gctb program
#'
#' @return bash code
#' @export
#'
#' @examples \dontrun{
#' # if you have GCTB and GCTB_LDMATRIX  and prs_archivein your environemnt
#' gctb_sbayesr_default("filepath/to/my/gwas.ma", "my_name")
#' gctb_sbayesr_default("filepath/to/my/gwas.ma", "my_name",
#' out="/my/custom/output/directory")
#' }
gctb_sbayesr_default <- function(infile,
                                 name,
                                 pi="0.95,0.02,0.02,0.01",
                                 gamma="0.0,0.01,0.1,1",
                                 chain_length=10000,
                                 burn_in=4000,
                                 thin=10,
                                 out_freq=10,
                                 seed=2022,
                                 out=Sys.getenv("PRS_ARCHIVE"),
                                 ldmatrix=Sys.getenv("GCTB_LDMATRIX"),
                                 gctb=Sys.getenv("GCTB")
) {


  glue::glue(

    "{gctb} ",
    "--sbayes R ",
    "--mldm {ldmatrix} ",
    "--gwas-summary {infile} ",
    "--pi {pi} ",
    "--gamma {gamma} ",

    "--robust ",
    "--ambiguous-snp ",
    "--impute-n ",

    "--seed {seed}",
    "--chain-length {chain_length} ",
    "--burn-in {burn_in} ",
    "--thin {thin} ",
    "--out-freq {out_freq} ",
    "--no-mcmc-bin ",

    "--out {out}/{name}"
  )
}
