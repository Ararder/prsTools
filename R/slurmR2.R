#' Title
#'
#' @param ... name-value pairs of arguments
#' @param slurm use a slurm prefix or not?
#'
#' @return a string
#' @export
#'
#' @examples \dontrun{
#' to_bash("bfile" = "my_genetic/data")
#' }
to_bash <- function(..., slurm=FALSE) {
  args <- list(...)
  names <- names(args)
  stopifnot(is.logical(slurm))
  stopifnot(length(args) >= 1L)

  if(slurm){
    c("#!/bin/bash", purrr::map2_chr(names,args, ~paste0("SBATCH ", .x, "=", .y)))
  } else {
    purrr::map2_chr(names,args, ~paste0("--", .x, " ", .y)) |>
      stringr::str_flatten(collapse = " ") |>
      stringr::str_replace_all(string = _, "  ", " ")

  }

}

#' Construct templates for commandline clals
#'
#' @param program filepath of program to run
#' @param default_args a list of arguments, in the form of list("arg_name" = arg)
#'
#' @return a string
#' @export
#'
#' @examples \dontrun{
#' construct_template("/nfs/home/ldsc/ldsc.py", list("hm3" = "/home/arvhar/hapmap3.tsv"))
#' }
construct_template <- function(program, default_args) {
  parsed_args <- do.call(to_bash, default_args)

  function(...) {
    res <- to_bash(...)
    paste0(program," ", res," ", parsed_args)
    # list(program, res, )
  }

}

# sbayes_default_args <- list(
#   ldmatrix=Sys.getenv("GCTB_LDMATRIX"),
#   pi="0.95,0.02,0.02,0.01",
#   gamma="0.0,0.01,0.1,1",
#   robust="",
#   "ambiguous-snp"="",
#   "impute-n"="",
#   "chain-length"=10000,
#   "burn-in"=4000,
#   thin=10,
#   "out-freq"=10,
#   seed=2022
# )
#
# plink_default_args <- list(
#   "threads" = 16,
#   "variance-standardize" = ""
# )
#
# plink_score <- construct_template("plink2", plink_default_args)
#
#
# plink_score(
#   bfile = "/nfs/projects/S3/genotypes",
#   score = "/arvhar/sumstats.snpRes",
#   out = "/home/folder/for/prs"
# )
