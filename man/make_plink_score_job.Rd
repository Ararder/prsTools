% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slurmR.R
\name{make_plink_score_job}
\alias{make_plink_score_job}
\title{Title}
\usage{
make_plink_score_job(bfile, score, out, threads = 4, name)
}
\arguments{
\item{bfile}{filepath to genetic bed bim fam genetic data}

\item{score}{filepath to sumstat file to score}

\item{out}{filepath to out directory}

\item{threads}{number of threads}

\item{name}{prefix for file names.}
}
\value{
a string
}
\description{
Title
}
\examples{
make_plink_score_job(bfile = "GENODATA/bedbimfam", score = "mdd_rescaled.snpRes", "my_dir/results", name = "mdd2021")

}
