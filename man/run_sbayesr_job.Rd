% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slurmR.R
\name{run_sbayesr_job}
\alias{run_sbayesr_job}
\title{Title}
\usage{
run_sbayesr_job(sumstat, name, sbatch = TRUE)
}
\arguments{
\item{sumstat}{filepath to sumstat to rescale using sbayesr}

\item{name}{prefix name for output files}

\item{sbatch}{should the script be sbatched?}
}
\value{
write a text file to prs archive
}
\description{
Title
}
\examples{
\dontrun{
run_sbayesr_job("path/to/my_sumstats.ma")
}
}
