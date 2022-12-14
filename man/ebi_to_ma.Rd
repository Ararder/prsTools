% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/munge_gwas_catalogue.R
\name{ebi_to_ma}
\alias{ebi_to_ma}
\title{Converts GWAS catalogue sumstats to the .ma format}
\usage{
ebi_to_ma(df, n, snp_freq)
}
\arguments{
\item{df}{dataframe/tibble of a gwas catalogue formatted sumstat}

\item{n}{sample size to be added if it is missing}

\item{snp_freq}{a dataframe wtih four columns: SNP A1 A2 freq}
}
\value{
a tibble
}
\description{
Converts GWAS catalogue sumstats to the .ma format
}
\examples{
\dontrun{
ebi_to_ma(df, n, snp_freq)}
}
