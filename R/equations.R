#' Title Expected R2 in PRS.
#'  Calculates the expected R2 in out of sample prediction.
#'
#' @param h SNP heritability of phenotype
#' @param m Number of independant SNPs used for predictor. Wray et al (2018) suggests
#' using m = 50000, and this is therefore the default parameter
#' @param n Sample size of discovery GWAS
#'
#' @return a float (0 -> 1)
#' @export
#'
#' @examples
#' # the latest SCZ gwas has a effecive sample size of ~110k, and h2 of 0.24
#' # what do we expect the out of sample r2 to be?
#' e_r2(h = 0.24, m=50000, n = 110000)
#'
e_r2 <- function(h, m=50000, n){
  stopifnot(h > 0 & h <= 1)
  stopifnot(m >= 1)
  stopifnot(n >= 1)
  loss <- 1+(m/(h*n))
  temp <- h*n
  temp1 <- m/temp
  h/loss

}

#' Title Calculate the effective sample size
#'
#' @param cases number of affected individuals
#' @param controls number of controls
#'
#' @return a float
#' @export
#'
#' @examples
#' n_eff <- neff(25000, 75000)
neff <- function(cases, controls) {
  stopifnot(cases >= 1)
  stopifnot(controls >= 1)
  proportion <- cases/ (cases + controls)
  neff <- 4*proportion*((1-proportion)*(cases + controls))
  return(neff)
}

#' Title
#'
#' @param pop_prev prevalence of the disorder in the general population
#' @param sample_prev the prevalence of the disorder in the sample
#' @param h2 observed-scale heritability
#'
#' @return a float
#' @export
#'
#' @examples
#' liability_scale_h2(0.1, 0.5, 0.27)
liability_scale_h2 <- function(pop_prev,sample_prev, h2) {
  K <- pop_prev
  P <- sample_prev
  zv <- stats::dnorm(stats::qnorm(K))

  h2 * K^2 * ( 1 - K)^2 / P / (1-P) / zv^2

}

#' Convert a R2 estimate from observec scale to liability scale
#'
#' @param k population disease risk
#' @param r2 observed scale R2
#' @param p proportion of cases
#'
#' @return a float
#' @export
#'
#' @examples
#' liability_scale_r2(0.1, 0.15, 0.4)
liability_scale_r2 <- function(k, r2, p) {

  x <- stats::qnorm(1 - k)
  z <- stats::dnorm(x)
  i <- z / k
  C <- k * (1 - k) * k * (1 - k) / (z^2 * p * (1 - p))
  theta <- i * ((p - k) / (1 - k)) * (i * ((p - k) / (1 - k)) - x)
  e <- 1 - p^(2 * p) * (1 - p)^(2 * (1 - p))
  h2l_NKr2 <- C * e * r2 / (1 + C * e * theta * r2)
  h2l_NKr2 <- round(h2l_NKr2, 6)
  return(h2l_NKr2)
}
