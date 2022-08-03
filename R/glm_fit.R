utils::globalVariables(c("nobs", "logLik", ".data",
                         "n", ".", "estimate", "conf.low", "conf.high", "p.value"))
cox_snell_r2 <- function(LL_base, LL_full, N) {
  round(1 - exp((2 / N) * (LL_base[1] - LL_full[1])), 6)
}

nagelkerke_r2 <- function(cox_snell_r2, LL_base, N) {
  round(cox_snell_r2 / (1 - exp((2 / N) * LL_base[1])), 6)
}

#' Generate statistical metrics from a logistic regression using polygenic risk scores and case status
#'
#' @param df a dataframe with prs, case status and covariates
#' @param base_formula a glm formula of the logistic regression with only covariates
#' @param formula a glm formula for the full regression (case status on prs and covariates)
#' @param pop_prev optional, for computing R2 on the liability scale.
#'
#' @return a tibble
#' @export
#'
#' @examples \dontrun{
#' # OBS: in the dataframe passed, outcome variable needs to be named case,
#' # and PRS column needs to be named score
#' base <- as.formula(case ~ PC1 + PC2 + sex)
#' full <- as.formula(case ~ score + PC1 + PC2 + sex)
#' glm_prs_metrics(df, base, full, pop_prev = 0.01)
#' }
glm_prs_metrics <- function(df, base_formula, formula, pop_prev) {
  # if formulas, assume
  if(missing(base_formula)) base_formula <- stats::as.formula("case ~ PC1 + PC2 + PC3 + PC4 + PC5")
  if(missing(formula))      formula      <- stats::as.formula("case ~ score + PC1 + PC2 + PC3 + PC4 + PC5")

  stopifnot(!any(c(missing(base_formula), missing(formula), missing(df))))

  base_model <- stats::glm(base_formula, family = "binomial", data = df)
  full_model <- stats::glm(formula, family = "binomial", data = df)


  N <- dplyr::pull(broom::glance(base_model), nobs)
  ll_base <- dplyr::pull(broom::glance(base_model), logLik)
  ll_full <- dplyr::pull(broom::glance(full_model), logLik)

  cs_r2 <-  cox_snell_r2(ll_base, ll_full, N)
  nagel_r2 <- nagelkerke_r2(cs_r2, ll_base, N)
  prop_cases <-
    nrow(dplyr::filter(df,  .data[["case"]] == 1)) / (nrow(dplyr::filter(df,  .data[["case"]] == 1)) + nrow(dplyr::filter(df,  .data[["case"]] == 0)))

  if(!missing(pop_prev)) lia_r2 <- liability_scale_r2({{ pop_prev }}, nagel_r2, prop_cases) else lia_r2 <- NA


  probs <- stats::predict(full_model, type = "response")
  auc_val <- as.numeric(pROC::auc(df[["case"]], probs))

  # number of cases and controls
  ncas <- dplyr::tibble(dplyr::count(df, .data[["case"]])) %>% dplyr::pull(n) %>% .[2]
  ncon <- dplyr::tibble(dplyr::count(df, .data[["case"]])) %>% dplyr:: pull(n) %>% .[1]

  odds_ratio <- full_model %>%
    broom::tidy(exponentiate=TRUE, conf.int=TRUE) %>%
    dplyr::filter(.data[["term"]] =="score") %>%
    dplyr::select(estimate, conf.low, conf.high, p.value)

  # merge metrics
  dplyr::tibble(N = N, ncas = ncas, ncon = ncon, cox = cs_r2,
         nagelkerke = nagel_r2, nagel_lia = lia_r2, auc = auc_val) %>%
    dplyr::bind_cols(odds_ratio)




}








