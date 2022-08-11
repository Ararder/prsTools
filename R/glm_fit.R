utils::globalVariables(c("nobs", "logLik", ".data",
                         "n", ".", "estimate", "conf.low", "conf.high", "p.value"))
cox_snell_r2 <- function(LL_base, LL_full, N) {
  round(1 - exp((2 / N) * (LL_base[1] - LL_full[1])), 6)
}

nagelkerke_r2 <- function(cox_snell_r2, LL_base, N) {
  round(cox_snell_r2 / (1 - exp((2 / N) * LL_base[1])), 6)
}

get_dependant_and_predictor <- function(formula) {
  string <- as.character(formula)

  # remove the ~
  tilde <- stringr::str_detect(string, "~", negate = TRUE)

  dependant_variable <- string[tilde][1]
  # split character vector into each predictor
  split <- strsplit(string[tilde][2], " ")[[1]]
  # remove +
  split <- split[stringr::str_detect(split, "\\+", negate = TRUE)]

  list(y = dependant_variable, score = split[1], X = split)
}


#' Generate statistical metrics from a logistic regression using polygenic risk scores and case status
#'
#' @param df a dataframe with prs, case status and covariates
#' @param base_formula a glm formula of the logistic regression with only covariates
#' @param formula a glm formula for the full regression (case status on prs and covariates)
#' @param pop_prev optional, for computing R2 on the liability scale.
#' @param odds_ratio should the estimates be convert to odds_ratio?
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
glm_prs_metrics <- function(df, base_formula, formula, pop_prev, odds_ratio=FALSE) {
  
  # require base formula, formula and DF
  stopifnot(!missing(base_formula))
  stopifnot(!missing(formula))
  stopifnot(!missing(df))
  case <- get_dependant_and_predictor(formula)[["y"]]
  prs <- get_dependant_and_predictor(formula)[['X']][1]

  message(glue::glue(
    "using variable {case} as y,
     using variable {prs} as PRS column"
  ))

  # check that case y column is only 1 or zeroes
  stopifnot("y should be a vector of 1 and 0" = sum(df[[case]] == 1 | df[[case]] == 0) == nrow(df))
  # check that df variable is not missing


  base_model <- stats::glm(base_formula, family = "binomial", data = df)
  full_model <- stats::glm(formula, family = "binomial", data = df)


  N <- dplyr::pull(broom::glance(base_model), nobs)
  ll_base <- dplyr::pull(broom::glance(base_model), logLik)
  ll_full <- dplyr::pull(broom::glance(full_model), logLik)

  cs_r2 <-  cox_snell_r2(ll_base, ll_full, N)
  nagel_r2 <- nagelkerke_r2(cs_r2, ll_base, N)
  prop_cases <-
    nrow(dplyr::filter(df,  .data[[case]] == 1)) / (nrow(dplyr::filter(df,  .data[[case]] == 1)) + nrow(dplyr::filter(df,  .data[[case]] == 0)))

  if(!missing(pop_prev)) lia_r2 <- liability_scale_r2({{ pop_prev }}, nagel_r2, prop_cases) else lia_r2 <- NA


  probs <- stats::predict(full_model, type = "response")
  auc_val <- as.numeric(pROC::auc(df[[case]], probs))

  # number of cases and controls
  ncas <- dplyr::tibble(dplyr::count(df, .data[[case]])) %>% dplyr::pull(n) %>% .[2]
  ncon <- dplyr::tibble(dplyr::count(df, .data[[case]])) %>% dplyr:: pull(n) %>% .[1]


  if(odds_ratio) {
    estimates <- broom::tidy(full_model,exponentiate=TRUE, conf.int=TRUE)
  } else {
    estimates <- broom::tidy(full_model)
  }

  estimates <- estimates %>%
    dplyr::filter(.data[["term"]] == {{ prs }})

  # merge metrics
  dplyr::tibble(N = N, ncas = ncas, ncon = ncon, cox = cs_r2,
         nagelkerke = nagel_r2, nagel_lia = lia_r2, auc = auc_val) %>%
    dplyr::bind_cols(estimates)




}









