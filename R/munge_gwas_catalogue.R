utils::globalVariables(c("any_of", "freq"))

nonharmonized_ma <- c("SNP" = "variant_id",
  "A1" = "effect_allele",
  "A2" = "other_allele",
  "freq" = "effect_allele_frequency",
  "b" = "beta",
  "se" =  "standard_error",
  "p" = "p_value",
  "N" = "sample_size",
  "position" = "base_pair_location",
  "chromosome" = "chromosome")

harmonized_ma <- c("SNP" = "hm_rsid",
  "A1" = "hm_effect_allele",
  "A2" = "hm_other_allele",
  "freq" = "hm_effect_allele_frequency",
  "b" = "hm_beta",
  "se" =  "standard_error",
  "p" = "p_value",
  "N" = "sample_size",
  "position" = "hm_pos",
  "chromosome" = "hm_chrom")


select_cols <- function(df){
  if(any(stringr::str_detect(colnames(df), "^hm_"))) {
    dplyr::select(df, any_of(harmonized_ma))
    } else {
    dplyr::select(df, any_of(nonharmonized_ma))
    }

}


freq_join <- function(df, snp_freqs) {
  same <- df %>%
    dplyr::inner_join(snp_freqs, by = c("SNP", "A1" = "A1", "A2" = "A2"))

  flipped <- df %>%
    dplyr::inner_join(snp_freqs, by = c("SNP", "A1" = "A2", "A2" = "A1"))

    dplyr::bind_rows(same, flipped)
}

check_n <- function(x, n) {
  if(!"N" %in% colnames(x)) dplyr::mutate(x, N = {{ n}}) else x
}

check_freq <- function(x, snp_freq){
  if(!"freq" %in% colnames(x)) return(freq_join(x, snp_freq))

  # if there are missing values in the frequency column, use imputed freqs
  if(sum(is.na(x[['freq']])) > 0) return(freq_join(dplyr::select(x, -freq), snp_freq))

  x

}

#' Converts GWAS catalogue sumstats to the .ma format
#'
#' @param df dataframe/tibble of a gwas catalogue formatted sumstat
#' @param n sample size to be added if it is missing
#' @param snp_freq a dataframe wtih four columns: SNP A1 A2 freq
#'
#' @return a tibble
#' @export
#'
#' @examples \dontrun{
#' ebi_to_ma(df, n, snp_freq)}
ebi_to_ma <- function(df, n, snp_freq){
  df %>%
    select_cols() %>%
    check_n(n = n) %>%
    check_freq(snp_freq = snp_freq) %>%
    dplyr::select(., -any_of(c("position", "chromosome")))

}




