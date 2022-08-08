utils::globalVariables(c("any_of", "freq", "chromosome", "position", "p2"))

utils::globalVariables(c("SNP", "A1", "A2", "b","se" ,"p" , "N"))


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


#' Select columns from gwas catalogue, to convert to .ma format from GCTA
#'
#' @param df dataframe downloaded from GWAS catalogue
#'
#' @return a tibble
#' @export
#'
#' @examples \dontrun{
#' select_cols(df)
#' }
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
    dplyr::select(SNP, A1, A2, b, freq, b, se, p, N)

}


get_top_mhc <- function(df, build = 38, pval = "p"){

  if(build == 37){
    mhc_start <- 28477797
    mhc_end <- 33448354

  } else if(build == 38){
    mhc_start <- 28510120
    mhc_end <- 33480577


  }

  mhc <- df %>%
    dplyr::filter(chromosome == 6 & position >= mhc_start & position <= mhc_end)

  if(is.character(df[["p"]])) {
    message("The p-value column is a character vector,
          this could mean P-values have are so small that they cannot be represented as doubles")
    message("extracting the 3 last elements of the p-value string to determine smallest p-value")

    mhc %>%
      dplyr::mutate(p2 = as.numeric(.data[[pval]])) %>%
      dplyr::filter(p2 == min(p2)) %>%
      dplyr::mutate(p2 = stringr::str_sub(.data[[pval]], start = -3)) %>%
      dplyr::filter(p2 == max(p2))

  } else if(is.numeric(df[["p"]])) {

    mhc %>%
      dplyr::filter(.data[[pval]] == min(.data[[pval]]))
  }


}




