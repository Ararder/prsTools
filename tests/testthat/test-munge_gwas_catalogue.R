
df <- readr::read_tsv(test_path("data", "asthma_gwas.tsv"))
missing_snps <- df %>% dplyr::mutate(freq = sample(c(0.01, NA_real_),size = 100, replace = TRUE))
snp_freq <- readr::read_tsv(test_path("data" ,"snp_freq.tsv"))



test_that("Adds sample size if sample size is missing", {
  df_w_n <-  dplyr::mutate(df,N = 15)

  # Check that N is correctly added
  expect_true("N" %in% colnames(check_n(df, 70000)))
  expect_true(check_n(df, 70000)[["N"]][1] == 70000)

  # Check that N is not overwritten if it does exist
  expect_true(check_n(df_w_n, 70000)[["N"]][1] == 15)
})

test_that("Common gotchas for frequency",{
  testframe <- check_freq(select_cols(missing_snps), snp_freq)
  testframe2 <- check_freq(dplyr::select(select_cols(df), -freq), snp_freq)

  # allele frequency is imputed if there are missing rows for frequency
  expect_true(dplyr::summarise(testframe,sum(is.na(freq))) == 0)

  # frequency is imputed if its missing from the columns
  expect_true("freq" %in% colnames(testframe2))
})


