
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prsTools

<!-- badges: start -->
<!-- badges: end -->

The goal of prsTools is to …

## Installation

The prsTools R package is installed by using the install_github command

``` r
library(devtools)
#> Loading required package: usethis
# install_github("ararder/prsTools")
```

## Getting the metrics

The most common (and basic) form of PRS analysis is to use the PRS to
predict an outcome y, in a regression framework. In R, the most common
use is to use the glm function.

In many cases, one wants to fit many different variations of the same
model, with different covariates PRS:es.

glm_prs_metrics provided a framework for doing this programmatically,
and for extracting relevant metrics.

``` r
library(prsTools)
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
#> ✔ tibble  3.1.8      ✔ dplyr   1.0.10
#> ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
#> ✔ readr   2.1.2      ✔ forcats 0.5.2 
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

test_df <- tibble(
  disease_status = sample(c(0,1), 10, replace=TRUE),
  prs_1 = rnorm(10),
  prs_2 = rnorm(10),
  pc1 = rnorm(10),
  age = sample(seq(30,60), 10, replace=TRUE),
)
```

The base formula is used for calculating R2 in logistic regression. and
is used as a baseline comparison

``` r
base_formula <- as.formula("disease_status ~ pc1")
full_formula <- as.formula("disease_status ~ prs_1 + pc1")

# get commonly used metrics to evaluate the PRS. The model defaults to returning the odds-ratio/beta for the first predictor
# in this case prs_1
glm_prs_metrics(df = test_df, base_formula = base_formula, formula = full_formula)
#> using variable disease_status as y,
#> using variable prs_1 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> # A tibble: 1 × 12
#>       N  ncas  ncon   cox nagelkerke nagel…¹   auc term  estim…² std.e…³ stati…⁴
#>   <int> <int> <int> <dbl>      <dbl> <lgl>   <dbl> <chr>   <dbl>   <dbl>   <dbl>
#> 1    10     4     6 0.102      0.143 NA      0.792 prs_1   -1.12    1.23  -0.909
#> # … with 1 more variable: p.value <dbl>, and abbreviated variable names
#> #   ¹​nagel_lia, ²​estimate, ³​std.error, ⁴​statistic
```

For case-control phenotypes, we need to put the R2 measurement on the
liabiltiy scale to be able to compare against cohorts with different
case/control ratios for this, we need to populatiom prevalence of the
disease. here i put it to 20%

``` r
glm_prs_metrics(df = test_df, base_formula = base_formula, formula = full_formula, pop_prev = 0.2)
#> using variable disease_status as y,
#> using variable prs_1 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> # A tibble: 1 × 12
#>       N  ncas  ncon   cox nagelkerke nagel…¹   auc term  estim…² std.e…³ stati…⁴
#>   <int> <int> <int> <dbl>      <dbl>   <dbl> <dbl> <chr>   <dbl>   <dbl>   <dbl>
#> 1    10     4     6 0.102      0.143   0.148 0.792 prs_1   -1.12    1.23  -0.909
#> # … with 1 more variable: p.value <dbl>, and abbreviated variable names
#> #   ¹​nagel_lia, ²​estimate, ³​std.error, ⁴​statistic
```

we can return odds-ratio by setting the argument odds_ratio = TRUE
instead of the beta this is helpful for generating confidence intervals
on the odds-ratio scale

``` r
glm_prs_metrics(df = test_df, base_formula = base_formula, formula = full_formula, pop_prev = 0.2, odds_ratio = TRUE)
#> using variable disease_status as y,
#> using variable prs_1 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> # A tibble: 1 × 14
#>       N  ncas  ncon   cox nagelkerke nagel…¹   auc term  estim…² std.e…³ stati…⁴
#>   <int> <int> <int> <dbl>      <dbl>   <dbl> <dbl> <chr>   <dbl>   <dbl>   <dbl>
#> 1    10     4     6 0.102      0.143   0.148 0.792 prs_1   0.326    1.23  -0.909
#> # … with 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>, and
#> #   abbreviated variable names ¹​nagel_lia, ²​estimate, ³​std.error, ⁴​statistic
```

Often you want to run lots of possible variations of this model.

``` r
formula_list <- list(
  full_formula,
  as.formula("disease_status ~ prs_1 + prs_2 + pc1"),
  as.formula("disease_status ~ prs_1 + prs_2 + pc1 + age"),
  as.formula("disease_status ~ prs_2 + pc1 + age")
)

# it's now programmatically easy to compare these different models.
map_df(formula_list, glm_prs_metrics, df = test_df, base_formula = base_formula, pop_prev = 0.2)
#> using variable disease_status as y,
#> using variable prs_1 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> using variable disease_status as y,
#> using variable prs_1 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> using variable disease_status as y,
#> using variable prs_1 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> using variable disease_status as y,
#> using variable prs_2 as PRS column
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> # A tibble: 4 × 12
#>       N  ncas  ncon    cox nagelke…¹ nagel…²   auc term  estim…³ std.e…⁴ stati…⁵
#>   <int> <int> <int>  <dbl>     <dbl>   <dbl> <dbl> <chr>   <dbl>   <dbl>   <dbl>
#> 1    10     4     6 0.102     0.143   0.148  0.792 prs_1  -1.12     1.23  -0.909
#> 2    10     4     6 0.105     0.147   0.152  0.792 prs_1  -1.23     1.47  -0.836
#> 3    10     4     6 0.315     0.443   0.483  0.792 prs_1  -3.18     3.00  -1.06 
#> 4    10     4     6 0.0491    0.0690  0.0703 0.625 prs_2  -0.696    1.52  -0.457
#> # … with 1 more variable: p.value <dbl>, and abbreviated variable names
#> #   ¹​nagelkerke, ²​nagel_lia, ³​estimate, ⁴​std.error, ⁵​statistic
```

# Commands for Slurm

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
