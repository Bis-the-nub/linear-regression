Linear Regression
================
Rene Chi
2025-11-06

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
```

Load the NYC data

``` r
data("nyc_airbnb")
```

Look at the data / do some cleaning

``` r
nyc_airbnb =
  nyc_airbnb |> 
  mutate(
    stars = review_scores_location / 2
         ) |> 
  rename(
    borough = neighbourhood_group
  ) |> 
  filter(borough != "Staten Island") |> 
  select(price, stars, borough, room_type, neighbourhood)
```

Do regression!!

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

do some additional cleaning then fit

``` r
nyc_airbnb = 
  nyc_airbnb |> 
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type))

fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

Look at `lm` stuff

``` r
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       19.839     12.189   1.628    0.104    
    ## stars             31.990      2.527  12.657   <2e-16 ***
    ## boroughBrooklyn  -49.754      2.235 -22.262   <2e-16 ***
    ## boroughQueens    -77.048      3.727 -20.675   <2e-16 ***
    ## boroughBronx     -90.254      8.567 -10.534   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

Look at cleaner `lm` stuff

``` r
fit |> 
  broom::tidy() |> 
  select(term, estimate, p.value) |> 
  mutate(term = str_replace(term, "borough", "Borough: ")) |> 
  knitr::kable(digits = 3)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |   19.839 |   0.104 |
| stars             |   31.990 |   0.000 |
| Borough: Brooklyn |  -49.754 |   0.000 |
| Borough: Queens   |  -77.048 |   0.000 |
| Borough: Bronx    |  -90.254 |   0.000 |

# Diagnostics

Look at residuals.

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  modelr::add_predictions(fit)
```

    ## # A tibble: 40,492 × 7
    ##    price stars borough room_type       neighbourhood  resid  pred
    ##    <dbl> <dbl> <fct>   <fct>           <chr>          <dbl> <dbl>
    ##  1    99   5   Bronx   Private room    City Island     9.47  89.5
    ##  2   200  NA   Bronx   Private room    City Island    NA     NA  
    ##  3   300  NA   Bronx   Entire home/apt City Island    NA     NA  
    ##  4   125   5   Bronx   Entire home/apt City Island    35.5   89.5
    ##  5    69   5   Bronx   Private room    City Island   -20.5   89.5
    ##  6   125   5   Bronx   Entire home/apt City Island    35.5   89.5
    ##  7    85   5   Bronx   Entire home/apt City Island    -4.53  89.5
    ##  8    39   4.5 Bronx   Private room    Allerton      -34.5   73.5
    ##  9    95   5   Bronx   Entire home/apt Allerton        5.47  89.5
    ## 10   125   4.5 Bronx   Entire home/apt Allerton       51.5   73.5
    ## # ℹ 40,482 more rows

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  modelr::add_predictions(fit) |> 
  filter(resid < 1000) |> 
  ggplot(aes(x = stars, y = resid)) +
  geom_point()
```

![](linear-regression_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  modelr::add_predictions(fit) |> 
  filter(resid < 1000) |> 
  ggplot(aes(x = borough, y = resid)) +
  geom_violin()
```

![](linear-regression_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

## Hypothesis test

``` r
fit_null = lm(price ~ stars + borough, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough + room_type, data = nyc_airbnb)

anova(fit_null, fit_alt) |> 
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30525 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30523 9.21e8     2  8.42e7     1394.       0

## interactions vs nested data

``` r
fit_interactions =
  lm(price ~ stars * borough + room_type * borough, data = nyc_airbnb)

fit_interactions |> 
broom::tidy()
```

    ## # A tibble: 16 × 5
    ##    term                                  estimate std.error statistic  p.value
    ##    <chr>                                    <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                              95.7      19.2     4.99   6.13e- 7
    ##  2 stars                                    27.1       3.96    6.84   8.20e-12
    ##  3 boroughBrooklyn                         -26.1      25.1    -1.04   2.99e- 1
    ##  4 boroughQueens                            -4.12     40.7    -0.101  9.19e- 1
    ##  5 boroughBronx                             -5.63     77.8    -0.0723 9.42e- 1
    ##  6 room_typePrivate room                  -124.        3.00  -41.5    0       
    ##  7 room_typeShared room                   -154.        8.69  -17.7    1.42e-69
    ##  8 stars:boroughBrooklyn                    -6.14      5.24   -1.17   2.41e- 1
    ##  9 stars:boroughQueens                     -17.5       8.54   -2.04   4.09e- 2
    ## 10 stars:boroughBronx                      -22.7      17.1    -1.33   1.85e- 1
    ## 11 boroughBrooklyn:room_typePrivate room    32.0       4.33    7.39   1.55e-13
    ## 12 boroughQueens:room_typePrivate room      54.9       7.46    7.37   1.81e-13
    ## 13 boroughBronx:room_typePrivate room       71.3      18.0     3.96   7.54e- 5
    ## 14 boroughBrooklyn:room_typeShared room     47.8      13.9     3.44   5.83e- 4
    ## 15 boroughQueens:room_typeShared room       58.7      17.9     3.28   1.05e- 3
    ## 16 boroughBronx:room_typeShared room        83.1      42.5     1.96   5.03e- 2

Lets just look at brooklyn first

``` r
nyc_airbnb |> 
  filter(borough == "Brooklyn") |> 
  lm(price ~ stars + room_type, data = _) |> 
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               69.6     14.0       4.96 7.27e-  7
    ## 2 stars                     21.0      2.98      7.05 1.90e- 12
    ## 3 room_typePrivate room    -92.2      2.72    -34.0  6.40e-242
    ## 4 room_typeShared room    -106.       9.43    -11.2  4.15e- 29

Write a short function

``` r
lm_airbnb = function(df){
  lm(price ~ stars + room_type, data = df)
}

nyc_airbnb |> 
  filter(borough == "Queens") |> 
  lm_airbnb() |> 
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              91.6      25.8       3.54 4.00e- 4
    ## 2 stars                     9.65      5.45      1.77 7.65e- 2
    ## 3 room_typePrivate room   -69.3       4.92    -14.1  1.48e-43
    ## 4 room_typeShared room    -95.0      11.3      -8.43 5.52e-17

Create a list of dataframes, and iterate to fit the model each time.

``` r
nested_lm_results =
  nyc_airbnb |> 
  nest(data = -borough) |> 
  mutate(
    fits = map(data, lm_airbnb),
    results = map(fits, broom::tidy)
  ) |> 
  select(borough, results) |> 
  unnest(results)
```

Do some untidying

``` r
nested_lm_results |> 
  select(borough, term, estimate) |> 
  pivot_wider(
    names_from = term,
    values_from = estimate
  )
```

    ## # A tibble: 4 × 5
    ##   borough   `(Intercept)` stars `room_typePrivate room` `room_typeShared room`
    ##   <fct>             <dbl> <dbl>                   <dbl>                  <dbl>
    ## 1 Bronx              90.1  4.45                   -52.9                  -70.5
    ## 2 Queens             91.6  9.65                   -69.3                  -95.0
    ## 3 Brooklyn           69.6 21.0                    -92.2                 -106. 
    ## 4 Manhattan          95.7 27.1                   -124.                  -154.

Use an *anonymous* function instead of `lm_airbnb`

``` r
nested_lm_results =
  nyc_airbnb |> 
  nest(data = -borough) |> 
  mutate(
    fits = map(data, \(df) lm(price ~ stars + room_type, data = df)),
    results = map(fits, broom::tidy)
    ) |> 
  select(borough, results) |> 
  unnest(results)
```

Lets do a more extreme example!

``` r
manhattan_analysis =
  nyc_airbnb |> 
  filter(
    borough == "Manhattan"
  ) |> 
  nest(data = -neighbourhood) |> 
  mutate(
    fits = map(data, \(df) lm(price ~ stars + room_type, data = df)),
    results = map(fits, broom::tidy)    
  ) |> 
  select(neighbourhood, results) |> 
  unnest(results)  
```

Make a plot

``` r
manhattan_analysis |> 
  filter(term == "stars") |> 
  mutate(
    neighbourhood = fct_reorder(neighbourhood, estimate)
  ) |> 
  ggplot(aes(x = neighbourhood, y = estimate)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](linear-regression_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
