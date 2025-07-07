
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ship of Theseus

<!-- badges: start -->

<!-- badges: end -->

The goal of Theseus is to …

## Installation

You can install the development version of Theseus like so:

``` r
remotes::install_github("hoxo-m/Theseus")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
library(nycflights13)

data <- flights |> 
  filter(!is.na(dep_delay)) |>
  mutate(on_time = dep_delay <= 0)  # on time

data |> select(year, month, day, origin, dest, carrier, on_time) |> head()
#> # A tibble: 6 × 7
#>    year month   day origin dest  carrier on_time
#>   <int> <int> <int> <chr>  <chr> <chr>   <lgl>  
#> 1  2013     1     1 EWR    IAH   UA      FALSE  
#> 2  2013     1     1 LGA    IAH   UA      FALSE  
#> 3  2013     1     1 JFK    MIA   AA      FALSE  
#> 4  2013     1     1 JFK    BQN   B6      TRUE   
#> 5  2013     1     1 LGA    ATL   DL      TRUE   
#> 6  2013     1     1 EWR    ORD   UA      TRUE

data1 <- data |> filter(month == 9L)
data2 <- data |> filter(month == 12L)

data1 |> summarise(on_time_rate = mean(on_time)) |> pull(on_time_rate)
#> [1] 0.7118575
data2 |> summarise(on_time_rate = mean(on_time)) |> pull(on_time_rate)
#> [1] 0.5001844
```

``` r
library(Theseus)

ship <- create_ship(data1, data2, on_time, labels = c("2013-09", "2013-12"))

ship$table(origin)
#> # A tibble: 3 × 8
#>   origin contrib    n1    n2    x1    x2 rate1 rate2
#>   <chr>    <dbl> <int> <int> <int> <int> <dbl> <dbl>
#> 1 EWR    -0.0953  9407  9445  6457  3895 0.686 0.412
#> 2 JFK    -0.0589  8816  8963  6167  4659 0.700 0.520
#> 3 LGA    -0.0574  8899  8702  6683  5006 0.751 0.575
```

``` r
ship$plot(origin)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="500" />

``` r
ship$plot_flip(carrier)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="500" />
