
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sifr: search in files

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/s-fleck/sifr.svg?branch=master)](https://travis-ci.org/s-fleck/sifr)
<!-- badges: end -->

sifr is a tool for searching text strings and regular expression
patterns recursively in directory trees from within R. It supports
colored console output, RStudio source markers, and can also return the
search results as a `data.frame` for further processing. In addition,
sifr provides `sed_replace()` on supported platforms (such as Linux,
BSD, macOS) for replacing regex patterns across directory trees via
[sed](https://en.wikipedia.org/wiki/Sed).

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/sifr")
```

## Example

``` r
library(sifr)

tf <- tempfile(fileext = ".csv")
write.csv(cars, tf)

x <- sif("26", dir = dirname(tf), markers = FALSE, path_pattern = ".*\\.csv$")
print(x)
#> Results for 26 
#> 
#> /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv
#>  9   "8",10,26 
#> 17   "16",13,26 
#> 21   "20",14,26 
#> 26   "25",15,26 
#> 27   "26",15,54
as.data.frame(x)
#>                                   path ln   pos   contents
#> 1 /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv  9  8, 9  "8",10,26
#> 2 /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv 17 9, 10 "16",13,26
#> 3 /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv 21 9, 10 "20",14,26
#> 4 /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv 26 9, 10 "25",15,26
#> 5 /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv 27  2, 3 "26",15,54


sed_replace("26", "twentysix", dir = dirname(tf), path_pattern = ".*\\.csv$")
#> Results for 26 
#> 
#> /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv
#>  9   "8",10,26 
#> 17   "16",13,26 
#> 21   "20",14,26 
#> 26   "25",15,26 
#> 27   "26",15,54
sif("26", dir = dirname(tf), markers = FALSE, path_pattern = ".*\\.csv$")
#> No files found in '/tmp/Rtmp3FeKjv' that containt the regex pattern '26'
sif("twentysix", dir = dirname(tf), markers = FALSE, path_pattern = ".*\\.csv$")
#> Results for twentysix 
#> 
#> /tmp/Rtmp3FeKjv/file4b5a3a81c1c4.csv
#>  9   "8",10,twentysix 
#> 17   "16",13,twentysix 
#> 21   "20",14,twentysix 
#> 26   "25",15,twentysix 
#> 27   "twentysix",15,54
unlink(tf)
```
