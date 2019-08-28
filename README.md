
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sifr: search in files

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

sifr is a tool for searching text strings and regular expression
patterns recursively in directory trees from within R. It supports
coloured console output, RStudio source markers, and can also return the
search results as a `data.frame` for further processing. In addtion,
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
#> /tmp/RtmpFEuxRk/file6af53c174426.csv
#>  9   "8",10,26 
#> 17   "16",13,26 
#> 21   "20",14,26 
#> 26   "25",15,26 
#> 27   "26",15,54
as.data.frame(x)
#>                                   path ln   pos   contents
#> 1 /tmp/RtmpFEuxRk/file6af53c174426.csv  9  8, 9  "8",10,26
#> 2 /tmp/RtmpFEuxRk/file6af53c174426.csv 17 9, 10 "16",13,26
#> 3 /tmp/RtmpFEuxRk/file6af53c174426.csv 21 9, 10 "20",14,26
#> 4 /tmp/RtmpFEuxRk/file6af53c174426.csv 26 9, 10 "25",15,26
#> 5 /tmp/RtmpFEuxRk/file6af53c174426.csv 27  2, 3 "26",15,54
```
