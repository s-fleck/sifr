
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sifr: search in files

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
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
#> C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv
#>  9   "8",10,26 
#> 17   "16",13,26 
#> 21   "20",14,26 
#> 26   "25",15,26 
#> 27   "26",15,54
as.data.frame(x)
#>                                                                      path ln
#> 1 C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv  9
#> 2 C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv 17
#> 3 C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv 21
#> 4 C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv 26
#> 5 C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv 27
#>     pos   contents
#> 1  8, 9  "8",10,26
#> 2 9, 10 "16",13,26
#> 3 9, 10 "20",14,26
#> 4 9, 10 "25",15,26
#> 5  2, 3 "26",15,54


sed_replace("26", "twentysix", dir = dirname(tf), path_pattern = ".*\\.csv$")
#> Warning in system2("sed", sprintf("-i 's/%s/%s/%s' %s", pattern, replace, :
#> '"sed"' not found

#> Warning in system2("sed", sprintf("-i 's/%s/%s/%s' %s", pattern, replace, :
#> '"sed"' not found

#> Warning in system2("sed", sprintf("-i 's/%s/%s/%s' %s", pattern, replace, :
#> '"sed"' not found

#> Warning in system2("sed", sprintf("-i 's/%s/%s/%s' %s", pattern, replace, :
#> '"sed"' not found

#> Warning in system2("sed", sprintf("-i 's/%s/%s/%s' %s", pattern, replace, :
#> '"sed"' not found
#> Results for 26 
#> 
#> C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv
#>  9   "8",10,26 
#> 17   "16",13,26 
#> 21   "20",14,26 
#> 26   "25",15,26 
#> 27   "26",15,54
sif("26", dir = dirname(tf), markers = FALSE, path_pattern = ".*\\.csv$")
#> Results for 26 
#> 
#> C:/Users/STEFAN~1.FLE/AppData/Local/Temp/Rtmpm07tWz/file8458639fef6.csv
#>  9   "8",10,26 
#> 17   "16",13,26 
#> 21   "20",14,26 
#> 26   "25",15,26 
#> 27   "26",15,54
sif("twentysix", dir = dirname(tf), markers = FALSE, path_pattern = ".*\\.csv$")
#> Results for twentysix 
#> No matches found
unlink(tf)
```
