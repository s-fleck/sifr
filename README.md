# sifr

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
 
Search and replace regex patterns accross R source files (and other files).

The search part is portable, replacing text in files currently only works on
linux machines that have **sed** installed.




## Installation

You can install sifr from GitHub with:


``` r
# install.packages("devtools")
devtools::install_github("s-fleck/sifr")
```




## Example

``` r
sifr::sif("library")
```
