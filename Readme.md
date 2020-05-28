# Eval in R

This project aims at analyzing the usage of `eval`in the R language.

- it analyzes `eval` in traces from execution of R packages, see *trace-eval-analysis.R*. Traces are consumed as [fst](https://www.fstpackage.org/) files. 
- it can download a list of most downloaded packages from further analysis, see *download-packages.R*
- it analyzes directly the source code of packages for `eval` usages
- it can download a list of most popular packages on [www.cranstatic.org]()

The scripts mostly generate graphs: histograms of most used `eval` calls, most used exprs in `eval`.

## Disclaimer

This is a Work-In-Progress, with some hard-coded values...

## Requirements

GNU R >= 3.6.3 (may work with earlier versions)

### Packages

- all:
    - tidyverse
- trace-eval-analysis:
    - forcats
    - fst
    - stringi
    - stringr
    - viridis
- download-packages:
    - cranlogs
- text-eval-analysis:
    - stringr
    - xfun
    - rlang
- cranstatic
    - rvest
    - selectr
    - stringr
    - fst