# rxode2

<details>

* Version: 2.0.13
* GitHub: https://github.com/nlmixr2/rxode2
* Source code: https://github.com/cran/rxode2
* Date/Publication: 2023-04-22 07:40:02 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::revdep_details(, "rxode2")` for more info

</details>

## In both

*   checking whether package ‘rxode2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/aron/dev/rstudio/rsconnect/revdep/checks.noindex/rxode2/new/rxode2.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rxode2’ ...
** package ‘rxode2’ successfully unpacked and MD5 sums checked
** using staged installation

R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin20 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
...
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c correction.c -o correction.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c daxpy.c -o daxpy.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c ddot.c -o ddot.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c dgefa.c -o dgefa.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c dgesl.c -o dgesl.o
gfortran -arch x86_64  -fPIC  -Wall -g -O2  -c dgpadm.f -o dgpadm.o
make: gfortran: No such file or directory
make: *** [dgpadm.o] Error 1
ERROR: compilation failed for package ‘rxode2’
* removing ‘/Users/aron/dev/rstudio/rsconnect/revdep/checks.noindex/rxode2/new/rxode2.Rcheck/rxode2’


```
### CRAN

```
* installing *source* package ‘rxode2’ ...
** package ‘rxode2’ successfully unpacked and MD5 sums checked
** using staged installation

R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin20 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
...
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c correction.c -o correction.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c daxpy.c -o daxpy.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c ddot.c -o ddot.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c dgefa.c -o dgefa.o
clang -arch x86_64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2parse/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/rxode2random/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/PreciseSums/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/Rcpp/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/RcppArmadillo/include' -I'/Users/aron/dev/rstudio/rsconnect/revdep/library.noindex/rxode2/BH/include' -I/opt/R/x86_64/include   -D_isrxode2_ -fPIC  -falign-functions=64 -Wall -g -O2  -c dgesl.c -o dgesl.o
gfortran -arch x86_64  -fPIC  -Wall -g -O2  -c dgpadm.f -o dgpadm.o
make: gfortran: No such file or directory
make: *** [dgpadm.o] Error 1
ERROR: compilation failed for package ‘rxode2’
* removing ‘/Users/aron/dev/rstudio/rsconnect/revdep/checks.noindex/rxode2/old/rxode2.Rcheck/rxode2’


```
