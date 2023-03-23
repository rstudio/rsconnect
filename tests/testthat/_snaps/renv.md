# generates expected packrat file

    Code
      showDcf(parseRenvDependencies(test_path("renv-cran")))
    Output
      Package: cli
      Version: 3.6.0
      Source: CRAN
      Repository: https://cran.rstudio.com
    Code
      showDcf(parseRenvDependencies(test_path("renv-github")))
    Output
      Package: withr
      Version: 2.5.0.9000
      Source: GitHub
      RemoteType: github
      RemoteHost: api.github.com
      RemoteUsername: r-lib
      RemoteRepo: withr
      RemoteRef: main
      RemoteSha: 3b01a2bef7b17bf2f82d94be58a2d4287827fa21
    Code
      showDcf(parseRenvDependencies(test_path("renv-bioc")))
    Output
      Package: BiocGenerics
      Version: 0.44.0
      Source: Bioconductor
      
      Package: BiocManager
      Version: 1.30.20
      Source: CRAN
      Repository: https://cran.rstudio.com

