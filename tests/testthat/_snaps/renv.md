# generates expected packrat file

    Code
      showPackratTranslation(test_path("renv-cran"))
    Output
      RVersion: 4.2.1
      Repos: CRAN=https://cran.rstudio.com
      
      Package: cli
      Version: 3.6.0
      Source: CRAN
      Repository: CRAN
    Code
      showPackratTranslation(test_path("renv-github"))
    Output
      RVersion: 4.2.1
      Repos: CRAN=https://cran.rstudio.com
      
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
      showPackratTranslation(test_path("renv-bioc"))
    Output
      RVersion: 4.2.1
      Repos: CRAN=https://cran.rstudio.com
      
      Package: BiocGenerics
      Version: 0.44.0
      Source: Bioconductor
      git_url: https://git.bioconductor.org/packages/BiocGenerics
      git_branch: RELEASE_3_16
      git_last_commit: d7cd9c1
      git_last_commit_date: 2022-11-01
      
      Package: BiocManager
      Version: 1.30.20
      Source: CRAN
      Repository: CRAN

