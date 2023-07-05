## Release instructions

-   Create and switch to a release branch (e.g. `release/<version>`).

    ```bash
    git checkout -b release/0.8.26
    ```

-   Regenerate the `README.md`.

    ```r
    devtools::build_readme()
    ```

-   Update version in `DESCRIPTION` file, if necessary.

-   Update `NEWS.md`. Confirm recent changes are included.

    1. Update version in header, if necessary.
    2. Remove "(development version)" from the version header

-   Commit the `README.md`, `NEWS.md` and `DESCRIPTION` changes.

    ```bash
    git commit -m 'prepare for CRAN release' README.md NEWS.md DESCRIPTION
    ```

-   Check and fix URLs (from R):

    ```r
    install.packages("urlchecker")
    urlchecker::url_check()
    ```

-   Generate a release `.tar.gz` by running the following command from the
    parent directory of your `rsconnect` repository:
    
    ```console
    R CMD build rsconnect
    ```

-   Test the package (also from the parent directory):

    ```console
    R CMD check --as-cran rsconnect_*.tar.gz
    ```

-   Push the branch to GitHub and let our CI workflow run.

-   Fix any issues identified by the previous steps. Rinse and repeat.

-   Submit to CRAN. Cross fingers. https://cran.r-project.org/
    
-   After acceptance, squash-and-merge the `release/<version>` branch back to
    `main`.

-   Create a git tag for your new release and push that tag.

    ```console
    git tag -a -m 'CRAN release: vX.Y.Z' vX.Y.Z COMMIT_HASH
    git push origin vX.Y.Z
    ```

-   Create a GitHub release against that tag and include the NEWS.md items in
    its notes.

-   Create a branch to bump for development (e.g. `development/<version>`).

    Update `DESCRIPTION`, shifting to a "development" version like 0.8.29-1.
    Update `NEWS.md` with a version header for the next version like 0.8.30.
    
    ````markdown
    # rsconnect VERSION (development version)
    ````

    Squash-and-merge the PR for this branch back to `main`.
