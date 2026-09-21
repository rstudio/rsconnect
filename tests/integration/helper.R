# Every directory under deploy-content/ is a piece of sample content.
# test-deploy.R deploys each one to a real Posit Connect server.
deploy_fixtures <- function() {
  sort(list.dirs(
    test_path("deploy-content"),
    full.names = FALSE,
    recursive = FALSE
  ))
}
