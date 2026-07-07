# Disable the update-check feature (version-check.R) for the whole test suite by
# default. Tests that want to exercise the feature turn it back on locally (see
# test-version-check.R) or mock checkForNewerVersion() directly (see
# test-http.R).
options(rsconnect.check_updates = FALSE)
