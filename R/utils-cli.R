# https://github.com/r-lib/cli/issues/228 ---------------------------------

simulate_user_input <- function(x, env = caller_env()) {
  withr::local_options(
    rlang_interactive = TRUE,
    cli_prompt = as.character(x),
    .local_envir = env
  )
}

cli_menu <- function(
  header,
  prompt,
  choices,
  not_interactive = choices,
  quit = integer(),
  .envir = caller_env(),
  error_call = caller_env()
) {
  if (!is_interactive()) {
    cli::cli_abort(
      c(header, not_interactive),
      .envir = .envir,
      call = error_call
    )
  }

  choices <- paste0(cli::style_bold(seq_along(choices)), ": ", choices)
  cli::cli_inform(
    c(header, prompt, choices),
    .envir = .envir
  )

  repeat {
    selected <- cli_readline("Selection: ")
    if (selected %in% c("0", seq_along(choices))) {
      break
    }
    cli::cli_inform(
      "Type a number between 1 and {length(choices)}, or type 0 to exit."
    )
  }

  selected <- as.integer(selected)
  if (selected %in% c(0, quit)) {
    if (is_testing()) {
      cli::cli_abort("Quitting...", call = NULL)
    } else {
      cli::cli_alert_danger("Quitting...")
      # simulate user pressing Ctrl + C
      invokeRestart("abort", cnd)
    }
  }
  selected
}

cli_readline <- function(prompt) {
  testing <- getOption("cli_prompt", character())

  if (length(testing) > 0) {
    selected <- testing[[1]]
    cli::cli_inform(paste0(prompt, selected))
    options(cli_prompt = testing[-1])
    selected
  } else {
    readline(prompt)
  }
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
