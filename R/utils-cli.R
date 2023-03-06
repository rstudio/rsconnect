# https://github.com/r-lib/cli/issues/228 ---------------------------------

cli_menu <- function(header,
                     prompt,
                     choices,
                     not_interactive = choices,
                     quit = integer(),
                     .envir = caller_env()) {
  if (!is_interactive()) {
    cli::cli_abort(c(header, not_interactive), .envir = .envir)
  }
  choices <- sapply(choices, cli::format_inline, .envir = .envir, USE.NAMES = FALSE)

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
    cli::cli_abort("Quiting...", call = NULL)
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
    readline("Selection: ")
  }
}
