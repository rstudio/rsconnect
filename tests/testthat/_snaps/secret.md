# print and str obfuscate output

    Code
      x <- secret("THIS IS MY PASSWORD: foo")
      x
    Output
      [1] "THIS I... (redacted)"
    Code
      str(x)
    Output
       THIS I... (redacted)

