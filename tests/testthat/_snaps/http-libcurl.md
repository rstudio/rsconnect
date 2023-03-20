# can trace JSON

    Code
      . <- POST_JSON(service, list(), "", list(a = 1, b = 2))
    Output
      << {
      <<   "a": 1,
      <<   "b": 2
      << }
      >> {"a":[1],"b":[2]}

