# non-libCurl methods are deprecated

    Code
      . <- httpFunction()
    Condition
      Warning:
      The `rsconnect.http` option was deprecated in rsconnect 1.0.0.
      i It should no longer be necessary to set this option
      i If the default http handler doesn't work for you, please file an issue at <https://github.com/rstudio/rsconnect/issues>

# authHeaders() picks correct method based on supplied fields

    Code
      str(authHeaders(list(secret = "123"), url, "GET"))
    Output
      List of 3
       $ Date              : chr "Thu, 09 Mar 2023 14:29:00 GMT"
       $ X-Auth-Signature  : chr "YmJiMjM1Y2E5MjFlNGFkOTMxZjQxNzU4NGQ1ZTk3MzYyYzg1YjcyMzUyMzhlYTY4Y2UxMjI1MzJkZWE1MDA3NQ==; version=1"
       $ X-Content-Checksum: chr "d41d8cd98f00b204e9800998ecf8427e"
    Code
      str(authHeaders(list(private_key = key), url, "GET"))
    Output
      List of 3
       $ Date              : chr "Thu, 09 Mar 2023 14:29:00 GMT"
       $ X-Auth-Signature  : chr "mk4e1sdK0Gy9Uex2nJMtkntdT/boQWRakSRB6iYw9hmP2zMHQjvynY+Kc5hqbGAK7tbzG52fC+5MQSOUapNKBF6GNnVe1cp2jFq4pmhEL2yhlkB"| __truncated__
       $ X-Content-Checksum: chr "1B2M2Y8AsgTpgAmY7PhCfg=="

# includes body in error if available

    Code
      handleResponse(resp_text)
    Condition
      Error:
      ! <http://example.com/error> failed with HTTP status 400
      Failed
    Code
      handleResponse(resp_json)
    Condition
      Error:
      ! <http://example.com/error> failed with HTTP status 400
      failed
    Code
      handleResponse(resp_html)
    Condition
      Error:
      ! <http://example.com/error> failed with HTTP status 400
      Failed

# but still gives got error if no body

    Code
      handleResponse(resp_text)
    Condition
      Error:
      ! <http://example.com/error> failed with HTTP status 400
    Code
      handleResponse(resp_json)
    Condition
      Error:
      ! <http://example.com/error> failed with HTTP status 400
      Unexpected json response:
    Code
      handleResponse(resp_html)
    Condition
      Error:
      ! <http://example.com/error> failed with HTTP status 400

# errors contain method

    Code
      GET(service, list(), path = "status/404")
    Condition
      Error in `GET()`:
      ! <http://127.0.0.1:{port}/status/404> failed with HTTP status 404
    Code
      POST(service, list(), path = "status/403")
    Condition
      Error in `POST()`:
      ! <http://127.0.0.1:{port}/status/403> failed with HTTP status 403

