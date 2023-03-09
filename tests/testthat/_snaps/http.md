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
       $ X-Auth-Signature  : chr "tqz4HGcSmuWKIGzIj42OEkwYZQzfJBdrUynlBQKSEEok2zMFZwsgEpEzU8PzpoeMEmcX5+Cr1IuDLLASz0ivAQ=="
       $ X-Content-Checksum: chr "1B2M2Y8AsgTpgAmY7PhCfg=="

# throws useful errors when request fails

    Code
      handleResponse(resp_text)
    Condition
      Error:
      ! HTTP 400
       http://example.com/error
      Failed
    Code
      handleResponse(resp_json)
    Condition
      Error:
      ! HTTP 400
       http://example.com/error
      failed
    Code
      handleResponse(resp_html)
    Condition
      Error:
      ! HTTP 400
       http://example.com/error
      Failed

# throws useful errors when request fails with empty body

    Code
      handleResponse(resp_text)
    Condition
      Error:
      ! HTTP 400
       http://example.com/error
    Code
      handleResponse(resp_json)
    Condition
      Error:
      ! HTTP 400
       http://example.com/error
      Unexpected json response: 
    Code
      handleResponse(resp_html)
    Condition
      Error:
      ! HTTP 400
       http://example.com/error

