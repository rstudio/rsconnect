# authHeaders() picks correct method based on supplied fields

    Code
      str(authHeaders(list(secret = "123"), url, "GET"))
    Output
      List of 3
       $ Date              : chr "Thu, 09 Mar 2023 14:29:00 GMT"
       $ X-Auth-Signature  : chr "YmJiMjM1Y2E5MjFlNGFkOTMxZjQxNzU4NGQ1ZTk3MzYyYzg1YjcyMzUyMzhlYTY4Y2UxMjI1MzJkZWE1MDA3NQ==; version=1"
       $ X-Content-Checksum: chr "d41d8cd98f00b204e9800998ecf8427e"

