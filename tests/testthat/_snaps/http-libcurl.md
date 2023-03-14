# can trace JSON

    Code
      . <- GET(service, list(), "get")
    Output
      >> {
      >>   "args": {},
      >>   "headers": {
      >>     "Host": "127.0.0.1:{port}",
      >>     "User-Agent": "rsconnect/0.8.29.1",
      >>     "Accept": "*/*",
      >>     "Accept-Encoding": "deflate, gzip",
      >>     "X-Auth-Token": "anonymous-access"
      >>   },
      >>   "origin": "127.0.0.1",
      >>   "path": "/get",
      >>   "url": "http://127.0.0.1:{port}/get"
      >> }
    Code
      . <- POST_JSON(service, list(), "post", list(a = 1, b = 2))
    Output
      << {
      <<   "a": 1,
      <<   "b": 2
      << }
      >> {
      >>   "args": {},
      >>   "data": "{\n  \"a\": 1,\n  \"b\": 2\n}",
      >>   "files": {},
      >>   "form": {},
      >>   "headers": {
      >>     "Host": "127.0.0.1:{port}",
      >>     "User-Agent": "rsconnect/0.8.29.1",
      >>     "Accept": "*/*",
      >>     "Accept-Encoding": "deflate, gzip",
      >>     "X-Auth-Token": "anonymous-access",
      >>     "Content-Type": "application/json",
      >>     "Content-Length": "22"
      >>   },
      >>   "json": {
      >>     "a": 1,
      >>     "b": 2
      >>   },
      >>   "method": "post",
      >>   "path": "/post",
      >>   "origin": "127.0.0.1",
      >>   "url": "http://127.0.0.1:{port}/post"
      >> }

