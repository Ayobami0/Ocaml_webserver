exception Http_error of int

let error_to_http_resp ?(body) err =
  Response.bytes_of_http_response
    { http_status = err; http_headers = []; http_body = body }
