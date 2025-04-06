(*webserver.ml*)

open Unix

let _host_ = ""
let _port_ = "5000"
let _opts_ = [ AI_FAMILY PF_INET6; AI_SOCKTYPE SOCK_STREAM ]

let getaddr host port =
  let info = getaddrinfo host port _opts_ in
  match info with
  | [] -> failwith "cannot get address. invalid parameter provided"
  | x :: _ -> x

let read_template_file path =
  let n = if (String.equal path "/") then "index" else path in
  try
    let c = open_in ("templates/" ^ n ^ ".html") in
    let html = In_channel.input_all c in
    close_in c;
    html
  with Sys_error _ -> raise (Errors.Http_error Utils.not_found)

let start_server ?host ?port () =
  let p = match port with Some a -> a | None -> _port_ in
  let h = match host with Some a -> a | None -> _host_ in
  let addrinfo = getaddr h p in
  let s = socket addrinfo.ai_family addrinfo.ai_socktype addrinfo.ai_protocol in
  bind s addrinfo.ai_addr;
  listen s 1;
  while true do
    let fd, client = accept s in
    let _ =
      match client with
      | ADDR_INET (n, v) -> (n, v)
      | ADDR_UNIX _ ->
          close fd;
          failwith "server doesn't support unix addressing"
    in

    let buf = Bytes.create 1024 in
    let _ = recv fd buf 0 1024 [] in
    let req = Request.http_request_of_bytes buf in
    let resp =
      try
        let body = read_template_file req.http_path in
        let len = string_of_int (String.length body) in
        Response.bytes_of_http_response
          {
            http_body = Some body;
            http_headers =
              [ ("Content-Type", "text/html"); ("Content-Length", len) ];
            http_status = 200;
          }
      with Errors.Http_error e ->
        Errors.error_to_http_resp ~body:"<h1>Page Not Found</h1>" e
    in
    let _ = send fd (fst resp) 0 (snd resp) [] in
    close fd
  done;
  close s
