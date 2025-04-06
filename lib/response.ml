open Utils

type http_response = {
  http_body : string option;
  http_headers : http_header list;
  http_status : int;
}

let bytes_of_http_response resp =
  let status_line = Printf.sprintf "HTTP/%s %d" http_version resp.http_status in
  let s_header =
    String.concat "\r\n"
      (List.map
         (fun x -> Printf.sprintf "%s: %s" (fst x) (snd x))
         resp.http_headers)
  in
  let s_body = match resp.http_body with Some v -> v | None -> "" in
  let b_resp =
    Bytes.of_string
      (Printf.sprintf "%s\r\n%s\r\n\r\n%s" status_line s_header s_body)
  in
  (b_resp, Bytes.length b_resp)
