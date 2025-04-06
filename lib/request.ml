open Utils

type http_request = {
  http_body : string option;
  http_path : string;
  http_headers : http_header list;
  http_method : http_method;
  http_version : string;
}

let http_request_of_bytes b =
  let s = String.of_bytes b in
  let msg = Str.split (Str.regexp_string "\r\n\r\n") s in
  match msg with
  | [ request_line_header; body ] -> (
      match Str.split (Str.regexp_string "\r\n") request_line_header with
      | request_line :: xs -> (
          match Str.split (Str.regexp_string " ") request_line with
          | [ met; path; ver ] ->
              let headers =
                List.map
                  (fun x ->
                    let r = Str.split (Str.regexp_string ": ") x in
                    (List.nth r 0, List.nth r 1))
                  xs
              in
              let body = if body == "" then None else Some body in
              let met =
                match met with
                | "GET" -> GET
                | "POST" -> POST
                | _ -> failwith "invalid method"
              in
              {
                http_body = body;
                http_path = path;
                http_headers = headers;
                http_method = met;
                http_version = ver;
              }
          | _ -> failwith "invalid request_line format")
      | _ -> failwith "invalid header format")
  | _ -> failwith "invalid message"
